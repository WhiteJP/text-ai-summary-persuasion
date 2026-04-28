# =============================================================================
# 08 · Robustness Checks: Baseline Carry-Forward (three scenarios)
# =============================================================================
# Re-estimates ATEs under baseline carry-forward across three scenarios:
#   1. T1 BCF (ITT):            wave-1 post − pre, change = 0 for attriters
#   2. T2 BCF (ITT):            follow-up − pre on full ITT, change = 0 if no FU
#   3. T2 BCF (T1 completers):  follow-up − pre among T1 completers, change = 0 if no FU
#
# Reports Cohen's d (ATE + text×format interaction), BF01 (medium priors),
# and BF01 prior sensitivity grid for each scenario.

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(estimatr)
  library(broom)
  library(BayesFactor)
  library(tidyr)
})

d_itt <- readRDS(here::here("data", "d_itt.rds"))
d     <- readRDS(here::here("data", "d.rds"))

# =============================================================================
# DV specifications
# =============================================================================

dv_specs <- tribble(
  ~change_dv,             ~pre_dv,              ~label,                    ~text_treatment,
  "irs_approval_change",  "irs_approval_pre",   "IRS Favorability",        "Lewis",
  "civil_service_change", "civil_service_pre",  "Civil Service Fav.",      "Lewis",
  "doge_change",          "doge_pre_1",         "DOGE Disapproval",        "Lewis",
  "trump_change",         "trump_pre_1",        "Trump Disapproval",       "Lewis",
  "enforce_change",       "enforce_pre_1",      "IRS Funding Support",     "Lewis",
  "fav_ssa_change",       "fav_ssa_pre_1",      "SSA Agent Fav.",          "Lewis",
  "mvs_change",           "mvs_pre",            "Material Values",         "Haidt"
)

# =============================================================================
# Helpers
# =============================================================================

extract_ate <- function(change_dv, pre_dv, data, text_treatment) {
  data_model <- data %>%
    filter(!is.na(.data[[change_dv]]), !is.na(.data[[pre_dv]])) %>%
    mutate(
      text_treat  = ifelse(text == text_treatment, 1, 0),
      format_eff  = ifelse(format == "Full", -0.5, 0.5),
      pre_score_z = as.numeric(scale(.data[[pre_dv]]))
    )
  formula_str <- paste0(change_dv, " ~ text_treat * format_eff * pre_score_z")
  mod <- lm_robust(as.formula(formula_str), data = data_model)
  tidy(mod, conf.int = TRUE) %>%
    filter(term == "text_treat") %>%
    select(estimate, std.error, p.value, conf.low, conf.high)
}

extract_format_int <- function(change_dv, pre_dv, data, text_treatment) {
  data_model <- data %>%
    filter(!is.na(.data[[change_dv]]), !is.na(.data[[pre_dv]])) %>%
    mutate(
      text_treat  = ifelse(text == text_treatment, 1, 0),
      format_eff  = ifelse(format == "Full", -0.5, 0.5),
      pre_score_z = as.numeric(scale(.data[[pre_dv]]))
    )
  formula_str <- paste0(change_dv, " ~ text_treat * format_eff * pre_score_z")
  mod <- lm_robust(as.formula(formula_str), data = data_model)
  tidy(mod, conf.int = TRUE) %>%
    filter(term == "text_treat:format_eff") %>%
    slice(1) %>%
    select(estimate, p.value, conf.low, conf.high)
}

sigstars <- function(p) {
  dplyr::case_when(
    p < 0.001 ~ "***", p < 0.01 ~ "**", p < 0.05 ~ "*", p < 0.1 ~ ".", TRUE ~ ""
  )
}

fmt_d_ci <- function(d, lo, hi, p) {
  sprintf("%.2f [%.2f, %.2f]%s", d, lo, hi, sigstars(p))
}

# =============================================================================
# Baseline carry-forward dataset (T1)
# =============================================================================

d_cf <- d_itt
for (cv in dv_specs$change_dv) {
  d_cf[[cv]][!(d_cf$Finished %in% "1")] <- 0
}

# Pooled SD from completers (common denominator for T1 d)
pooled_sds <- pmap_dfr(dv_specs, function(change_dv, pre_dv, label, text_treatment) {
  tibble::tibble(DV = label, pooled_sd = sd(d[[pre_dv]], na.rm = TRUE))
})

# Orientation from completers ATE sign (positive = persuaded as intended)
main_ates <- pmap_dfr(dv_specs, function(change_dv, pre_dv, label, text_treatment) {
  extract_ate(change_dv, pre_dv, d, text_treatment) %>%
    mutate(DV = label, .before = 1)
})
sign_map <- main_ates %>% transmute(DV, orient = ifelse(estimate >= 0, 1, -1))

# =============================================================================
# Three-scenario BCF: shared functions
# =============================================================================

compute_bf_with_pre_bcf3 <- function(outcome_col, pre_col, text_treatment, data,
                                     rscale = "medium", rscale_cont = "medium",
                                     seed = 88101L) {
  set.seed(seed)

  data_model <- data |>
    mutate(
      text_treat = factor(ifelse(text == text_treatment, 1, 0)),
      format_eff = factor(format)
    ) |>
    select(
      outcome = all_of(outcome_col),
      pre_raw = all_of(pre_col),
      text_treat,
      format_eff
    ) |>
    drop_na() |>
    mutate(pre_score_z = as.numeric(scale(pre_raw))) |>
    select(outcome, pre_score_z, text_treat, format_eff)

  df <- as.data.frame(data_model)

  bf_full <- BayesFactor::lmBF(
    outcome ~ pre_score_z + text_treat + format_eff + text_treat:format_eff,
    data = df,
    rscaleFixed = rscale,
    rscaleCont = rscale_cont,
    iterations = 50000L
  )

  bf_additive <- BayesFactor::lmBF(
    outcome ~ pre_score_z + text_treat + format_eff,
    data = df,
    rscaleFixed = rscale,
    rscaleCont = rscale_cont,
    iterations = 50000L
  )

  bf_01 <- bf_additive / bf_full
  BayesFactor::extractBF(bf_01)$bf
}

rscale_grid_bcf3 <- c("medium" = 0.5, "wide" = sqrt(2) / 2, "ultrawide" = 1)

apply_orient_bcf3 <- function(df, orient_tbl) {
  df |>
    left_join(orient_tbl, by = "DV") |>
    mutate(
      d = d * orient,
      d_lo_raw = d_ci_low * orient,
      d_hi_raw = d_ci_high * orient,
      d_ci_low = pmin(d_lo_raw, d_hi_raw),
      d_ci_high = pmax(d_lo_raw, d_hi_raw)
    ) |>
    select(-orient, -d_lo_raw, -d_hi_raw)
}

bcf3_freq_table <- function(
    scenario_label,
    data,
    spec_df,
    pooled_sds_df,
    orient_tbl,
    ref_change,
    ref_pre
) {
  ate_raw <- pmap_dfr(spec_df, function(change_dv, pre_dv, label, text_treatment) {
    extract_ate(change_dv, pre_dv, data, text_treatment) |>
      mutate(DV = label, .before = 1)
  })
  int_raw <- pmap_dfr(spec_df, function(change_dv, pre_dv, label, text_treatment) {
    extract_format_int(change_dv, pre_dv, data, text_treatment) |>
      mutate(DV = label, .before = 1)
  })
  ate_d <- ate_raw |>
    left_join(pooled_sds_df, by = "DV") |>
    mutate(
      d = estimate / pooled_sd,
      d_ci_low = conf.low / pooled_sd,
      d_ci_high = conf.high / pooled_sd
    )
  ate_h <- apply_orient_bcf3(ate_d, orient_tbl)
  int_d <- int_raw |>
    left_join(pooled_sds_df, by = "DV") |>
    mutate(
      d = estimate / pooled_sd,
      d_ci_low = conf.low / pooled_sd,
      d_ci_high = conf.high / pooled_sd
    )
  int_h <- apply_orient_bcf3(int_d, orient_tbl)
  n_reg <- sum(!is.na(data[[ref_change]]) & !is.na(data[[ref_pre]]))
  tibble::tibble(
    scenario = scenario_label,
    DV = ate_h$DV,
    ATE_d = fmt_d_ci(ate_h$d, ate_h$d_ci_low, ate_h$d_ci_high, ate_h$p.value),
    TextFormat_d = fmt_d_ci(int_h$d, int_h$d_ci_low, int_h$d_ci_high, int_h$p.value),
    n = n_reg
  )
}

bcf3_bf_primary <- function(data, spec_df, scenario_label) {
  pmap_dfr(spec_df, function(change_dv, pre_dv, label, text_treatment) {
    tibble::tibble(
      scenario = scenario_label,
      DV = label,
      bf_01 = compute_bf_with_pre_bcf3(change_dv, pre_dv, text_treatment, data)
    )
  })
}

bcf3_bf_sensitivity <- function(data, spec_df, scenario_label, seed_base) {
  purrr::imap_dfr(rscale_grid_bcf3, function(rs, scale_name) {
    pmap_dfr(spec_df, function(change_dv, pre_dv, label, text_treatment) {
      tibble::tibble(
        scenario = scenario_label,
        DV = label,
        prior = scale_name,
        bf_01 = compute_bf_with_pre_bcf3(
          change_dv, pre_dv, text_treatment, data,
          rscale = rs, rscale_cont = scale_name
        )
      )
    })
  })
}

bcf3_dv_pub <- function(x) {
  dplyr::mutate(
    x,
    DV = dplyr::recode(
      DV,
      `Civil Service Fav.` = "Civil Service Favorability",
      `SSA Agent Fav.` = "SSA Agent Favorability"
    )
  )
}

# =============================================================================
# Run three-scenario BCF analysis
# =============================================================================

cat("\n=== Three-scenario BCF robustness (Cohen's d + BF01 + prior sensitivity) ===\n")
cat("Baseline-carry-forward (BCF) imputes change = 0 for anyone who did not complete\n")
cat("the relevant wave, providing an ITT-style bound on attrition bias.\n")
cat("Three scenarios: (1) T1 BCF on full ITT, (2) T2 BCF on full ITT, (3) T2 BCF\n")
cat("on T1 completers only. Each reports Cohen's d for the text ATE, BF01 for format\n")
cat("equivalence (additive vs full model on change scores controlling for pre),\n")
cat("and a prior-sensitivity grid (medium / wide / ultrawide JZS priors).\n")

# --- Scenario 1: wave-1 post − pre, BCF on ITT ---
freq_rows <- list(
  bcf3_freq_table(
    "T1 BCF (ITT)",
    d_cf,
    dv_specs,
    pooled_sds,
    sign_map,
    dv_specs$change_dv[[1]],
    dv_specs$pre_dv[[1]]
  )
)
bf_prim_rows <- list(bcf3_bf_primary(d_cf, dv_specs, "T1 BCF (ITT)"))
bf_sens_rows <- list(bcf3_bf_sensitivity(d_cf, dv_specs, "T1 BCF (ITT)", 88201L))

# --- Follow-up specs & reference SD / orientation (shared T2) ---
fu_w_path <- here::here("data", "d_with_followup.rds")
itt_fu_path <- here::here("data", "d_itt_with_followup.rds")
fu_specs_bcf3 <- NULL
pooled_sds_t2 <- NULL
orient_t2 <- NULL
d_fu_comp_ref <- NULL
d_wfu_bcf3 <- NULL

if (file.exists(fu_w_path) || file.exists(itt_fu_path)) {
  source(here::here("scripts", "functions", "forest_measure_specs.R"))
  spec_forest_bcf3 <- forest_measure_specs()
  fu_specs_bcf3 <- spec_forest_bcf3 |>
    dplyr::transmute(
      change_dv = .data$change_dv_t2,
      pre_dv = .data$pre_dv,
      text_treatment = .data$text_treatment,
      label = .data$label
    )
}

if (!is.null(fu_specs_bcf3) && file.exists(fu_w_path)) {
  d_wfu_bcf3 <- readRDS(fu_w_path) |>
    dplyr::arrange(dplyr::desc(!is.na(.data$ResponseId_fu))) |>
    dplyr::distinct(PROLIFIC_PID, .keep_all = TRUE)
  d_fu_comp_ref <- d_wfu_bcf3 |> filter(!is.na(.data$ResponseId_fu), .data$fu_completed_outcomes == 1L)
  pooled_sds_t2 <- pmap_dfr(fu_specs_bcf3, function(change_dv, pre_dv, label, text_treatment) {
    tibble::tibble(DV = label, pooled_sd = stats::sd(d[[pre_dv]], na.rm = TRUE))
  })
  main_fu_ates_ref <- pmap_dfr(fu_specs_bcf3, function(change_dv, pre_dv, label, text_treatment) {
    extract_ate(change_dv, pre_dv, d_fu_comp_ref, text_treatment) |>
      mutate(DV = label, .before = 1)
  })
  orient_t2 <- main_fu_ates_ref |>
    transmute(DV, orient = ifelse(estimate >= 0, 1, -1))
}

# --- Scenario 2: T2 BCF from wave-1 ITT ---
if (!is.null(fu_specs_bcf3) && file.exists(itt_fu_path) && !is.null(orient_t2)) {
  d_itt_wfu <- readRDS(itt_fu_path) |>
    dplyr::arrange(dplyr::desc(.data$Finished == "1"), dplyr::desc(!is.na(.data$ResponseId_fu))) |>
    dplyr::distinct(PROLIFIC_PID, .keep_all = TRUE)
  d_bcf_t2_itt <- d_itt_wfu
  no_t2_itt <- is.na(d_bcf_t2_itt$ResponseId_fu) |
    !(d_bcf_t2_itt$Finished %in% "1") |
    !(d_bcf_t2_itt$fu_completed_outcomes %in% 1L)
  for (cv in fu_specs_bcf3$change_dv) {
    d_bcf_t2_itt[[cv]][no_t2_itt] <- 0
  }
  freq_rows[[length(freq_rows) + 1L]] <- bcf3_freq_table(
    "T2 BCF (ITT)",
    d_bcf_t2_itt,
    fu_specs_bcf3,
    pooled_sds_t2,
    orient_t2,
    fu_specs_bcf3$change_dv[[1]],
    fu_specs_bcf3$pre_dv[[1]]
  )
  bf_prim_rows[[length(bf_prim_rows) + 1L]] <- bcf3_bf_primary(
    d_bcf_t2_itt, fu_specs_bcf3, "T2 BCF (ITT)"
  )
  bf_sens_rows[[length(bf_sens_rows) + 1L]] <- bcf3_bf_sensitivity(
    d_bcf_t2_itt, fu_specs_bcf3, "T2 BCF (ITT)", 88401L
  )
} else if (!is.null(fu_specs_bcf3)) {
  cat(
    "Skipped T2 BCF (ITT): need d_itt_with_followup.rds and d_with_followup.rds ",
    "(for T2 SD / orientation reference).\n",
    sep = ""
  )
}

# --- Scenario 3: T2 BCF from T1 completers only ---
if (!is.null(fu_specs_bcf3) && !is.null(d_wfu_bcf3) && !is.null(orient_t2)) {
  d_bcf_t2_t1c <- d_wfu_bcf3
  no_fu_t1c <- is.na(d_bcf_t2_t1c$ResponseId_fu) | !(d_bcf_t2_t1c$fu_completed_outcomes %in% 1L)
  for (cv in fu_specs_bcf3$change_dv) {
    d_bcf_t2_t1c[[cv]][no_fu_t1c] <- 0
  }
  freq_rows[[length(freq_rows) + 1L]] <- bcf3_freq_table(
    "T2 BCF (T1 completers)",
    d_bcf_t2_t1c,
    fu_specs_bcf3,
    pooled_sds_t2,
    orient_t2,
    fu_specs_bcf3$change_dv[[1]],
    fu_specs_bcf3$pre_dv[[1]]
  )
  bf_prim_rows[[length(bf_prim_rows) + 1L]] <- bcf3_bf_primary(
    d_bcf_t2_t1c, fu_specs_bcf3, "T2 BCF (T1 completers)"
  )
  bf_sens_rows[[length(bf_sens_rows) + 1L]] <- bcf3_bf_sensitivity(
    d_bcf_t2_t1c, fu_specs_bcf3, "T2 BCF (T1 completers)", 88601L
  )
}

# =============================================================================
# Combine and print results
# =============================================================================

bcf3_freq_all <- bcf3_dv_pub(dplyr::bind_rows(freq_rows))
bcf3_bf_prim_all <- bcf3_dv_pub(dplyr::bind_rows(bf_prim_rows))
bcf3_bf_sens_wide <- dplyr::bind_rows(bf_sens_rows) |>
  mutate(bf_fmt = sprintf("%.3f", bf_01)) |>
  select(scenario, DV, prior, bf_fmt) |>
  tidyr::pivot_wider(
    names_from = prior,
    values_from = bf_fmt,
    names_prefix = "BF01_"
  ) |>
  bcf3_dv_pub()

cat("\n--- Frequentist (harmonized Cohen's d [95% CI]) ---\n\n")
print(as.data.frame(bcf3_freq_all), row.names = FALSE)

cat("\n--- BF01 primary (rscaleFixed = rscaleCont = medium) ---\n\n")
print(
  as.data.frame(bcf3_bf_prim_all |> mutate(bf_01 = round(bf_01, 3))),
  row.names = FALSE
)

cat("\n--- BF01 prior sensitivity (same rscale name for fixed + continuous) ---\n\n")
print(as.data.frame(bcf3_bf_sens_wide), row.names = FALSE)


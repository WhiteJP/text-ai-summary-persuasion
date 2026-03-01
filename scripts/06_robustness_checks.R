# =============================================================================
# 06 · Robustness Checks: Attrition Sensitivity
# =============================================================================
# Re-estimates ATEs under two increasingly pessimistic assumptions:
#   1. Carry Forward: attriters' change set to 0 (no treatment effect)
#   2. Worst Observed: Lee-style trimming within format strata
# Produces the direction-harmonized d-first table reported in the paper.

library(dplyr)
library(purrr)
library(estimatr)
library(broom)

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
# Helper: lightweight ATE extraction (same model as main analysis)
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

sigstars <- function(p) {
  dplyr::case_when(
    p < 0.001 ~ "***", p < 0.01 ~ "**", p < 0.05 ~ "*", p < 0.1 ~ ".", TRUE ~ ""
  )
}

# =============================================================================
# Helper: Lee-style trimming within format strata
# =============================================================================
# For each format stratum, trim observed completers from the text arm with
# the higher response rate until rates are equalized. Lower bound removes
# observations most favorable to the treatment effect; upper bound removes
# least favorable.

trim_lee_within_format <- function(d_itt, change_var, text_treatment,
                                   bound = c("lower", "upper")) {
  bound <- match.arg(bound)
  d_work <- d_itt |> mutate(.lee_row_id = row_number())
  format_levels <- unique(d_work$format)
  drop_ids <- integer(0)

  for (fmt in format_levels) {
    d_fmt <- d_work |> filter(format == fmt)
    if (nrow(d_fmt) == 0) next

    treat_flag <- d_fmt$text == text_treatment
    n_t_itt <- sum(treat_flag, na.rm = TRUE)
    n_c_itt <- sum(!treat_flag, na.rm = TRUE)
    if (n_t_itt == 0 || n_c_itt == 0) next

    observed_flag <- d_fmt$Finished %in% "1" & !is.na(d_fmt[[change_var]])
    n_t_obs <- sum(treat_flag & observed_flag, na.rm = TRUE)
    n_c_obs <- sum(!treat_flag & observed_flag, na.rm = TRUE)

    p_t <- n_t_obs / n_t_itt
    p_c <- n_c_obs / n_c_itt
    if (is.na(p_t) || is.na(p_c) || abs(p_t - p_c) < 1e-12) next

    high_is_treat <- p_t > p_c
    p_low <- min(p_t, p_c)
    n_high_itt <- ifelse(high_is_treat, n_t_itt, n_c_itt)
    n_high_obs <- ifelse(high_is_treat, n_t_obs, n_c_obs)

    target_high_obs <- floor(p_low * n_high_itt)
    n_trim <- max(0, n_high_obs - target_high_obs)
    if (n_trim == 0) next

    d_high_obs <- d_fmt |>
      filter(observed_flag, (text == text_treatment) == high_is_treat) |>
      arrange(.data[[change_var]])

    if (nrow(d_high_obs) == 0) next
    n_trim <- min(n_trim, nrow(d_high_obs))

    trim_upper_tail <- (bound == "lower" && high_is_treat) ||
                       (bound == "upper" && !high_is_treat)
    ids_to_trim <- if (trim_upper_tail) {
      tail(d_high_obs$.lee_row_id, n_trim)
    } else {
      head(d_high_obs$.lee_row_id, n_trim)
    }

    drop_ids <- c(drop_ids, ids_to_trim)
  }

  d_work |>
    filter(!(Finished %in% "1") | !(.lee_row_id %in% drop_ids)) |>
    select(-.lee_row_id)
}

# =============================================================================
# Run robustness checks
# =============================================================================

n_attriters <- sum(!(d_itt$Finished %in% "1"))
cat(sprintf(
  "\n=== Attrition Robustness Checks ===\n\nITT: %d | Completers: %d | Attriters: %d (%.1f%%)\n\n",
  nrow(d_itt), nrow(d), n_attriters, 100 * n_attriters / nrow(d_itt)
))

# --- Main estimates (completers) ---
main_ates <- pmap_dfr(dv_specs, function(change_dv, pre_dv, label, text_treatment) {
  extract_ate(change_dv, pre_dv, d, text_treatment) %>%
    mutate(DV = label, method = "Completers", .before = 1)
})

# --- Carry Forward (change = 0 for attriters) ---
d_cf <- d_itt
for (cv in dv_specs$change_dv) {
  d_cf[[cv]][!(d_cf$Finished %in% "1")] <- 0
}

cf_ates <- pmap_dfr(dv_specs, function(change_dv, pre_dv, label, text_treatment) {
  extract_ate(change_dv, pre_dv, d_cf, text_treatment) %>%
    mutate(DV = label, method = "Carry Forward", .before = 1)
})

# --- Lee-Style Trimming Within Format ---
lee_lower_ates <- pmap_dfr(dv_specs, function(change_dv, pre_dv, label, text_treatment) {
  d_trim <- trim_lee_within_format(d_itt, change_dv, text_treatment, bound = "lower")
  extract_ate(change_dv, pre_dv, d_trim, text_treatment) %>%
    mutate(DV = label, method = "Lee Lower", .before = 1)
})

lee_upper_ates <- pmap_dfr(dv_specs, function(change_dv, pre_dv, label, text_treatment) {
  d_trim <- trim_lee_within_format(d_itt, change_dv, text_treatment, bound = "upper")
  extract_ate(change_dv, pre_dv, d_trim, text_treatment) %>%
    mutate(DV = label, method = "Lee Upper", .before = 1)
})

# =============================================================================
# Direction-harmonized, d-first pessimistic summary table
# =============================================================================

# Pooled SD from completers (common denominator)
pooled_sds <- pmap_dfr(dv_specs, function(change_dv, pre_dv, label, text_treatment) {
  tibble::tibble(DV = label, pooled_sd = sd(d[[pre_dv]], na.rm = TRUE))
})

add_d <- function(df) {
  df %>%
    left_join(pooled_sds, by = "DV") %>%
    mutate(
      d        = estimate / pooled_sd,
      d_ci_low = conf.low / pooled_sd,
      d_ci_high = conf.high / pooled_sd
    )
}

main_d    <- add_d(main_ates)
cf_d      <- add_d(cf_ates)
lee_lo_d  <- add_d(lee_lower_ates)
lee_up_d  <- add_d(lee_upper_ates)

# Pick pessimistic Lee bound (closest to zero given main effect sign)
lee_pess_d <- main_d %>%
  transmute(DV, pos_main = estimate >= 0) %>%
  left_join(
    lee_lo_d %>% transmute(DV, l_d = d, l_lo = d_ci_low, l_hi = d_ci_high, l_p = p.value),
    by = "DV"
  ) %>%
  left_join(
    lee_up_d %>% transmute(DV, u_d = d, u_lo = d_ci_low, u_hi = d_ci_high, u_p = p.value),
    by = "DV"
  ) %>%
  transmute(
    DV,
    d         = ifelse(pos_main, l_d,  u_d),
    d_ci_low  = ifelse(pos_main, l_lo, u_lo),
    d_ci_high = ifelse(pos_main, l_hi, u_hi),
    p.value   = ifelse(pos_main, l_p,  u_p)
  )

# Harmonize direction: positive = persuaded as intended
sign_map <- main_d %>% transmute(DV, orient = ifelse(estimate >= 0, 1, -1))

harmonize <- function(df) {
  df %>%
    left_join(sign_map, by = "DV") %>%
    mutate(
      d         = d * orient,
      d_lo_raw  = d_ci_low * orient,
      d_hi_raw  = d_ci_high * orient,
      d_ci_low  = pmin(d_lo_raw, d_hi_raw),
      d_ci_high = pmax(d_lo_raw, d_hi_raw)
    ) %>%
    select(-orient, -d_lo_raw, -d_hi_raw)
}

main_h <- harmonize(main_d)
cf_h   <- harmonize(cf_d)
lee_h  <- harmonize(lee_pess_d)

fmt_d_ci <- function(d, lo, hi, p) {
  sprintf("%.2f [%.2f, %.2f]%s", d, lo, hi, sigstars(p))
}

cat("ATE Sensitivity to Attrition (Cohen's d [95% CI])\n")
cat("Positive = attitude change in predicted direction of persuasion\n\n")

robustness_table <- main_h %>%
  transmute(DV, Completers = fmt_d_ci(d, d_ci_low, d_ci_high, p.value)) %>%
  left_join(
    cf_h %>% transmute(DV, `Carry Fwd` = fmt_d_ci(d, d_ci_low, d_ci_high, p.value)),
    by = "DV"
  ) %>%
  left_join(
    lee_h %>% transmute(DV, `Worst Obs.` = fmt_d_ci(d, d_ci_low, d_ci_high, p.value)),
    by = "DV"
  )

print(as.data.frame(robustness_table), row.names = FALSE)

cat("\nNote: Carry Fwd sets attriters' change = 0 (post = pre).\n")
cat("Worst Obs. = Lee-style pessimistic bound (closest to zero).\n")
cat(". p < .10, * p < .05, ** p < .01, *** p < .001\n")

# =============================================================================
# 06 · T2 analyses (preregistered follow-up)
# =============================================================================
# Requires: data/d_with_followup.rds and data/d_itt_with_followup.rds from
# scripts/01b_link_followup_data.R (after 01_wrangle_data.R). Run after
# scripts/05_t1_analyses.R. Forest inputs for Figure 1 → forest_t2.rds.
#
# 01b left-joins follow-up Qualtrics exports to wave-1 d.rds and d_itt.rds only
# (not d_all), writing d_with_followup.rds and d_itt_with_followup.rds.
# Analytic sample here: wave-1 completers with non-missing ResponseId_fu and
# all outcome measures completed (fu_completed_outcomes == 1),
# one row per PROLIFIC_PID (duplicates dropped with a warning if present).
#
# Implements:
#   (1) ATE at T2: change_score = t2 - t0 ~ text_treat * format_eff * pre_score_z
#       (same coding as main experiment); HC2 robust SEs.
#   (2) Binary above-midpoint LPM at T2 (rescaled persuasion-direction outcome
#       y_above ~ text_treat * format_eff * pre_score_z; full sample).
#   (3) BayesFactor equivalence (additive vs full) at T2, with prior-sensitivity
#       grid (medium / wide / ultrawide JZS priors).
#   (4) Forest-plot inputs for Figure 1 (T2 series) → output/intermediate/forest_t2.rds.
#   (5) Moderated mediation in lavaan: T1 = wave-1 post, T2 = fu. Persistence =
#       T1→T2 (pers_*); direct text→T2 (dir_*); indirect a×b (ind_*). Moderation
#       by format: pers_diff, dir_diff, ind_diff (Full−Summary). Averages:
#       pers_mean, dir_mean, ind_mean. SEs: bootstrap or analytic; med_use_bootstrap.
#   (6) T1 reanalysis with follow-up sample: same main lm as 05 (no Bayes
#       factors), wave-1 change scores restricted to follow-up completers.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(tibble)
  library(estimatr)
  library(broom)
  library(BayesFactor)
  library(lavaan)
  library(here)
})

source(here::here("scripts", "functions", "forest_plot_estimates.R"))
source(here::here("scripts", "functions", "forest_measure_specs.R"))
d_ref <- readRDS(here::here("data", "d.rds"))

# --------------------------------------------------------------------------- #
# Data: wave-1 completers who completed follow-up
# --------------------------------------------------------------------------- #

d_fu_raw <- readRDS(here("data", "d_with_followup.rds")) |>
  filter(!is.na(.data$ResponseId_fu), .data$fu_completed_outcomes == 1L)

n_before_distinct <- nrow(d_fu_raw)
dup_n <- n_before_distinct - dplyr::n_distinct(d_fu_raw$PROLIFIC_PID)
if (dup_n > 0L) {
  warning(
    "Dropping ", dup_n, " duplicate row(s) for the same PROLIFIC_PID after ",
    "follow-up filter (keeping first row per PID).",
    call. = FALSE
  )
}
d_fu <- d_fu_raw |>
  distinct(PROLIFIC_PID, .keep_all = TRUE)

cat("\n=== Follow-up analytic sample ===\n")
cat("N (wave-1 completers with follow-up): ", nrow(d_fu), "\n\n")

# --------------------------------------------------------------------------- #
# Helpers (mirror T1; Cohen's d for figure uses SD(pre) from d.rds via d_ref)
# --------------------------------------------------------------------------- #

sigstars <- function(p) {
  dplyr::case_when(
    p < 0.001 ~ "***",
    p < 0.01 ~ "**",
    p < 0.05 ~ "*",
    p < 0.1 ~ ".",
    TRUE ~ ""
  )
}

# text_treat: 1 = focal book, 0 = other (Haidt=1, Lewis=0 when text_treatment = "Haidt").
# ref_data: dataset for SD(pre) denominator in Cohen's d (wave-1 completers for consistency).
persuasion_analysis_t2 <- function(change_dv, pre_dv, data, label, text_treatment = "Lewis",
                                   ref_data = NULL) {
  data_model <- data |>
    mutate(
      text_treat = ifelse(text == text_treatment, 1L, 0L),
      format_eff = ifelse(format == "Full", -0.5, 0.5),
      pre_score_z = as.numeric(scale(.data[[pre_dv]]))
    ) |>
    filter(!is.na(.data[[change_dv]]), !is.na(.data[[pre_dv]]))

  formula_str <- paste0(change_dv, " ~ text_treat * format_eff * pre_score_z")
  mod <- lm_robust(as.formula(formula_str), data = data_model)
  lm_mod <- lm(as.formula(formula_str), data = data_model)

  coefs <- broom::tidy(mod, conf.int = TRUE)
  text_coef <- coefs |> dplyr::filter(term == "text_treat")
  interaction_coef <- coefs |> dplyr::filter(term == "text_treat:format_eff")

  sd_source <- if (!is.null(ref_data)) ref_data else data_model
  pooled_sd <- stats::sd(sd_source[[pre_dv]], na.rm = TRUE)
  std_ate <- if (nrow(text_coef) == 1L) text_coef$estimate / pooled_sd else NA_real_

  list(
    model = mod,
    lm_model = lm_mod,
    coefs = coefs,
    text_coef = text_coef,
    interaction_coef = interaction_coef,
    pooled_sd = pooled_sd,
    std_ate = std_ate,
    label = label
  )
}

compute_bf_with_pre <- function(outcome_col, pre_col, text_treatment, data,
                                rscale = "medium", rscale_cont = "medium",
                                seed = 270387L) {
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

rscale_grid <- c("medium" = 0.5, "wide" = sqrt(2) / 2, "ultrawide" = 1)

# --------------------------------------------------------------------------- #
# (1) ATE at T2 — preregistered DVs
# --------------------------------------------------------------------------- #

spec_forest <- forest_measure_specs()
fu_specs <- spec_forest |>
  dplyr::transmute(
    change_dv = .data$change_dv_t2,
    pre_dv = .data$pre_dv,
    text_treatment = .data$text_treatment,
    label = .data$label,
    ate_flip = .data$ate_flip
  )

t2_fit <- pmap(
  fu_specs,
  function(change_dv, pre_dv, text_treatment, label, ate_flip) {
    persuasion_analysis_t2(change_dv, pre_dv, d_fu, label, text_treatment, ref_data = d_ref)
  }
)

cat("=== Average Treatment Effects (ATE) at T2 ===\n\n")
cat("DV labels and signs match forest_measure_specs (ate_flip); Cohen's d = flip × b / SD(pre).\n")
cat("Change = follow-up − pre; HC2 robust SEs. Int = text × format interaction.\n\n")

ate_tbl <- imap_dfr(t2_fit, function(a, i) {
  idx <- as.integer(i)
  row <- fu_specs[idx, ]
  lab <- row$label
  ate_f <- row$ate_flip
  int_f <- ate_f * -1L
  tc <- a$text_coef
  ic <- a$interaction_coef
  t_ate <- flip_robust_coef(tc, ate_f)
  t_int <- flip_robust_coef(ic, int_f)
  ate_d <- ate_f * a$std_ate
  int_d <- if (nrow(ic) == 1L) int_f * ic$estimate / a$pooled_sd else NA_real_
  if (nrow(tc) != 1L || nrow(ic) != 1L) {
    warning("Unexpected coef rows for ", lab, call. = FALSE)
  }
  sd_pre <- a$pooled_sd
  tibble::tibble(
    DV        = lab,
    `ATE (b)` = if (nrow(tc) == 1L) sprintf("%.3f%s", t_ate$estimate, sigstars(tc$p.value)) else NA_character_,
    `ATE d`   = if (nrow(tc) == 1L) sprintf("%.2f [%.2f, %.2f]", ate_d, t_ate$conf.low / sd_pre, t_ate$conf.high / sd_pre) else NA_character_,
    `ATE p`   = if (nrow(tc) == 1L) round(tc$p.value, 5) else NA_real_,
    `Int (b)` = if (nrow(ic) == 1L) sprintf("%.3f%s", t_int$estimate, sigstars(ic$p.value)) else NA_character_,
    `Int d`   = if (nrow(ic) == 1L) sprintf("%.2f [%.2f, %.2f]", int_d, t_int$conf.low / sd_pre, t_int$conf.high / sd_pre) else NA_character_,
    `Int p`   = if (nrow(ic) == 1L) round(ic$p.value, 5) else NA_real_
  )
})

print(as.data.frame(ate_tbl), row.names = FALSE)

# --------------------------------------------------------------------------- #
# Binary above-midpoint LPM at T2 (full sample, persuasion-oriented rescaled)
# --------------------------------------------------------------------------- #

rescale_persuasion <- function(x, reverse, scale_hi) {
  if (reverse) scale_hi + 1 - x else x
}

binary_above_cut_lpm <- function(data, pre_col, post_col, reverse, scale_hi,
                                 text_treatment, label, cut = 4) {
  d_an <- data |>
    transmute(
      text, format,
      pre_rs = rescale_persuasion(.data[[pre_col]], reverse, scale_hi),
      post_rs = rescale_persuasion(.data[[post_col]], reverse, scale_hi)
    ) |>
    filter(!is.na(pre_rs), !is.na(post_rs)) |>
    mutate(
      y_above     = as.integer(post_rs >= cut),
      y_above_pre = as.integer(pre_rs >= cut),
      text_treat  = ifelse(text == text_treatment, 1L, 0L),
      format_eff  = ifelse(format == "Full", -0.5, 0.5),
      pre_score_z = as.numeric(scale(pre_rs))
    )

  pre_above_rate <- mean(d_an$y_above_pre, na.rm = TRUE)
  pre_below_rate <- 1 - pre_above_rate

  mod <- lm_robust(y_above ~ text_treat * format_eff * pre_score_z, data = d_an)
  tc <- tidy(mod, conf.int = TRUE) |> dplyr::filter(term == "text_treat")
  lpm_est <- tc$estimate[[1L]]

  tibble::tibble(
    DV = label,
    n = nrow(d_an),
    pre_pct_below = sprintf("%.1f", 100 * pre_below_rate),
    lpm_pct_persuaded = lpm_est / pre_below_rate,
    lpm_ate_text = sprintf("%.4f%s", lpm_est, sigstars(tc$p.value)),
    ci_95 = sprintf("[%.4f, %.4f]", tc$conf.low, tc$conf.high),
    p = round(tc$p.value, 5)
  )
}

binary_es_specs_t2 <- tribble(
  ~label, ~pre_col, ~post_col, ~reverse, ~scale_hi, ~text_treatment, ~cut,
  "IRS Favorability", "irs_approval_pre", "irs_approval_t2", FALSE, 7L, "Lewis", 4,
  "IRS Funding Support", "enforce_pre_1", "enforce_post_1_fu", FALSE, 7L, "Lewis", 4,
  "Civil Service Favorability", "civil_service_pre", "civil_service_t2", FALSE, 7L, "Lewis", 4,
  "SSA Agent Favorability", "fav_ssa_pre_1", "fav_ssa_post_1_fu", FALSE, 7L, "Lewis", 4,
  "DOGE Disapproval", "doge_pre_1", "doge_post_1_fu", TRUE, 7L, "Lewis", 4,
  "Trump Disapproval", "trump_pre_1", "trump_post_1_fu", TRUE, 7L, "Lewis", 4,
  "Reduced Material Values", "mvs_pre", "mvs_t2", TRUE, 5L, "Haidt", 3
)

cat("\n=== Binary above-midpoint LPM at T2 (rescaled FU post >= cut; full sample) ===\n")
cat("Cut = scale midpoint (4 for 1–7 scales, 3 for 1–5 MVS).\n")
cat("LPM: y_above ~ text_treat * format_eff * pre_score_z.\n")
cat("text_treat = 1 for the focal book in each row.\n")
cat("pre_pct_below = % below cut at pre (pooled) = room to move.\n")
cat("lpm_pct_persuaded ≈ LPM est / pre_below_rate (~fraction of persuadable people persuaded;\n")
cat("  assumes control stays at pre baseline; can exceed 100% if control moves opposite direction).\n\n")

binary_tbl_t2 <- pmap_dfr(
  binary_es_specs_t2,
  function(label, pre_col, post_col, reverse, scale_hi, text_treatment, cut) {
    binary_above_cut_lpm(d_fu, pre_col, post_col, reverse, scale_hi, text_treatment, label, cut)
  }
) |> arrange(DV)

print(as.data.frame(binary_tbl_t2), row.names = FALSE)

# --------------------------------------------------------------------------- #
# Bayes factors: format equivalence controlling for pre (preregistered priors)
# --------------------------------------------------------------------------- #

cat("\n\n=== Bayes Factors for Format Equivalence at T2 (BF01) ===\n")
cat("Tests whether the text treatment effect on follow-up outcomes differs by format.\n")
cat("Two nested models on change scores (follow-up − pre), both controlling for z-scored pre:\n")
cat("  Full:     change ~ pre_z + text + format + text:format\n")
cat("  Additive: change ~ pre_z + text + format\n")
cat("BF01 = marginal likelihood(additive) / marginal likelihood(full).\n")
cat("BF01 > 1 favors H0 (no text × format interaction, i.e. format equivalence).\n")
cat("BF01 > 3 is conventionally 'moderate' evidence for equivalence.\n")
cat("JZS priors: rscaleFixed = rscaleCont = medium (sensitivity analysis varies these).\n\n")

bf_t2_primary <- pmap_dfr(
  fu_specs,
  function(change_dv, pre_dv, text_treatment, label, ate_flip) {
    tibble::tibble(
      label = label,
      bf_01 = compute_bf_with_pre(change_dv, pre_dv, text_treatment, d_fu)
    )
  }
)
print(as.data.frame(bf_t2_primary |> mutate(bf_01 = round(bf_01, 3))), row.names = FALSE)

cat("\n=== BF01 sensitivity (rscaleFixed / rscaleCont grids, pre-controlled) ===\n\n")

bf_sens_rows <- list()
for (scale_name in names(rscale_grid)) {
  rs <- rscale_grid[[scale_name]]
  row <- pmap_dfr(
    fu_specs,
    function(change_dv, pre_dv, text_treatment, label, ate_flip) {
      tibble::tibble(
        DV = label,
        prior = scale_name,
        bf_01 = compute_bf_with_pre(
          change_dv, pre_dv, text_treatment, d_fu,
          rscale = rs, rscale_cont = scale_name
        )
      )
    }
  )
  bf_sens_rows[[scale_name]] <- row
}
bf_sens <- bind_rows(bf_sens_rows) |>
  mutate(bf_fmt = sprintf("%.2f", bf_01)) |>
  select(DV, prior, bf_fmt) |>
  tidyr::pivot_wider(names_from = prior, values_from = bf_fmt)

print(as.data.frame(bf_sens), row.names = FALSE)

# --------------------------------------------------------------------------- #
# Forest plot inputs for Figure 1 panels A & B (T2 series)
# --------------------------------------------------------------------------- #
# Same labels, facet, and ate_flip as T1 (forest_measure_specs.R).

pl <- forest_panel_labels()
lewis_panel <- pl$lewis
haidt_panel <- pl$haidt

stopifnot(length(t2_fit) == nrow(spec_forest))

ate_data_t2 <- purrr::map_dfr(seq_len(nrow(spec_forest)), function(i) {
  row <- spec_forest[i, ]
  panel <- if (row$facet == "lewis") lewis_panel else haidt_panel
  extract_forest_coef(t2_fit[[i]], row$label, row$pre_dv, panel, "ate", row$ate_flip, d_ref)
}) |>
  mutate(
    label = factor(label, levels = rev(unique(label))),
    panel = factor(panel, levels = c(lewis_panel, haidt_panel))
  )

int_data_t2 <- purrr::map_dfr(seq_len(nrow(spec_forest)), function(i) {
  row <- spec_forest[i, ]
  panel <- if (row$facet == "lewis") lewis_panel else haidt_panel
  extract_forest_coef(
    t2_fit[[i]], row$label, row$pre_dv, panel, "interaction",
    row$ate_flip * -1L, d_ref
  )
}) |>
  mutate(
    label = factor(label, levels = rev(unique(label))),
    panel = factor(panel, levels = c(lewis_panel, haidt_panel))
  )

saveRDS(
  list(
    ate = ate_data_t2,
    int = int_data_t2,
    lewis_panel = lewis_panel,
    haidt_panel = haidt_panel
  ),
  here::here("output", "intermediate", "forest_t2.rds")
)

cat("\nSaved: output/intermediate/forest_t2.rds\n")

# --------------------------------------------------------------------------- #
# (2) Moderated mediation: T1 = wave-1 post, T2 = follow-up composite
# --------------------------------------------------------------------------- #
# text_treat = 1 for the preregistered focal text per DV (Lewis vs Haidt per MVS).

med_model <- "
  T1 ~ a1*text_treat + a2*format_eff + a3*tx_fmt + a4*pre0
  T2 ~ b1*T1 + b2*format_eff + b3*t1_fmt + b4*text_treat + b5*tx_fmt + b6*pre0

  pers_full := b1 + b3*(-0.5)
  pers_sum  := b1 + b3*(0.5)
  pers_diff := pers_full - pers_sum
  pers_mean := (pers_full + pers_sum) / 2

  ind_full := (a1 + a3*(-0.5))*(b1 + b3*(-0.5))
  dir_full := b4 + b5*(-0.5)
  ind_sum  := (a1 + a3*(0.5))*(b1 + b3*(0.5))
  dir_sum  := b4 + b5*(0.5)
  ind_diff := ind_full - ind_sum
  dir_diff := dir_full - dir_sum
  ind_mean := (ind_full + ind_sum) / 2
  dir_mean := (dir_full + dir_sum) / 2
"

# FALSE = ML standard errors + delta method CIs for defined parameters (no bootstrap)
med_use_bootstrap <- FALSE

cat("\n\n=== Moderated mediation (lavaan; wave-1 post = T1, FU = T2) ===\n")
cat(
  "Inference: ",
  if (med_use_bootstrap) "5000 bootstrap resamples\n" else "analytic (ML / delta method)\n",
  "Persistence = T1→T2 slope (pers_*). Direct = text→T2 holding T1 (dir_*). ",
  "Indirect a×b = ind_*. ",
  "Moderation by format (Full − Summary): pers_diff, dir_diff, ind_diff. ",
  "If moderation ≈ 0, emphasize pers_mean & dir_mean (and ind_mean); else test pers_full/pers_sum ",
  "and dir_full/dir_sum vs 0 separately.\n\n",
  sep = ""
)

run_mediation <- function(t1_col, t2_col, pre_col, label, text_treatment = "Lewis",
                          use_bootstrap = TRUE) {
  dat <- d_fu |>
    mutate(
      text_treat = ifelse(text == text_treatment, 1L, 0L),
      format_eff = ifelse(format == "Full", -0.5, 0.5)
    ) |>
    transmute(
      T1 = .data[[t1_col]],
      T2 = .data[[t2_col]],
      pre0 = .data[[pre_col]],
      text_treat,
      format_eff,
      tx_fmt = text_treat * format_eff
    ) |>
    mutate(t1_fmt = T1 * format_eff) |>
    filter(
      is.finite(T1), is.finite(T2), is.finite(pre0),
      is.finite(text_treat), is.finite(format_eff)
    )

  if (nrow(dat) < 30L) {
    return(tibble::tibble(DV = label, note = "insufficient N", status = "skip"))
  }

  if (use_bootstrap) {
    set.seed(270387L)
  }
  fit <- try(
    if (use_bootstrap) {
      lavaan::sem(
        med_model,
        data = dat,
        se = "bootstrap",
        bootstrap = 5000L,
        meanstructure = TRUE
      )
    } else {
      lavaan::sem(
        med_model,
        data = dat,
        se = "standard",
        meanstructure = TRUE
      )
    },
    silent = TRUE
  )

  if (inherits(fit, "try-error")) {
    return(tibble::tibble(DV = label, note = "lavaan error", status = "fail"))
  }

  pe_args <- list(
    object = fit,
    ci = TRUE,
    level = 0.95
  )
  if (use_bootstrap) {
    pe_args$boot.ci.type <- "perc"
  }
  pe <- do.call(lavaan::parameterEstimates, pe_args)

  pull_defined <- function(lhs) {
    r <- pe[pe$lhs == lhs & pe$op == ":=", , drop = FALSE]
    if (!nrow(r)) {
      return(c(est = NA_real_, lo = NA_real_, hi = NA_real_, p = NA_real_))
    }
    r <- r[1, , drop = FALSE]
    c(est = r$est, lo = r$ci.lower, hi = r$ci.upper, p = r$pvalue)
  }

  pull_labeled <- function(lab) {
    r <- pe[pe$label == lab & !is.na(pe$label), , drop = FALSE]
    if (!nrow(r)) {
      return(c(est = NA_real_, lo = NA_real_, hi = NA_real_, p = NA_real_))
    }
    r <- r[1, , drop = FALSE]
    c(est = r$est, lo = r$ci.lower, hi = r$ci.upper, p = r$pvalue)
  }

  print_row <- function(v, e) {
    cat(sprintf(
      "  %-12s est=% .4f  95%% CI [% .4f, % .4f]  p=%s\n",
      v, e["est"], e["lo"], e["hi"],
      ifelse(is.na(e["p"]), "NA", format.pval(e["p"], digits = 5))
    ))
  }

  pull_est <- function(nm) {
    if (nm %in% c("b1", "b3")) {
      pull_labeled(nm)
    } else {
      pull_defined(nm)
    }
  }

  moderation_vars <- c("pers_diff", "dir_diff", "ind_diff")
  simple_vars <- c("pers_full", "pers_sum", "dir_full", "dir_sum")
  average_vars <- c("pers_mean", "dir_mean", "ind_mean")
  detail_vars <- c("b1", "b3", "ind_full", "ind_sum")

  mod_est <- stats::setNames(lapply(moderation_vars, pull_est), moderation_vars)
  sim_est <- stats::setNames(lapply(simple_vars, pull_est), simple_vars)
  avg_est <- stats::setNames(lapply(average_vars, pull_est), average_vars)
  det_est <- stats::setNames(lapply(detail_vars, pull_est), detail_vars)

  cat("\n--- ", label, " ---\n", sep = "")
  cat("  (1) Moderation by format (Full − Summary); CI includes 0 → no strong evidence effects differ.\n")
  for (v in moderation_vars) {
    print_row(v, mod_est[[v]])
  }
  cat("  (2) Simple effects at Full vs Summary (test vs 0 when moderation is present / for reporting).\n")
  for (v in simple_vars) {
    print_row(v, sim_est[[v]])
  }
  cat("  (3) Averages across formats (pers_mean = b1; use when moderation weak; test vs 0).\n")
  for (v in average_vars) {
    print_row(v, avg_est[[v]])
  }
  cat("  (4) Components: b1, b3; indirect a×b at each format (ind_full, ind_sum).\n")
  for (v in detail_vars) {
    print_row(v, det_est[[v]])
  }

  invisible(TRUE)
}

med_specs <- tribble(
  ~t1_col, ~t2_col, ~pre_col, ~label, ~text_treatment,
  "irs_approval_post", "irs_approval_t2", "irs_approval_pre", "IRS Approval", "Lewis",
  "civil_service_post", "civil_service_t2", "civil_service_pre", "Civil Service", "Lewis",
  "doge_post_1", "doge_post_1_fu", "doge_pre_1", "DOGE Approval", "Lewis",
  "trump_post_1", "trump_post_1_fu", "trump_pre_1", "Trump Approval", "Lewis",
  "enforce_post_1", "enforce_post_1_fu", "enforce_pre_1", "IRS Enforcement", "Lewis",
  "fav_ssa_post_1", "fav_ssa_post_1_fu", "fav_ssa_pre_1", "SSA Favorability", "Lewis",
  "mvs_post", "mvs_t2", "mvs_pre", "Material Values", "Haidt"
)

pwalk(
  med_specs,
  function(t1_col, t2_col, pre_col, label, text_treatment) {
    run_mediation(t1_col, t2_col, pre_col, label, text_treatment,
      use_bootstrap = med_use_bootstrap
    )
  }
)

cat("\n(Workflow: inspect pers_diff & dir_diff; if both ~0, emphasize pers_mean & dir_mean vs 0; else\n")
cat(" emphasize pers_full/pers_sum and dir_full/dir_sum vs 0. ind_* = indirect a×b path; parallel logic.)\n")

# --------------------------------------------------------------------------- #
# T1 reanalysis with follow-up sample
# --------------------------------------------------------------------------- #
# Same specification as scripts/05_t1_analyses.R (HC2 robust SEs): wave-1
# post − pre ~ text_treat * format_eff * z(pre). Sample = d_fu only.
#
# DV labels match forest_measure_specs() (Figure 1). Raw coefs are on the
# survey scale (e.g. approval change); ate_flip aligns printed ATE / d / CIs /
# interaction with the figure’s persuasion-direction convention (same as
# extract_forest_coef flip), so signs match the outcome wording.

cat("\n\n=== T1 reanalysis with follow-up sample (wave-1 change; N = follow-up completers) ===\n\n")

t1_fu_specs <- spec_forest |>
  dplyr::transmute(
    change_dv = .data$change_dv_t1,
    pre_dv = .data$pre_dv,
    text_treatment = .data$text_treatment,
    label = .data$label,
    ate_flip = .data$ate_flip
  )

t1_fu_fit <- pmap(
  t1_fu_specs,
  function(change_dv, pre_dv, text_treatment, label, ate_flip) {
    persuasion_analysis(change_dv, pre_dv, d_fu, label, text_treatment, ref_data = d_ref)
  }
)

flip_ci <- function(fl, low, high) {
  c(
    lo = min(fl * low, fl * high, na.rm = TRUE),
    hi = max(fl * low, fl * high, na.rm = TRUE)
  )
}

ate_tbl_t1_fu <- imap_dfr(t1_fu_fit, function(a, i) {
  lab <- t1_fu_specs$label[[i]]
  fl <- t1_fu_specs$ate_flip[[i]]
  int_f <- fl * -1L
  tc <- a$text_coef
  ic <- a$interaction_coef
  if (nrow(tc) != 1L || nrow(ic) != 1L) {
    warning("Unexpected coef rows for T1 FU reanalysis: ", lab, call. = FALSE)
  }
  sd_pre <- a$pooled_sd
  ate <- if (nrow(tc) == 1L) fl * tc$estimate else NA_real_
  ate_d <- if (nrow(tc) == 1L) fl * a$std_ate else NA_real_
  ci_tc <- if (nrow(tc) == 1L) flip_ci(fl, tc$conf.low, tc$conf.high) else c(lo = NA, hi = NA)
  int_est <- if (nrow(ic) == 1L) int_f * ic$estimate else NA_real_
  int_d <- if (nrow(ic) == 1L) int_f * ic$estimate / sd_pre else NA_real_
  ci_ic <- if (nrow(ic) == 1L) flip_ci(int_f, ic$conf.low, ic$conf.high) else c(lo = NA, hi = NA)
  tibble::tibble(
    DV        = lab,
    `ATE (b)` = if (nrow(tc) == 1L) sprintf("%.3f%s", ate, sigstars(tc$p.value)) else NA_character_,
    `ATE d`   = if (nrow(tc) == 1L) sprintf("%.2f [%.2f, %.2f]", ate_d, ci_tc[["lo"]] / sd_pre, ci_tc[["hi"]] / sd_pre) else NA_character_,
    `ATE p`   = if (nrow(tc) == 1L) round(tc$p.value, 5) else NA_real_,
    `Int (b)` = if (nrow(ic) == 1L) sprintf("%.3f%s", int_est, sigstars(ic$p.value)) else NA_character_,
    `Int d`   = if (nrow(ic) == 1L) sprintf("%.2f [%.2f, %.2f]", int_d, ci_ic[["lo"]] / sd_pre, ci_ic[["hi"]] / sd_pre) else NA_character_,
    `Int p`   = if (nrow(ic) == 1L) round(ic$p.value, 5) else NA_real_
  )
})

print(as.data.frame(ate_tbl_t1_fu), row.names = FALSE)
cat("N: ", nrow(d_fu), "\n")

cat("\n=== 06_t2_analyses.R finished ===\n")

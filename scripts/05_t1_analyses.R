# =============================================================================
# 05 · T1 analyses (wave-1 post − pre)
# =============================================================================
# Implements:
#   (1) ATE estimation for all 7 preregistered DVs (HC2 robust SEs):
#       change ~ text_treat * format_eff * z(pre); Cohen's d on SD(pre).
#   (2) Binary above-midpoint LPM (rescaled persuasion-direction outcome
#       y_above ~ text_treat * format_eff * z(pre); full sample), with
#       lpm_pct_persuaded ≈ LPM est / pre_below_rate.
#   (3) SSA-within-format estimates (text effect within Full vs Summary).
#   (4) Bayes factors for format equivalence (additive vs full on
#       pre-controlled change), with medium / wide / ultrawide prior
#       sensitivity (same comparison as T2 in 06_t2_analyses.R).
#   (5) Forest-plot inputs for Figure 1 (T1 series) → output/intermediate/forest_t1.rds.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(estimatr)
  library(broom)
  library(BayesFactor)
})

source(here::here("scripts", "functions", "forest_plot_estimates.R"))
source(here::here("scripts", "functions", "forest_measure_specs.R"))

d <- readRDS(here::here("data", "d.rds"))
fs::dir_create(here::here("output", "intermediate"))

# =============================================================================
# Run analyses for all 7 DVs
# =============================================================================

irs_approval   <- persuasion_analysis("irs_approval_change",  "irs_approval_pre",  d, "IRS Approval",     "Lewis")
civil_service  <- persuasion_analysis("civil_service_change", "civil_service_pre", d, "Civil Service",     "Lewis")
doge_approval  <- persuasion_analysis("doge_change",          "doge_pre_1",        d, "DOGE Approval",    "Lewis")
trump_approval <- persuasion_analysis("trump_change",         "trump_pre_1",       d, "Trump Approval",   "Lewis")
irs_enforce    <- persuasion_analysis("enforce_change",       "enforce_pre_1",     d, "IRS Enforcement",  "Lewis")
fav_ssa        <- persuasion_analysis("fav_ssa_change",       "fav_ssa_pre_1",     d, "SSA Favorability", "Lewis")
# Focal book = Pursuit of Happiness → text_treat: Haidt = 1, Lewis = 0
mvs            <- persuasion_analysis("mvs_change",           "mvs_pre",           d, "Material Values",  "Haidt")

# Order matches scripts/functions/forest_measure_specs.R (same as Figure 1 / T2).
spec_forest <- forest_measure_specs()
analyses_t1 <- list(
  irs_approval,
  irs_enforce,
  civil_service,
  fav_ssa,
  doge_approval,
  trump_approval,
  mvs
)
stopifnot(length(analyses_t1) == nrow(spec_forest))

# =============================================================================
# SSA favorability: text effect within Full vs Summary (separate models)
# =============================================================================
# Outcome ~ text * pre_score_z within each format. Cohen's d matches main table:
# b / SD(pre) on the full completers sample.

ssa_within_format <- function(data, format_label, change_dv, pre_dv) {
  data_model <- data |>
    mutate(pre_score_z = scale(.data[[pre_dv]])[, 1]) |>
    filter(format == format_label)

  formula_str <- paste0(change_dv, " ~ text * pre_score_z")
  mod <- lm_robust(as.formula(formula_str), data = data_model)
  coefs <- tidy(mod, conf.int = TRUE)
  text_main <- coefs |>
    filter(grepl("^text", term), !grepl(":", term, fixed = TRUE))
  int_coef <- coefs |>
    filter(grepl("^text", term), grepl(":", term, fixed = TRUE))
  pooled_sd <- sd(data[[pre_dv]], na.rm = TRUE)
  # Default R contrast: reference is Lewis_*; coef is Haidt − Lewis. Flip so
  # estimates match the main ATE table (Lewis − Haidt, Cohen's d same sign).
  flip <- if (nrow(text_main) == 1 && grepl("Haidt", text_main$term)) -1L else 1L
  list(
    model = mod, coefs = coefs, text_coef = text_main,
    interaction_coef = int_coef, pooled_sd = pooled_sd,
    flip = flip,
    std_ate = flip * text_main$estimate / pooled_sd
  )
}

ssa_full    <- ssa_within_format(d, "Full",    "fav_ssa_change", "fav_ssa_pre_1")
ssa_summary <- ssa_within_format(d, "Summary", "fav_ssa_change", "fav_ssa_pre_1")

# =============================================================================
# ATE summary table
# =============================================================================

sigstars <- function(p) {
  dplyr::case_when(
    p < 0.001 ~ "***", p < 0.01 ~ "**", p < 0.05 ~ "*", p < 0.1 ~ ".", TRUE ~ ""
  )
}

cat("\n=== Average Treatment Effects (ATE) ===\n\n")
cat("DV labels and signs match forest_measure_specs (ate_flip); Cohen's d = flip × b / SD(pre).\n")
cat("Int = text × format interaction (Full − Summary difference).\n\n")

ate_table <- purrr::map2_dfr(analyses_t1, seq_len(nrow(spec_forest)), function(a, i) {
  row <- spec_forest[i, ]
  tc <- a$text_coef
  ic <- a$interaction_coef
  ate_f <- row$ate_flip
  int_f <- ate_f * -1L
  t_ate <- flip_robust_coef(tc, ate_f)
  t_int <- flip_robust_coef(ic, int_f)
  ate_d <- ate_f * a$std_ate
  int_d <- if (nrow(ic) == 1L) int_f * ic$estimate / a$pooled_sd else NA_real_
  if (nrow(tc) != 1L || nrow(ic) != 1L) {
    warning("Unexpected coef rows for ", row$label, call. = FALSE)
  }
  sd_pre <- a$pooled_sd
  tibble::tibble(
    DV          = row$label,
    `ATE (b)`   = if (nrow(tc) == 1L) sprintf("%.3f%s", t_ate$estimate, sigstars(tc$p.value)) else NA_character_,
    `ATE d`     = if (nrow(tc) == 1L) sprintf("%.2f [%.2f, %.2f]", ate_d, t_ate$conf.low / sd_pre, t_ate$conf.high / sd_pre) else NA_character_,
    `ATE p`     = if (nrow(tc) == 1L) round(tc$p.value, 5) else NA_real_,
    `Int (b)`   = if (nrow(ic) == 1L) sprintf("%.3f%s", t_int$estimate, sigstars(ic$p.value)) else NA_character_,
    `Int d`     = if (nrow(ic) == 1L) sprintf("%.2f [%.2f, %.2f]", int_d, t_int$conf.low / sd_pre, t_int$conf.high / sd_pre) else NA_character_,
    `Int p`     = if (nrow(ic) == 1L) round(ic$p.value, 5) else NA_real_
  )
})

print(as.data.frame(ate_table), row.names = FALSE)

# =============================================================================
# Binary above-midpoint LPM (full sample, persuasion-oriented rescaled scale)
# =============================================================================
# Rescaled pre/post: reverse-coded (8−x on 1–7, or 6−x on MVS 1–5) where needed
# so higher = persuasion direction (same logic as Panel A forest signs).
# y = 1 if rescaled post >= 4 (scale midpoint), 0 otherwise — full sample.
# LPM: y_above ~ text_treat * format_eff * pre_score_z.
# pre_below_rate = P(below cut at pre), pooled = maximum possible pp shift.
# lpm_pct_persuaded ≈ lpm_est / pre_below_rate (~ fraction of persuadable people persuaded,
# assuming control stays at pre baseline; can exceed 1 if control moves opposite direction).

rescale_persuasion <- function(x, reverse, scale_hi) {
  if (reverse) scale_hi + 1 - x else x
}

binary_above_cut_lpm <- function(data, pre_col, post_col, reverse, scale_hi,
                                 text_treatment, label, cut = 4) {
  d_an <- data |>
    transmute(
      text, format,
      pre_rs  = rescale_persuasion(.data[[pre_col]],  reverse, scale_hi),
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

binary_es_specs <- tribble(
  ~label,                       ~pre_col,              ~post_col,            ~reverse, ~scale_hi, ~text_treatment, ~cut,
  "IRS Favorability",           "irs_approval_pre",    "irs_approval_post",  FALSE,     7L,        "Lewis",        4,
  "IRS Funding Support",        "enforce_pre_1",       "enforce_post_1",      FALSE,     7L,        "Lewis",        4,
  "Civil Service Favorability", "civil_service_pre",   "civil_service_post", FALSE,     7L,        "Lewis",        4,
  "SSA Agent Favorability",     "fav_ssa_pre_1",       "fav_ssa_post_1",     FALSE,     7L,        "Lewis",        4,
  "DOGE Disapproval",           "doge_pre_1",          "doge_post_1",        TRUE,      7L,        "Lewis",        4,
  "Trump Disapproval",          "trump_pre_1",         "trump_post_1",       TRUE,      7L,        "Lewis",        4,
  "Reduced Material Values",    "mvs_pre",             "mvs_post",           TRUE,      5L,        "Haidt",        3
)

cat("\n=== Binary above-midpoint LPM (rescaled post >= cut; full sample) ===\n")
cat("Cut = scale midpoint (4 for 1–7 scales, 3 for 1–5 MVS).\n")
cat("LPM: y_above ~ text_treat * format_eff * pre_score_z.\n")
cat("text_treat = 1 for the focal book in each row.\n")
cat("pre_pct_below = % below cut at pre (pooled) = room to move.\n")
cat("lpm_pct_persuaded ≈ LPM est / pre_below_rate (~fraction of persuadable people persuaded;\n")
cat("  assumes control stays at pre baseline; can exceed 100% if control moves opposite direction).\n\n")

binary_tbl <- pmap_dfr(
  binary_es_specs,
  function(label, pre_col, post_col, reverse, scale_hi, text_treatment, cut) {
    binary_above_cut_lpm(d, pre_col, post_col, reverse, scale_hi, text_treatment, label, cut)
  }
) |> arrange(DV)

print(as.data.frame(binary_tbl), row.names = FALSE)

# =============================================================================
# Forest plot data setup (used by SSA within-format and forest RDS below)
# =============================================================================

pl <- forest_panel_labels()
lewis_panel <- pl$lewis
haidt_panel <- pl$haidt

forest_specs <- purrr::map2(
  analyses_t1,
  seq_len(nrow(spec_forest)),
  function(a, i) {
    row <- spec_forest[i, ]
    list(
      a = a,
      label = row$label,
      pre = row$pre_dv,
      ate_flip = row$ate_flip,
      facet = row$facet
    )
  }
)

cat("\n=== SSA Agent Favorability: text effect within format, as sig interaction ===\n")
cat("(fav_ssa_change ~ text * pre_score_z; pre_score_z from full sample)\n")
cat("ATE / d: Lewis (Cyber Sleuth) − Haidt (Pursuit of Happiness), same as main table.\n\n")

ssa_within_rows <- function(a, fmt_lab) {
  tc <- a$text_coef
  ic <- a$interaction_coef
  f <- a$flip
  est <- f * tc$estimate
  ci_lo <- f * tc$conf.high
  ci_hi <- f * tc$conf.low
  tibble::tibble(
    Format       = fmt_lab,
    `ATE (b)`    = sprintf("%.3f%s", est, sigstars(tc$p.value)),
    `Cohen's d`  = sprintf("%.2f", a$std_ate),
    `95% CI`     = sprintf("[%.3f, %.3f]", ci_lo, ci_hi),
    p            = round(tc$p.value, 5),
    `Pre × text` = if (nrow(ic) == 1) {
      sprintf("%.3f%s", f * ic$estimate, sigstars(ic$p.value))
    } else {
      NA_character_
    },
    `Int. p`     = if (nrow(ic) == 1) round(ic$p.value, 5) else NA_real_
  )
}

ssa_within_tbl <- bind_rows(
  ssa_within_rows(ssa_full, "Full text"),
  ssa_within_rows(ssa_summary, "AI summary")
)
print(as.data.frame(ssa_within_tbl), row.names = FALSE)

# =============================================================================
# Bayes factors for format equivalence (pre-controlled change only; matches T2)
# =============================================================================
# Compare additive vs full interaction on change (post − pre), with z-scored pre:
#   full:    change ~ pre_z + text + format + text:format
#   additive: change ~ pre_z + text + format
# BF01 = ML(additive) / ML(full). Values > 1 favor no text×format interaction.
# JZS priors: rscaleFixed = rscaleCont = medium (wide/ultrawide in sensitivity).

cat("\n\n=== Bayes Factors for Format Equivalence (BF01) ===\n")
cat("Tests whether the text treatment effect differs by format (full text vs AI summary).\n")
cat("Two nested models on change scores (post − pre), both controlling for z-scored pre:\n")
cat("  Full:     change ~ pre_z + text + format + text:format\n")
cat("  Additive: change ~ pre_z + text + format\n")
cat("BF01 = marginal likelihood(additive) / marginal likelihood(full).\n")
cat("BF01 > 1 favors H0 (no text × format interaction, i.e. format equivalence).\n")
cat("BF01 > 3 is conventionally 'moderate' evidence for equivalence.\n")
cat("JZS priors: rscaleFixed = rscaleCont = medium (sensitivity analysis varies these).\n\n")

compute_bf_with_pre <- function(outcome_col, pre_col, text_treatment, data,
                                rscale = "medium", rscale_cont = "medium",
                                seed = 417201L) {
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

  bf_full <- lmBF(
    outcome ~ pre_score_z + text_treat + format_eff + text_treat:format_eff,
    data = df,
    rscaleFixed = rscale,
    rscaleCont = rscale_cont,
    iterations = 50000
  )

  bf_additive <- lmBF(
    outcome ~ pre_score_z + text_treat + format_eff,
    data = df,
    rscaleFixed = rscale,
    rscaleCont = rscale_cont,
    iterations = 50000
  )

  bf_01 <- bf_additive / bf_full
  extractBF(bf_01)$bf
}

bf_specs <- tribble(
  ~change_dv,             ~pre_dv,               ~text_treatment, ~label,
  "irs_approval_change",  "irs_approval_pre",   "Lewis",         "IRS Favorability",
  "enforce_change",       "enforce_pre_1",       "Lewis",         "IRS Funding Support",
  "civil_service_change", "civil_service_pre",  "Lewis",         "Civil Service Favorability",
  "fav_ssa_change",       "fav_ssa_pre_1",     "Lewis",         "SSA Agent Favorability",
  "doge_change",          "doge_pre_1",        "Lewis",         "DOGE Disapproval",
  "trump_change",         "trump_pre_1",       "Lewis",         "Trump Disapproval",
  "mvs_change",           "mvs_pre",           "Haidt",         "Reduced Material Values"
)

bf_primary <- pmap_dfr(bf_specs, function(change_dv, pre_dv, text_treatment, label) {
  tibble::tibble(
    label = label,
    bf_01 = compute_bf_with_pre(change_dv, pre_dv, text_treatment, d)
  )
})
print(as.data.frame(bf_primary |> mutate(bf_01 = round(bf_01, 2))))

# =============================================================================
# BF prior sensitivity (same pre-controlled change specification as above)
# =============================================================================

cat("\n\n=== BF01 Sensitivity (rscaleFixed / rscaleCont grids; pre-controlled change) ===\n\n")

rscale_grid <- c("medium" = 0.5, "wide" = sqrt(2) / 2, "ultrawide" = 1)

bf_sensitivity_pre_change <- map_dfr(names(rscale_grid), function(scale_name) {
  pmap_dfr(bf_specs, function(change_dv, pre_dv, text_treatment, label) {
    tibble::tibble(
      DV    = label,
      prior = scale_name,
      bf_01 = compute_bf_with_pre(
        change_dv, pre_dv, text_treatment, d,
        rscale = rscale_grid[scale_name],
        rscale_cont = scale_name
      )
    )
  })
}) |>
  mutate(bf_fmt = sprintf("%.2f", bf_01)) |>
  select(DV, prior, bf_fmt) |>
  pivot_wider(names_from = prior, values_from = bf_fmt)

print(as.data.frame(bf_sensitivity_pre_change), row.names = FALSE)

# =============================================================================
# Forest plot inputs for Figure 1 (panels A & B) — consumed by 07_main_figure.R
# =============================================================================

ate_data <- purrr::map_dfr(seq_along(forest_specs), function(i) {
  x <- forest_specs[[i]]
  row <- spec_forest[i, ]
  panel <- if (row$facet == "lewis") lewis_panel else haidt_panel
  extract_forest_coef(x$a, x$label, x$pre, panel, "ate", row$ate_flip, d)
}) |>
  mutate(
    label = factor(label, levels = rev(unique(label))),
    panel = factor(panel, levels = c(lewis_panel, haidt_panel))
  )

int_data_fig <- purrr::map_dfr(seq_along(forest_specs), function(i) {
  x <- forest_specs[[i]]
  row <- spec_forest[i, ]
  panel <- if (row$facet == "lewis") lewis_panel else haidt_panel
  extract_forest_coef(
    x$a, x$label, x$pre, panel, "interaction",
    row$ate_flip * -1L, d
  )
}) |>
  mutate(
    label = factor(label, levels = rev(unique(label))),
    panel = factor(panel, levels = c(lewis_panel, haidt_panel))
  )

saveRDS(
  list(
    ate = ate_data,
    int = int_data_fig,
    lewis_panel = lewis_panel,
    haidt_panel = haidt_panel
  ),
  here::here("output", "intermediate", "forest_t1.rds")
)

cat("\nSaved: output/intermediate/forest_t1.rds\n")

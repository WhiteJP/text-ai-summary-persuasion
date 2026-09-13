# Cohen's d: coef / SD(pre) in wave-1 completers (`d_ref` / `ref_data`), not the
# analysis sample SD.  This keeps d comparable across T1, T2, and robustness analyses.
#
# Treatment / format coding: `code_text_treat` / `code_format_eff` in analysis_helpers.R.
#   - Cyber Sleuth outcomes: text_treatment = "Lewis" → Lewis = 1, Haidt = 0.
#   - Material Values (Pursuit of Happiness): text_treatment = "Haidt" → Haidt = 1, Lewis = 0.

source(here::here("scripts", "functions", "analysis_helpers.R"))

persuasion_analysis <- function(change_dv, pre_dv, data,
                                label, text_treatment = "Lewis",
                                ref_data = NULL) {
  data_model <- data %>%
    dplyr::filter(!is.na(.data[[change_dv]]), !is.na(.data[[pre_dv]])) %>%
    dplyr::mutate(
      text_treat   = code_text_treat(text, text_treatment),
      format_eff   = code_format_eff(format),
      pre_score_z  = as.numeric(scale(.data[[pre_dv]]))
    )

  formula_str <- paste0(change_dv, " ~ text_treat * format_eff * pre_score_z")
  mod    <- estimatr::lm_robust(as.formula(formula_str), data = data_model)
  lm_mod <- stats::lm(as.formula(formula_str), data = data_model)

  coefs            <- broom::tidy(mod, conf.int = TRUE)
  text_coef        <- coefs |> dplyr::filter(term == "text_treat")
  interaction_coef <- coefs |> dplyr::filter(term == "text_treat:format_eff")

  sd_source <- if (!is.null(ref_data)) ref_data else data
  pooled_sd <- stats::sd(sd_source[[pre_dv]], na.rm = TRUE)

  list(
    model = mod, lm_model = lm_mod, coefs = coefs,
    text_coef = text_coef, interaction_coef = interaction_coef,
    pooled_sd = pooled_sd
  )
}

#' Flip a one-row tidy coef onto the persuasion scale (b and Cohen's d).
#'
#' `flip` is `ate_flip` (+1 / −1). CIs swap bounds when flipped. `sd_pre` is
#' pooled pre-treatment SD (wave-1 completers when the caller passes `ref_data`).
orient_coef <- function(tc, flip, sd_pre) {
  empty <- list(
    estimate = NA_real_, conf.low = NA_real_, conf.high = NA_real_,
    d = NA_real_, d_lo = NA_real_, d_hi = NA_real_, se_d = NA_real_,
    p = NA_real_
  )
  if (is.null(tc) || nrow(tc) != 1L) {
    return(empty)
  }
  f <- as.integer(flip)
  lo <- f * (if (f == 1L) tc$conf.low else tc$conf.high)
  hi <- f * (if (f == 1L) tc$conf.high else tc$conf.low)
  list(
    estimate = f * tc$estimate,
    conf.low = lo,
    conf.high = hi,
    d = f * tc$estimate / sd_pre,
    d_lo = lo / sd_pre,
    d_hi = hi / sd_pre,
    se_d = tc$std.error / sd_pre,
    p = tc$p.value
  )
}

#' Console ATE / interaction row (same signs as the forest).
ate_int_table_row <- function(analysis, label, flip) {
  tc <- analysis$text_coef
  ic <- analysis$interaction_coef
  if (nrow(tc) != 1L || nrow(ic) != 1L) {
    warning("Unexpected coef rows for ", label, call. = FALSE)
  }
  t_ate <- orient_coef(tc, flip, analysis$pooled_sd)
  t_int <- orient_coef(ic, flip, analysis$pooled_sd)
  fmt_b <- function(o) {
    if (is.na(o$estimate)) NA_character_ else sprintf("%.3f%s", o$estimate, sigstars(o$p))
  }
  fmt_d <- function(o) {
    if (is.na(o$d)) NA_character_ else sprintf("%.2f [%.2f, %.2f]", o$d, o$d_lo, o$d_hi)
  }
  tibble::tibble(
    DV = label,
    `ATE (b)` = fmt_b(t_ate),
    `ATE d` = fmt_d(t_ate),
    `ATE p` = if (is.na(t_ate$p)) NA_real_ else round(t_ate$p, 5),
    `Int (b)` = fmt_b(t_int),
    `Int d` = fmt_d(t_int),
    `Int p` = if (is.na(t_int$p)) NA_real_ else round(t_int$p, 5)
  )
}

extract_forest_coef <- function(analysis, label, panel, coef_type, flip) {
  tc <- if (coef_type == "ate") analysis$text_coef else analysis$interaction_coef
  if (nrow(tc) == 0) return(NULL)
  o <- orient_coef(tc, flip, analysis$pooled_sd)
  tibble::tibble(
    label   = label,
    panel   = panel,
    effect  = coef_type,
    d       = o$d,
    se_d    = o$se_d,
    ci_low  = o$d_lo,
    ci_high = o$d_hi,
    p       = o$p
  )
}

# Cohen's d: coef / SD(pre) in wave-1 completers (`d_ref` / `ref_data`), not the
# analysis sample SD.  This keeps d comparable across T1, T2, and robustness analyses.
#
# Treatment dummy `text_treat`: 1 = preregistered focal book, 0 = the other book.
#   - Cyber Sleuth outcomes: text_treatment = "Lewis" → Lewis = 1, Haidt = 0.
#   - Material Values (Pursuit of Happiness): text_treatment = "Haidt" → Haidt = 1, Lewis = 0.
# The lm coef on text_treat is then the contrast for that focal book vs the other.

persuasion_analysis <- function(change_dv, pre_dv, data,
                                label, text_treatment = "Lewis",
                                ref_data = NULL) {
  data_model <- data %>%
    dplyr::mutate(
      text_treat   = ifelse(text == text_treatment, 1L, 0L),
      format_eff   = ifelse(format == "Full", -0.5, 0.5),
      pre_score_z  = scale(.data[[pre_dv]])[, 1]
    )

  formula_str <- paste0(change_dv, " ~ text_treat * format_eff * pre_score_z")
  mod    <- estimatr::lm_robust(as.formula(formula_str), data = data_model)
  lm_mod <- stats::lm(as.formula(formula_str), data = data_model)

  coefs            <- broom::tidy(mod, conf.int = TRUE)
  text_coef        <- coefs |> dplyr::filter(term == "text_treat")
  interaction_coef <- coefs |> dplyr::filter(term == "text_treat:format_eff")

  sd_source <- if (!is.null(ref_data)) ref_data else data
  pooled_sd <- stats::sd(sd_source[[pre_dv]], na.rm = TRUE)
  std_ate   <- text_coef$estimate / pooled_sd

  list(
    model = mod, lm_model = lm_mod, coefs = coefs,
    text_coef = text_coef, interaction_coef = interaction_coef,
    pooled_sd = pooled_sd, std_ate = std_ate
  )
}

#' One-row lm_robust tidy coef on the b scale (same flip rules as extract_forest_coef).
#' @param flip `ate_flip` for ATE; use `ate_flip * -1L` for text×format interaction.
flip_robust_coef <- function(tc, flip) {
  if (is.null(tc) || nrow(tc) != 1L) {
    return(list(estimate = NA_real_, conf.low = NA_real_, conf.high = NA_real_))
  }
  f <- as.integer(flip)
  list(
    estimate = f * tc$estimate,
    conf.low = f * (if (f == 1L) tc$conf.low else tc$conf.high),
    conf.high = f * (if (f == 1L) tc$conf.high else tc$conf.low)
  )
}

extract_forest_coef <- function(analysis, label, pre_dv, panel, coef_type, flip, d_ref) {
  pre_sd <- stats::sd(d_ref[[pre_dv]], na.rm = TRUE)
  tc <- if (coef_type == "ate") analysis$text_coef else analysis$interaction_coef
  if (nrow(tc) == 0) return(NULL)
  tibble::tibble(
    label   = label,
    panel   = panel,
    effect  = coef_type,
    d       = flip * tc$estimate / pre_sd,
    se_d    = tc$std.error / pre_sd,
    ci_low  = flip * (if (flip ==  1) tc$conf.low  else tc$conf.high) / pre_sd,
    ci_high = flip * (if (flip ==  1) tc$conf.high else tc$conf.low)  / pre_sd,
    p       = tc$p.value
  )
}

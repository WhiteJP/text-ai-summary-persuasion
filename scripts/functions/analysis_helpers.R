# Shared estimators used by 05, 06, 07, and 08.
# Numbered scripts source this file; you do not need to run it yourself.

#' Focal-book dummy: 1 = `focal` (Lewis or Haidt), 0 = the other book.
code_text_treat <- function(text, focal) {
  as.integer(text == focal)
}

#' Format contrast: Full = +0.5, Summary = −0.5.
#' `text_treat` is then the format-averaged ATE; `text_treat:format_eff` is Full − Summary.
code_format_eff <- function(format) {
  ifelse(format == "Full", 0.5, -0.5)
}

#' Significance stars. Console uses `.` for p < .10; set `tex = TRUE` for a dagger.
sigstars <- function(p, tex = FALSE) {
  dplyr::case_when(
    is.na(p) ~ "",
    p < 0.001 ~ "***",
    p < 0.01 ~ "**",
    p < 0.05 ~ "*",
    p < 0.1 ~ if (isTRUE(tex)) "$^{\\dagger}$" else ".",
    TRUE ~ ""
  )
}

#' Flip a 1–k item / composite so higher = persuasion direction.
#'
#' `scale_hi + 1 - x` is 8−x on 1–7 items and 6−x on the 1–5 MVS composite.
#' Used only for the binary LPM; ATE / forest sign flips use `ate_flip`.
rescale_persuasion <- function(x, reverse, scale_hi) {
  if (reverse) scale_hi + 1L - x else x
}

#' Linear probability model: above-midpoint on the persuasion-oriented scale.
#'
#' Midpoint cut is 4 on 1–7 outcomes and 3 on the 1–5 MVS (the scale center).
#' After optional reverse-coding, y = 1 if rescaled post >= cut.
#' `lpm_pct_persuaded` = LPM / pre_below_rate. Pass `ref_data` (wave-1
#' completers) so T2 uses the same pre_below_rate as T1, matching Cohen's d.
binary_above_cut_lpm <- function(data, pre_col, post_col, reverse, scale_hi,
                                 text_treatment, label, cut = 4,
                                 ref_data = NULL) {
  d_an <- data |>
    dplyr::transmute(
      text, format,
      pre_rs = rescale_persuasion(.data[[pre_col]], reverse, scale_hi),
      post_rs = rescale_persuasion(.data[[post_col]], reverse, scale_hi)
    ) |>
    dplyr::filter(!is.na(.data$pre_rs), !is.na(.data$post_rs)) |>
    dplyr::mutate(
      y_above = as.integer(.data$post_rs >= cut),
      y_above_pre = as.integer(.data$pre_rs >= cut),
      text_treat = code_text_treat(.data$text, text_treatment),
      format_eff = code_format_eff(.data$format),
      pre_score_z = as.numeric(scale(.data$pre_rs))
    )

  if (is.null(ref_data)) {
    pre_below_rate <- 1 - mean(d_an$y_above_pre, na.rm = TRUE)
  } else {
    pre_rs_ref <- rescale_persuasion(ref_data[[pre_col]], reverse, scale_hi)
    pre_below_rate <- 1 - mean(pre_rs_ref >= cut, na.rm = TRUE)
  }

  mod <- estimatr::lm_robust(
    y_above ~ text_treat * format_eff * pre_score_z,
    data = d_an
  )
  tc <- broom::tidy(mod, conf.int = TRUE) |> dplyr::filter(.data$term == "text_treat")
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

#' BF01 for format equivalence on pre-controlled change (additive vs full).
#'
#' Same JZS comparison as the paper: change ~ pre_z + text + format [+ text:format].
#' Callers pass `seed` so T1 (417201) and T2 (270387) stay reproducible.
compute_bf_with_pre <- function(outcome_col, pre_col, text_treatment, data,
                                rscale = "medium", rscale_cont = "medium",
                                seed = 417201L) {
  set.seed(seed)

  data_model <- data |>
    dplyr::mutate(
      text_treat = factor(code_text_treat(.data$text, text_treatment)),
      format_eff = factor(.data$format)
    ) |>
    dplyr::select(
      outcome = dplyr::all_of(outcome_col),
      pre_raw = dplyr::all_of(pre_col),
      text_treat,
      format_eff
    ) |>
    tidyr::drop_na() |>
    dplyr::mutate(pre_score_z = as.numeric(scale(.data$pre_raw))) |>
    dplyr::select(outcome, pre_score_z, text_treat, format_eff)

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

  BayesFactor::extractBF(bf_additive / bf_full)$bf
}

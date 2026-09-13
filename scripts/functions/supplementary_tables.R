# Helpers to write paper supplementary LaTeX tables from analysis outputs.

source(here::here("scripts", "functions", "analysis_helpers.R"))

fmt_num_tex <- function(x, digits = 2) {
  s <- sprintf(paste0("%.", digits, "f"), x)
  ifelse(x < 0, paste0("$-$", sub("^-", "", s)), s)
}

fmt_est_ci_stars <- function(est, lo, hi, p, digits = 2) {
  paste0(
    fmt_num_tex(est, digits),
    " [", fmt_num_tex(lo, digits), ", ", fmt_num_tex(hi, digits), "]",
    sigstars(p, tex = TRUE)
  )
}

fmt_p_tex <- function(p) {
  ifelse(is.na(p), "---", sprintf("%.3f", p))
}

#' Write BF prior-sensitivity LaTeX table (T1 + T2).
#'
#' @param t1_long data.frame with columns DV, prior, bf_01
#' @param t2_long data.frame with columns DV, prior, bf_01
#' @param path output .tex path
write_bf_prior_sensitivity_tex <- function(t1_long, t2_long, path) {
  dv_order <- c(
    "IRS Favorability",
    "IRS Funding Support",
    "Civil Service Favorability",
    "SSA Agent Favorability",
    "DOGE Disapproval",
    "Trump Disapproval",
    "Reduced Material Values"
  )
  prior_order <- c("medium", "wide", "ultrawide")

  wide_one <- function(df, prefix) {
    df |>
      dplyr::mutate(
        prior = factor(prior, levels = prior_order),
        bf_fmt = sprintf("%.2f", bf_01)
      ) |>
      dplyr::select(DV, prior, bf_fmt) |>
      tidyr::pivot_wider(names_from = prior, values_from = bf_fmt) |>
      dplyr::rename_with(~ paste0(prefix, .x), .cols = -DV)
  }

  combined <- wide_one(t1_long, "t1_") |>
    dplyr::left_join(wide_one(t2_long, "t2_"), by = "DV") |>
    dplyr::mutate(DV = factor(DV, levels = dv_order)) |>
    dplyr::arrange(DV)

  tex_lines <- c(
    "\\begin{table}[!htbp]",
    "  \\centering",
    "  \\caption{Bayes-factor prior sensitivity for format equivalence ($BF_{01}$)}",
    "  \\label{tab:bf-prior-sensitivity}",
    "  \\footnotesize",
    "  \\setlength{\\tabcolsep}{4pt}",
    "  \\begin{tabular}{@{}lcccccc@{}}",
    "  \\toprule",
    "  & \\multicolumn{3}{c}{Immediately post-treatment} & \\multicolumn{3}{c}{2-month follow-up} \\\\",
    "  \\cmidrule(lr){2-4} \\cmidrule(lr){5-7}",
    "  Outcome & Medium & Wide & Ultrawide & Medium & Wide & Ultrawide \\\\",
    "  \\midrule"
  )

  for (i in seq_len(nrow(combined))) {
    tex_lines <- c(
      tex_lines,
      sprintf(
        "  %s & %s & %s & %s & %s & %s & %s \\\\",
        as.character(combined$DV[[i]]),
        combined$t1_medium[[i]],
        combined$t1_wide[[i]],
        combined$t1_ultrawide[[i]],
        combined$t2_medium[[i]],
        combined$t2_wide[[i]],
        combined$t2_ultrawide[[i]]
      )
    )
  }

  tex_lines <- c(
    tex_lines,
    "  \\bottomrule",
    "  \\end{tabular}",
    "  \\vspace{2pt}",
    "  \\begin{minipage}{0.98\\linewidth}",
    paste0(
      "    \\footnotesize\\textit{Note.} JZS Bayes factors ($BF_{01}$) comparing an additive model ",
      "(text + format + pre-score) to a full interaction model (text + format + text $\\times$ format + pre-score) ",
      "on pre-controlled change scores. Columns vary the Cauchy prior scale on standardized fixed and continuous ",
      "effects jointly: medium ($r = 0.5$; primary), wide ($r = \\sqrt{2}/2$), and ultrawide ($r = 1$). ",
      "$BF_{01} > 1$ favors format equivalence; $BF_{01} > 3$ is conventionally moderate evidence for equivalence ",
      "\\parencite{leeBayesianCognitiveModeling2014}. Wider priors generally increase $BF_{01}$ when the observed ",
      "interaction is small."
    ),
    "  \\end{minipage}",
    "\\end{table}",
    ""
  )

  writeLines(tex_lines, path)
  invisible(path)
}

#' Write moderated-mediation LaTeX table for the paper SM.
#'
#' @param med_wide data.frame with one row per DV and columns:
#'   DV, pers_est, pers_lo, pers_hi, pers_p,
#'   dir_est, dir_lo, dir_hi, dir_p,
#'   ind_est, ind_lo, ind_hi, ind_p,
#'   pers_diff_p
#' @param path output .tex path
#' @param n_fu follow-up N for the note
write_mediation_persistence_tex <- function(med_wide, path, n_fu) {
  dv_pub <- c(
    "IRS Approval" = "IRS Favorability",
    "Civil Service" = "Civil Service Favorability",
    "DOGE Approval" = "DOGE Approval",
    "Trump Approval" = "Trump Approval",
    "IRS Enforcement" = "IRS Funding Support",
    "SSA Favorability" = "SSA Agent Favorability",
    "Material Values" = "Material Values"
  )
  dv_order <- unname(dv_pub)

  tab <- med_wide |>
    dplyr::mutate(
      Outcome = dplyr::recode(DV, !!!dv_pub),
      Outcome = factor(Outcome, levels = dv_order),
      pers_cell = fmt_est_ci_stars(pers_est, pers_lo, pers_hi, pers_p),
      dir_cell = fmt_est_ci_stars(dir_est, dir_lo, dir_hi, dir_p),
      ind_cell = fmt_est_ci_stars(ind_est, ind_lo, ind_hi, ind_p),
      mod_cell = fmt_p_tex(pers_diff_p)
    ) |>
    dplyr::arrange(Outcome)

  tex_lines <- c(
    "\\begin{table}[!htbp]",
    "  \\centering",
    "  \\caption{Moderated mediation of 2-month persistence through immediate post-treatment scores}",
    "  \\label{tab:mediation}",
    "  \\footnotesize",
    "  \\setlength{\\tabcolsep}{3pt}",
    "  \\begin{tabular}{@{}lcccc@{}}",
    "  \\toprule",
    "  Outcome & Persistence ($b$) & Direct / sleeper & Indirect ($a \\times b$) & Format mod.\\ ($p$) \\\\",
    "  \\midrule"
  )

  for (i in seq_len(nrow(tab))) {
    tex_lines <- c(
      tex_lines,
      sprintf(
        "  %s & %s & %s & %s & %s \\\\",
        as.character(tab$Outcome[[i]]),
        tab$pers_cell[[i]],
        tab$dir_cell[[i]],
        tab$ind_cell[[i]],
        tab$mod_cell[[i]]
      )
    )
  }

  note <- paste0(
    "    \\footnotesize\\textit{Note.} Table reports effects averaged across format, as persistence they did not ",
    "differ by format (Format mod.\\ shows corresponding non-significant $p$ values). Estimates are on the raw ",
    "outcome scale, with all indirect effect point estimates in the direction of expected persuasion (i.e., for ",
    "DOGE Approval, Trump Approval, and Material Values negative values show reduced approval / materialism). ",
    "$^{\\dagger}p < .10$, *$p < .05$, **$p < .01$, ***$p < .001$."
  )

  tex_lines <- c(
    tex_lines,
    "  \\bottomrule",
    "  \\end{tabular}",
    "  \\vspace{2pt}",
    "  \\begin{minipage}{0.98\\linewidth}",
    note,
    "  \\end{minipage}",
    "\\end{table}",
    ""
  )

  writeLines(tex_lines, path)
  invisible(path)
}

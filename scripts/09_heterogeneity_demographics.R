# =============================================================================
# 09 · Treatment-Effect Heterogeneity by Demographics (T1 + T2)
# =============================================================================
# Tests whether treatment effects vary across demographic moderators.
# Model: change ~ text_treat * moderator + pre_score_z (no format).
# Holm correction applied across moderators within each DV.
# Outputs a summary table of moderator p-values (raw and Holm-corrected).
#
# Part 1: Wave-1 completers (T1 − pre).
# Part 2 (conditional): Follow-up completers (T2 − pre), same moderators/
#   correction. Requires data/d_with_followup.rds from 01b_link_followup_data.R.

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(tidyr)
  library(estimatr)
  library(broom)
  library(readr)
})

source(here::here("scripts", "functions", "analysis_helpers.R"))

d <- readRDS(here::here("data", "d.rds"))

# =============================================================================
# DV specifications
# =============================================================================

dv_specs <- tribble(
  ~change_dv,             ~pre_dv,             ~label,                    ~text_treatment,
  "irs_approval_change",  "irs_approval_pre",  "IRS Approval Composite",  "Lewis",
  "civil_service_change", "civil_service_pre", "Civil Service Composite", "Lewis",
  "doge_change",          "doge_pre_1",        "DOGE Approval",           "Lewis",
  "trump_change",         "trump_pre_1",       "Trump Approval",          "Lewis",
  "enforce_change",       "enforce_pre_1",     "IRS Enforcement",         "Lewis",
  "fav_ssa_change",       "fav_ssa_pre_1",     "SSA Agents Favorability", "Lewis",
  "mvs_change",           "mvs_pre",           "Material Values Scale",   "Haidt"
)

# =============================================================================
# Moderator set and labels
# =============================================================================

moderators <- c(
  "gender_recoded", "age", "educ_cat_clean", "income_cat_clean",
  "race_white_fct", "strong_republican", "strong_conservative"
)

moderator_labels <- c(
  gender_recoded      = "Gender",
  age                 = "Age",
  educ_cat_clean      = "Education (categorical)",
  income_cat_clean    = "Household Income",
  race_white_fct      = "Race (White vs Non-White)",
  strong_republican   = "Strong Republican",
  strong_conservative = "Strong Conservative"
)

# =============================================================================
# Core heterogeneity test function
# =============================================================================

estimate_het_one_mod <- function(data, change_dv, pre_dv, text_treatment, moderator) {
  d_mod <- data %>%
    filter(
      !is.na(.data[[change_dv]]),
      !is.na(.data[[pre_dv]]),
      !is.na(.data[[moderator]])
    ) %>%
    mutate(
      .outcome = .data[[change_dv]],
      .treat = code_text_treat(text, text_treatment),
      .pre_z = as.numeric(scale(.data[[pre_dv]]))
    )

  is_numeric_mod <- is.numeric(d_mod[[moderator]])

  if (is_numeric_mod) {
    d_mod$.mod <- as.numeric(d_mod[[moderator]])
  } else {
    d_mod$.mod <- factor(d_mod[[moderator]])
  }

  het_model <- lm_robust(.outcome ~ .treat * .mod + .pre_z, data = d_mod)

  int_terms <- grep("^\\.treat:\\.mod", names(coef(het_model)), value = TRUE)

  if (length(int_terms) > 0) {
    beta_int <- coef(het_model)[int_terms]
    V_int <- vcov(het_model)[int_terms, int_terms, drop = FALSE]
    wald_stat <- tryCatch(
      as.numeric(t(beta_int) %*% solve(V_int) %*% beta_int),
      error = function(e) NA_real_
    )
    df1 <- length(int_terms)
    df2 <- het_model$df.residual
    f_stat <- wald_stat / df1
    het_p <- pf(f_stat, df1, df2, lower.tail = FALSE)
  } else {
    f_stat <- NA_real_; df1 <- NA_integer_; df2 <- NA_integer_; het_p <- NA_real_
  }

  tibble(
    moderator = moderator,
    mod_label = moderator_labels[moderator],
    f_statistic = f_stat, df1 = df1, df2 = df2, p_value = het_p
  )
}

# =============================================================================
# Run all DVs x moderators
# =============================================================================

cat("\n=== Treatment-Effect Heterogeneity by Demographics ===\n\n")

all_het_tests <- list()

for (dv_idx in seq_len(nrow(dv_specs))) {
  spec <- dv_specs[dv_idx, ]

  dv_tests <- map_dfr(moderators, function(mod) {
    estimate_het_one_mod(d, spec$change_dv, spec$pre_dv, spec$text_treatment, mod)
  }) %>%
    mutate(dv = spec$label, .before = 1)

  dv_tests$p_holm <- p.adjust(dv_tests$p_value, method = "holm")
  all_het_tests[[spec$label]] <- dv_tests
}

het_summary <- bind_rows(all_het_tests)

# =============================================================================
# Summary table
# =============================================================================

summary_table <- het_summary %>%
  transmute(
    DV = dv,
    Moderator = mod_label,
    F = sprintf("%.3f", f_statistic),
    df1, df2,
    p = format.pval(p_value, digits = 5),
    p_holm = format.pval(p_holm, digits = 5),
    sig = sigstars(p_holm)
  )

print(as.data.frame(summary_table), row.names = FALSE)

sig_count <- sum(het_summary$p_holm < 0.05, na.rm = TRUE)
if (sig_count > 0) {
  cat(sprintf("\n%d significant after Holm correction (p_holm < 0.05).\n", sig_count))
} else {
  cat("\nNo significant heterogeneity after Holm correction.\n")
}

fs::dir_create(here::here("output", "tables"))
readr::write_csv(het_summary, here::here("output", "tables", "heterogeneity-demographics-t1.csv"))
readr::write_csv(summary_table, here::here("output", "tables", "heterogeneity-demographics-t1-summary.csv"))

# Paper table helpers: Lewis/Cyber Sleuth outcomes only (Material Values in text).
dv_pub_t1 <- c(
  "IRS Approval Composite" = "IRS Favorability",
  "Civil Service Composite" = "Civil Service Favorability",
  "DOGE Approval" = "DOGE Disapproval",
  "Trump Approval" = "Trump Disapproval",
  "IRS Enforcement" = "IRS Funding Support",
  "SSA Agents Favorability" = "SSA Agent Favorability"
)
dv_pub_t2 <- c(
  "IRS Approval Composite (FU)" = "IRS Favorability",
  "Civil Service Composite (FU)" = "Civil Service Favorability",
  "DOGE Approval (FU)" = "DOGE Disapproval",
  "Trump Approval (FU)" = "Trump Disapproval",
  "IRS Enforcement (FU)" = "IRS Funding Support",
  "SSA Agents Favorability (FU)" = "SSA Agent Favorability"
)
mod_short <- c(
  "Gender" = "Gender",
  "Age" = "Age",
  "Education (categorical)" = "Education",
  "Household Income" = "Income",
  "Race (White vs Non-White)" = "Race",
  "Strong Republican" = "\\shortstack{Partisan\\\\identification}",
  "Strong Conservative" = "\\shortstack{Political\\\\ideology}"
)
mod_cols <- unname(mod_short)

wide_holm_from <- function(het_df, dv_pub) {
  wide <- het_df |>
    filter(dv %in% names(dv_pub)) |>
    mutate(
      DV = dplyr::recode(dv, !!!dv_pub),
      Moderator = dplyr::recode(mod_label, !!!mod_short),
      cell = ifelse(
        is.na(p_holm),
        "---",
        sprintf("%.3f%s", p_holm, sigstars(p_holm, tex = TRUE))
      )
    ) |>
    select(DV, Moderator, cell) |>
    tidyr::pivot_wider(names_from = Moderator, values_from = cell) |>
    mutate(DV = factor(DV, levels = unname(dv_pub))) |>
    arrange(DV)
  wide
}

row_tex <- function(wide_df) {
  purrr::map_chr(seq_len(nrow(wide_df)), function(i) {
    cells <- vapply(mod_cols, function(m) as.character(wide_df[[m]][[i]]), character(1))
    sprintf("   %s & %s \\\\", as.character(wide_df$DV[[i]]), paste(cells, collapse = " & "))
  })
}

n_t1 <- nrow(d)
wide_t1 <- wide_holm_from(het_summary, dv_pub_t1)

# =============================================================================
# Part 2: Follow-up (T2 − pre) heterogeneity
# =============================================================================

fu_path <- here::here("data", "d_with_followup.rds")
het_summary_t2 <- NULL
n_t2 <- NA_integer_

if (file.exists(fu_path)) {
  d_fu <- readRDS(fu_path) |>
    dplyr::filter(!is.na(.data$ResponseId_fu), .data$fu_completed_outcomes == 1L)
  n_t2 <- nrow(d_fu)

  dv_specs_t2 <- tribble(
    ~change_dv,                ~pre_dv,             ~label,                          ~text_treatment,
    "irs_change_t2",           "irs_approval_pre",  "IRS Approval Composite (FU)",   "Lewis",
    "civil_service_change_t2", "civil_service_pre", "Civil Service Composite (FU)",  "Lewis",
    "doge_change_t2",          "doge_pre_1",        "DOGE Approval (FU)",            "Lewis",
    "trump_change_t2",         "trump_pre_1",       "Trump Approval (FU)",           "Lewis",
    "enforce_change_t2",       "enforce_pre_1",     "IRS Enforcement (FU)",          "Lewis",
    "fav_ssa_change_t2",       "fav_ssa_pre_1",     "SSA Agents Favorability (FU)",  "Lewis",
    "mvs_change_t2",           "mvs_pre",           "Material Values Scale (FU)",    "Haidt"
  )

  cat("\n=== Treatment-Effect Heterogeneity by Demographics (follow-up) ===\n\n")

  all_het_tests_t2 <- list()

  for (dv_idx in seq_len(nrow(dv_specs_t2))) {
    spec <- dv_specs_t2[dv_idx, ]

    dv_tests <- map_dfr(moderators, function(mod) {
      estimate_het_one_mod(d_fu, spec$change_dv, spec$pre_dv, spec$text_treatment, mod)
    }) %>%
      mutate(dv = spec$label, .before = 1)

    dv_tests$p_holm <- p.adjust(dv_tests$p_value, method = "holm")
    all_het_tests_t2[[spec$label]] <- dv_tests
  }

  het_summary_t2 <- bind_rows(all_het_tests_t2)

  summary_table_t2 <- het_summary_t2 %>%
    transmute(
      DV = dv,
      Moderator = mod_label,
      F = sprintf("%.3f", f_statistic),
      df1, df2,
      p = format.pval(p_value, digits = 5),
      p_holm = format.pval(p_holm, digits = 5),
      sig = sigstars(p_holm)
    )

  print(as.data.frame(summary_table_t2), row.names = FALSE)

  sig_count_t2 <- sum(het_summary_t2$p_holm < 0.05, na.rm = TRUE)
  if (sig_count_t2 > 0) {
    cat(sprintf("\n%d significant after Holm correction (p_holm < 0.05).\n", sig_count_t2))
  } else {
    cat("\nNo significant heterogeneity after Holm correction.\n")
  }

  readr::write_csv(het_summary_t2, here::here("output", "tables", "heterogeneity-demographics-t2.csv"))
  readr::write_csv(summary_table_t2, here::here("output", "tables", "heterogeneity-demographics-t2-summary.csv"))
  cat("Saved: output/tables/heterogeneity-demographics-t2.csv\n")
} else {
  cat("\nSkipped follow-up heterogeneity: data/d_with_followup.rds not found.\n")
}

# Combined T1 + T2 LaTeX table for the paper
ncol_tab <- length(mod_cols) + 1L
tex_lines <- c(
  "\\begin{table}[!htbp]",
  "  \\centering",
  "  \\caption{Treatment-effect moderation (Holm-adjusted $p$ values)}",
  "  \\label{tab:het-demographics}",
  "  \\footnotesize",
  "  \\setlength{\\tabcolsep}{3.5pt}",
  paste0("  \\begin{tabular}{@{}l", paste(rep("c", length(mod_cols)), collapse = ""), "@{}}"),
  "  \\toprule",
  paste0("  Outcome & ", paste(mod_cols, collapse = " & "), " \\\\"),
  "  \\midrule",
  sprintf(
    "  \\multicolumn{%d}{@{}l}{\\textbf{Immediately post-treatment}} \\\\",
    ncol_tab
  ),
  row_tex(wide_t1)
)

if (!is.null(het_summary_t2)) {
  wide_t2 <- wide_holm_from(het_summary_t2, dv_pub_t2)
  tex_lines <- c(
    tex_lines,
    "  \\midrule",
    sprintf(
      "  \\multicolumn{%d}{@{}l}{\\textbf{At 2-month follow-up}} \\\\",
      ncol_tab
    ),
    row_tex(wide_t2)
  )
  note_n <- sprintf(
    "Immediately post-treatment uses completers ($N = %d$) and 2-month follow-up uses returners ($N = %d$). ",
    n_t1, n_t2
  )
} else {
  note_n <- sprintf("Immediately post-treatment uses completers ($N = %d$). ", n_t1)
}

tex_lines <- c(
  tex_lines,
  "  \\bottomrule",
  "  \\end{tabular}",
  "  \\vspace{2pt}",
  "  \\begin{minipage}{0.98\\linewidth}",
  paste0(
    "    \\footnotesize\\textit{Note.} Entries are Holm-adjusted $p$ values for the text $\\times$ moderator ",
    "interaction from OLS models of change scores. ",
    note_n,
    "Correction is applied across the seven moderators within each outcome and time period. ",
    "$^{\\dagger}p < .10$, *$p < .05$, **$p < .01$, ***$p < .001$."
  ),
  "  \\end{minipage}",
  "\\end{table}",
  ""
)
writeLines(tex_lines, here::here("output", "tables", "te-heterogeneity.tex"))
cat("Saved: output/tables/heterogeneity-demographics-t1.csv\n")
cat("Saved: output/tables/te-heterogeneity.tex\n")

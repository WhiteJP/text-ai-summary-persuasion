# =============================================================================
# 08 · Robustness Checks: Baseline Carry-Forward
# =============================================================================
# Completers vs BCF on ITT (paper table):
#   1. T1 completers vs T1 BCF (ITT: change = 0 for wave-1 attriters)
#   2. T2 completers vs T2 BCF (ITT: change = 0 if no follow-up)
#
# Cohen's d uses SD(pre) from wave-1 completers `d` and ate_flip from
# forest_measure_specs() (same as Figures 1–2). Completers BF01s are the
# files written by 05/06 (bf01_t1.csv / bf01_t2.csv).
#
# Writes:
#   output/tables/ate-bcf-robustness.tex
#   output/tables/ate-bcf-robustness.csv
#   output/tables/ate-bcf-bf01-long.csv

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(BayesFactor)
  library(tidyr)
  library(readr)
})

source(here::here("scripts", "functions", "forest_measure_specs.R"))
source(here::here("scripts", "functions", "forest_plot_estimates.R"))
source(here::here("scripts", "functions", "analysis_helpers.R"))

d_itt <- readRDS(here::here("data", "d_itt.rds"))
d     <- readRDS(here::here("data", "d.rds"))

# Lewis / Cyber Sleuth outcomes only (Material Values reported in text).
spec_forest <- forest_measure_specs()
table_specs <- spec_forest |>
  filter(.data$facet == "lewis") |>
  transmute(
    change_dv = .data$change_dv_t1,
    pre_dv = .data$pre_dv,
    label = .data$label,
    text_treatment = .data$text_treatment,
    ate_flip = as.integer(.data$ate_flip)
  )
fu_specs <- spec_forest |>
  filter(.data$facet == "lewis") |>
  transmute(
    change_dv = .data$change_dv_t2,
    pre_dv = .data$pre_dv,
    text_treatment = .data$text_treatment,
    label = .data$label,
    ate_flip = as.integer(.data$ate_flip)
  )

fmt_d_ci_tex <- function(d, lo, hi, p) {
  sprintf("%.2f [%.2f, %.2f]%s", d, lo, hi, sigstars(p, tex = TRUE))
}
fmt_bf <- function(bf) sprintf("%.2f", bf)

run_coef_table <- function(data, spec_df) {
  pmap_dfr(spec_df, function(change_dv, pre_dv, label, text_treatment, ate_flip) {
    a <- persuasion_analysis(
      change_dv, pre_dv, data, label = "",
      text_treatment, ref_data = d
    )
    t_ate <- orient_coef(a$text_coef, ate_flip, a$pooled_sd)
    t_int <- orient_coef(a$interaction_coef, ate_flip, a$pooled_sd)
    bind_rows(
      tibble::tibble(
        DV = label, effect = "ATE",
        d = t_ate$d, d_ci_low = t_ate$d_lo, d_ci_high = t_ate$d_hi,
        p.value = t_ate$p,
        cell = fmt_d_ci_tex(t_ate$d, t_ate$d_lo, t_ate$d_hi, t_ate$p)
      ),
      tibble::tibble(
        DV = label, effect = "Interaction",
        d = t_int$d, d_ci_low = t_int$d_lo, d_ci_high = t_int$d_hi,
        p.value = t_int$p,
        cell = fmt_d_ci_tex(t_int$d, t_int$d_lo, t_int$d_hi, t_int$p)
      )
    )
  })
}

run_bf_table <- function(data, spec_df, seed_base) {
  map_dfr(seq_len(nrow(spec_df)), function(i) {
    row <- spec_df[i, ]
    cat(sprintf("  BF %s ...\n", row$label))
    bf <- compute_bf_with_pre(
      row$change_dv, row$pre_dv, row$text_treatment, data,
      seed = seed_base + i
    )
    tibble::tibble(DV = row$label, bf_01 = bf, cell = fmt_bf(bf))
  })
}

comp_d <- run_coef_table(d, table_specs)

d_cf_tbl <- d_itt
for (cv in table_specs$change_dv) {
  d_cf_tbl[[cv]][!(d_cf_tbl$Finished %in% "1")] <- 0
}
t1_bcf_d <- run_coef_table(d_cf_tbl, table_specs)

d_wfu <- readRDS(here::here("data", "d_with_followup.rds"))
d_fu <- d_wfu |>
  filter(!is.na(ResponseId_fu), fu_completed_outcomes == 1L)
t2_comp_d <- run_coef_table(d_fu, fu_specs)

d_itt_wfu <- readRDS(here::here("data", "d_itt_with_followup.rds"))

d_bcf_t2_itt <- d_itt_wfu
no_t2 <- is.na(d_bcf_t2_itt$ResponseId_fu) |
  !(d_bcf_t2_itt$Finished %in% "1") |
  !(d_bcf_t2_itt$fu_completed_outcomes %in% 1L)
for (cv in fu_specs$change_dv) {
  d_bcf_t2_itt[[cv]][no_t2] <- 0
}
t2_itt_d <- run_coef_table(d_bcf_t2_itt, fu_specs)

wide_from_freq <- function(effect_name) {
  tibble::tibble(DV = table_specs$label) |>
    left_join(
      comp_d |> filter(effect == effect_name) |>
        select(DV, `Completers (T1)` = cell),
      by = "DV"
    ) |>
    left_join(
      t1_bcf_d |> filter(effect == effect_name) |> select(DV, `T1 BCF (ITT)` = cell),
      by = "DV"
    ) |>
    left_join(
      t2_comp_d |> filter(effect == effect_name) |>
        select(DV, `Completers (T2)` = cell),
      by = "DV"
    ) |>
    left_join(
      t2_itt_d |> filter(effect == effect_name) |> select(DV, `T2 BCF (ITT)` = cell),
      by = "DV"
    )
}

ate_wide <- wide_from_freq("ATE")
int_wide <- wide_from_freq("Interaction")

cat("\n=== Bayes factors (medium JZS; BF01 favors format equivalence) ===\n")
bf_t1_path <- here::here("output", "tables", "bf01_t1.csv")
bf_t2_path <- here::here("output", "tables", "bf01_t2.csv")
if (!file.exists(bf_t1_path) || !file.exists(bf_t2_path)) {
  stop(
    "Missing bf01_t1.csv / bf01_t2.csv. Run scripts/05_t1_analyses.R and ",
    "scripts/06_t2_analyses.R first (same forest_measure_specs as Figure 3).",
    call. = FALSE
  )
}
cat("Completers (T1): using output/tables/bf01_t1.csv (matches Figure 3)\n")
bf_comp <- read_csv(bf_t1_path, show_col_types = FALSE) |>
  transmute(DV = label, bf_01 = bf_01, cell = fmt_bf(bf_01)) |>
  filter(DV %in% table_specs$label)
cat("T1 BCF:\n")
bf_t1 <- run_bf_table(d_cf_tbl, table_specs, 88201L)
cat("Completers (T2): using output/tables/bf01_t2.csv\n")
bf_t2comp <- read_csv(bf_t2_path, show_col_types = FALSE) |>
  transmute(DV = label, bf_01 = bf_01, cell = fmt_bf(bf_01)) |>
  filter(DV %in% table_specs$label)
cat("T2 BCF ITT:\n")
bf_t2itt <- run_bf_table(d_bcf_t2_itt, fu_specs, 88401L)

bf_wide <- tibble::tibble(DV = table_specs$label) |>
  left_join(bf_comp |> select(DV, `Completers (T1)` = cell), by = "DV") |>
  left_join(bf_t1 |> select(DV, `T1 BCF (ITT)` = cell), by = "DV") |>
  left_join(bf_t2comp |> select(DV, `Completers (T2)` = cell), by = "DV") |>
  left_join(bf_t2itt |> select(DV, `T2 BCF (ITT)` = cell), by = "DV")

bf_long <- bind_rows(
  bf_comp |> mutate(scenario = "Completers (T1)"),
  bf_t1 |> mutate(scenario = "T1 BCF (ITT)"),
  bf_t2comp |> mutate(scenario = "Completers (T2)"),
  bf_t2itt |> mutate(scenario = "T2 BCF (ITT)")
)

n_comp <- nrow(d)
n_itt <- nrow(d_itt)
n_fu <- nrow(d_fu)

row_tex <- function(wide_df) {
  purrr::map_chr(seq_len(nrow(wide_df)), function(i) {
    sprintf(
      "   %s & %s & %s & %s & %s \\\\",
      wide_df$DV[[i]],
      wide_df$`Completers (T1)`[[i]],
      wide_df$`T1 BCF (ITT)`[[i]],
      wide_df$`Completers (T2)`[[i]],
      wide_df$`T2 BCF (ITT)`[[i]]
    )
  })
}

col_heads <- sprintf(
  paste0(
    "  Outcome & \\shortstack{T1 Completers\\\\($N = %d$)} & ",
    "\\shortstack{T1 BCF\\\\(ITT, $N = %d$)} & ",
    "\\shortstack{T2 Completers\\\\($N = %d$)} & ",
    "\\shortstack{T2 BCF\\\\(ITT, $N = %d$)} \\\\"
  ),
  n_comp, n_itt, n_fu, n_itt
)

note <- sprintf(
  paste0(
    "    \\footnotesize\\textit{Note.} Overall ATE and text $\\times$ format interactions are Cohen's $d$ (95\\%% CIs), ",
    "standardized by pooled pre-treatment SD. Overall ATE is the text effect averaged across formats, where ",
    "positive values indicate change in the predicted direction of persuasion. Text $\\times$ format interactions ",
    "are the format contrast in that persuasion-oriented effect (positive = larger persuasive effects for full ",
    "text than the AI summary). Bayes factors compare an additive model (no text $\\times$ format interaction) ",
    "to a full interaction model on the same change scores (medium JZS priors); values $> 3$ are moderate ",
    "evidence for format equivalence, values $> 1$ are anecdotal evidence. Baseline carried forward (BCF) ",
    "analyses impute change $= 0$ for missing scores from ITT sample. T1 Completers and T2 Completers restate the results from the main analysis. ",
    "$^{\\dagger}p < .10$, *$p < .05$, **$p < .01$, ***$p < .001$."
  )
)

tex <- c(
  "\\begin{table}[!htbp]",
  "  \\centering",
  paste0(
    "  \\caption{Robustness to differential attrition using baseline-carried-forward (BCF) imputation.}"
  ),
  "  \\label{tab:robust-ate}",
  "  \\footnotesize",
  "  \\setlength{\\tabcolsep}{3.5pt}",
  "  \\begin{tabular}{@{}lcccc@{}}",
  "  \\toprule",
  col_heads,
  "  \\midrule",
  "  \\multicolumn{5}{@{}l}{\\textbf{Overall ATE (averaged across formats)}} \\\\",
  row_tex(ate_wide),
  "  \\midrule",
  paste0(
    "  \\multicolumn{5}{@{}l}{\\textbf{Full text ATE $-$ AI summary ATE ",
    "(text $\\times$ format interaction)}} \\\\"
  ),
  row_tex(int_wide),
  "  \\midrule",
  "  \\multicolumn{5}{@{}l}{\\textbf{Bayes factor for format equivalence ($BF_{01}$)}} \\\\",
  row_tex(bf_wide),
  "  \\bottomrule",
  "  \\end{tabular}",
  "  \\vspace{2pt}",
  "  \\begin{minipage}{0.98\\linewidth}",
  note,
  "  \\end{minipage}",
  "\\end{table}",
  ""
)

fs::dir_create(here::here("output", "tables"))
out_tex <- here::here("output", "tables", "ate-bcf-robustness.tex")
out_csv <- here::here("output", "tables", "ate-bcf-robustness.csv")
out_bf  <- here::here("output", "tables", "ate-bcf-bf01-long.csv")

writeLines(tex, out_tex)
write_csv(bf_long, out_bf)
write_csv(
  bind_rows(
    ate_wide |> mutate(effect = "ATE", .before = 1),
    int_wide |> mutate(effect = "Interaction", .before = 1),
    bf_wide |> mutate(effect = "BF01", .before = 1)
  ),
  out_csv
)

cat("\n=== Overall ATE ===\n")
print(as.data.frame(ate_wide), row.names = FALSE)
cat("\n=== Full text ATE - AI summary ATE ===\n")
print(as.data.frame(int_wide), row.names = FALSE)
cat("\n=== BF01 (>1 favors format equivalence; >3 moderate) ===\n")
print(as.data.frame(
  bf_long |>
    select(scenario, DV, bf_01) |>
    mutate(bf_01 = round(bf_01, 2)) |>
    tidyr::pivot_wider(names_from = scenario, values_from = bf_01)
), row.names = FALSE)
cat("\nNs: Completers (T1)=", n_comp,
    " Completers (T2)=", n_fu,
    " ITT=", n_itt, "\n", sep = "")
cat("\nSaved:\n  ", out_tex, "\n  ", out_csv, "\n  ", out_bf, "\n", sep = "")

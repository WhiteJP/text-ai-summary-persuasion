# =============================================================================
# 04 · Comprehension Checks & Reading Time
# =============================================================================
# T1: 2-way ANOVAs (text * format) for comprehension and reading time.
# T2 (conditional on follow-up data): T2-only ANOVA, T1→T2 change (ANCOVA +
#     paired t-tests), open-ended recall descriptives.

suppressPackageStartupMessages({
  library(dplyr)
  library(broom)
  library(car)
})

d <- readRDS(here::here("data", "d.rds"))

# =============================================================================
# PART A: ORIGINAL SAMPLE (T1)
# =============================================================================

cat("\n=== PART A: ORIGINAL SAMPLE (T1) ===\n")

# =============================================================================
# Comprehension (T1)
# =============================================================================

cat("\n=== Comprehension Checks (T1) ===\n\n")

# Descriptives by text
comp_by_text <- d |>
  group_by(text) |>
  summarise(
    n = n(),
    M = mean(comp_total, na.rm = TRUE),
    SD = sd(comp_total, na.rm = TRUE),
    .groups = "drop"
  )
cat("Comprehension by text (out of 3):\n")
print(as.data.frame(comp_by_text |> mutate(across(c(M, SD), ~ round(.x, 2)))))

# Descriptives by format
comp_by_format <- d |>
  group_by(format) |>
  summarise(
    n = n(),
    M = mean(comp_total, na.rm = TRUE),
    SD = sd(comp_total, na.rm = TRUE),
    .groups = "drop"
  )
cat("\nComprehension by format:\n")
print(as.data.frame(comp_by_format |> mutate(across(c(M, SD), ~ round(.x, 2)))))

# Type III ANOVA
cat("\n2-way ANOVA (Type III): comp_total ~ text * format\n")
comp_lm <- lm(
  comp_total ~ text * format,
  data = d,
  contrasts = list(text = contr.sum, format = contr.sum)
)
print(car::Anova(comp_lm, type = 3))

# =============================================================================
# Reading Time (T1)
# =============================================================================

cat("\n\n=== Reading Time ===\n\n")

d <- d |> mutate(text_total_time_min = text_total_time / 60)

# Descriptives by text x format
format_time <- function(seconds) {
  mins <- floor(seconds / 60)
  secs <- round(seconds %% 60)
  if (mins > 0) sprintf("%dm %ds", mins, secs) else sprintf("%ds", secs)
}

time_by_cond <- d |>
  group_by(text, format) |>
  summarise(
    n        = n(),
    mean_sec = mean(text_total_time, na.rm = TRUE),
    sd_sec   = sd(text_total_time, na.rm = TRUE),
    .groups  = "drop"
  ) |>
  mutate(
    mean_formatted = sapply(mean_sec, format_time),
    sd_formatted   = sapply(sd_sec, format_time)
  )

cat("Reading time by condition:\n")
print(as.data.frame(time_by_cond |>
  select(text, format, n, mean_formatted, sd_formatted)))

# Overall Full vs Summary means (reported in paper)
overall_format <- d |>
  group_by(format) |>
  summarise(mean_sec = mean(text_total_time, na.rm = TRUE), .groups = "drop") |>
  mutate(mean_formatted = sapply(mean_sec, format_time))
cat("\nOverall reading time by format:\n")
print(as.data.frame(overall_format))

# Type III ANOVA
cat("\n2-way ANOVA (Type III): text_total_time ~ text * format\n")
time_lm <- lm(
  text_total_time ~ text * format,
  data = d,
  contrasts = list(text = contr.sum, format = contr.sum)
)
print(car::Anova(time_lm, type = 3))

# =============================================================================
# PART B: FOLLOW-UP SAMPLE (T2)
# =============================================================================
# Conditional on d_with_followup.rds (produced by 01b). All T2 derived columns
# (comp_fu_total, comp_change, etc.) are pre-computed by add_t2_derived().

fu_path <- here::here("data", "d_with_followup.rds")

if (!file.exists(fu_path)) {
  cat("\n\nSkipped Part B (T2 comprehension): data/d_with_followup.rds not found.\n")
} else {

  cat("\n\n=== PART B: FOLLOW-UP SAMPLE (T2) ===\n")
  d_fu <- readRDS(fu_path) |>
    filter(!is.na(.data$ResponseId_fu), .data$fu_completed_outcomes == 1L) |>
    distinct(PROLIFIC_PID, .keep_all = TRUE)

  # --- T2-only comprehension (Type III ANOVA) --------------------------------

  cat("\n\n=== Follow-up comprehension (T2 only): Type III ANOVA ===\n\n")

  d_fu_comp <- d_fu |> filter(!is.na(comp_fu_total))

  if (nrow(d_fu_comp) < 20L) {
    cat("Insufficient non-missing comp_fu_total for ANOVA.\n")
  } else {
    comp_mod <- lm(
      comp_fu_total ~ text * format,
      data = d_fu_comp,
      contrasts = list(text = contr.sum, format = contr.sum)
    )
    print(car::Anova(comp_mod, type = 3))
    cat("\nCell means (0–3 correct):\n")
    print(
      as.data.frame(
        d_fu_comp |>
          group_by(text, format) |>
          summarise(mean_comp = mean(comp_fu_total), n = dplyr::n(), .groups = "drop")
      ),
      row.names = FALSE
    )
  }

  # --- Comprehension change (T1 → T2): ANCOVA + paired t-tests --------------

  cat("\n\n=== Comprehension: T1 (wave-1 post) vs T2 (follow-up) ===\n\n")

  d_comp_cc <- d_fu |>
    filter(!is.na(.data$comp_total), !is.na(.data$comp_fu_total))

  if (nrow(d_comp_cc) < 20L) {
    cat("Insufficient paired comprehension data (need comp_total and comp_fu_total).\n")
  } else {
    fmt_ms <- function(m, s) sprintf("%.2f (%.2f)", m, s)

    cat("Overall (N = ", nrow(d_comp_cc), "):\n", sep = "")
    cat(
      "  T1: M(SD) = ", fmt_ms(mean(d_comp_cc$comp_total), sd(d_comp_cc$comp_total)), "\n",
      "  T2: M(SD) = ", fmt_ms(mean(d_comp_cc$comp_fu_total), sd(d_comp_cc$comp_fu_total)), "\n",
      "  Change (T2-T1): M(SD) = ",
      fmt_ms(mean(d_comp_cc$comp_change), sd(d_comp_cc$comp_change)), "\n\n",
      sep = ""
    )

    cat("By text x format (0-3 items correct; change = T2 - T1):\n")
    comp_desc <- d_comp_cc |>
      group_by(text, format) |>
      summarise(
        n = dplyr::n(),
        t1_m = mean(comp_total),
        t1_sd = sd(comp_total),
        t2_m = mean(comp_fu_total),
        t2_sd = sd(comp_fu_total),
        ch_m = mean(comp_change),
        ch_sd = sd(comp_change),
        .groups = "drop"
      ) |>
      mutate(
        T1 = sprintf("%.2f (%.2f)", t1_m, t1_sd),
        T2 = sprintf("%.2f (%.2f)", t2_m, t2_sd),
        Change = sprintf("%.2f (%.2f)", ch_m, ch_sd)
      ) |>
      select(text, format, n, T1, T2, Change)
    print(as.data.frame(comp_desc), row.names = FALSE)
    cat("\n")

    cat("ANCOVA: T2 comprehension ~ text * format + T1 comprehension (Type III)\n\n")
    comp_anc <- lm(
      comp_fu_total ~ text * format + comp_total,
      data = d_comp_cc,
      contrasts = list(text = contr.sum, format = contr.sum)
    )
    print(car::Anova(comp_anc, type = 3))
    cat("\n")

    cat("Dependent-samples t-tests (T2 vs T1; H0: mean change = 0)\n\n")

    paired_sub <- function(dat, label) {
      n <- nrow(dat)
      if (n < 3L) {
        cat("  ", label, ": n = ", n, " (skipped)\n", sep = "")
        return(invisible(NULL))
      }
      tt <- t.test(dat$comp_fu_total, dat$comp_total, paired = TRUE)
      ci_lo <- tt$conf.int[1]
      ci_hi <- tt$conf.int[2]
      cat(
        "  ", label, ": n = ", n,
        ", M_diff = ", sprintf("%.3f", as.numeric(tt$estimate)),
        ", t(", sprintf("%.0f", tt$parameter), ") = ", sprintf("%.3f", tt$statistic),
        ", p = ", sprintf("%.5f", tt$p.value),
        ", 95% CI [", sprintf("%.3f", ci_lo), ", ", sprintf("%.3f", ci_hi), "]\n",
        sep = ""
      )
    }

    cat("By text (book):\n")
    paired_sub(dplyr::filter(d_comp_cc, text == "Lewis"), "Lewis")
    paired_sub(dplyr::filter(d_comp_cc, text == "Haidt"), "Haidt")
    cat("\nBy format:\n")
    paired_sub(dplyr::filter(d_comp_cc, format == "Full"), "Full text")
    paired_sub(dplyr::filter(d_comp_cc, format == "Summary"), "AI summary")
    cat("\n")
  }

  # --- Open-ended recall -----------------------------------------------------

  cat("\n=== Open-ended recall (free_text_recall_fu) ===\n\n")
  if ("free_text_recall_fu" %in% names(d_fu)) {
    recall <- d_fu$free_text_recall_fu
    n_tot <- length(recall)
    non_empty <- !is.na(recall) & nzchar(trimws(as.character(recall)))
    wc <- nchar(trimws(as.character(recall[non_empty])))
    cat("N:", n_tot, "  nonempty:", sum(non_empty), "\n")
    if (length(wc)) {
      cat(sprintf(
        "Word-length (chars): Mdn=%.0f, M=%.1f, SD=%.1f\n",
        median(wc), mean(wc), sd(wc)
      ))
    }
  } else {
    cat("Column free_text_recall_fu not found.\n")
  }
}

# =============================================================================
# 03 · Attrition Analysis
# =============================================================================
# Tests for differential attrition (logistic regression: dropout ~ format * text)
# and reports how far attriters progressed through the text.

library(dplyr)
library(broom)

d_itt <- readRDS(here::here("data", "d_itt.rds"))
d     <- readRDS(here::here("data", "d.rds"))

# =============================================================================
# Attrition rates by condition
# =============================================================================

cat("\n=== Attrition by Condition ===\n\n")

attrition_summary <- d_itt |>
  group_by(condition) |>
  summarise(
    n_itt     = n(),
    n_finished = sum(Finished == "1", na.rm = TRUE),
    n_dropped  = n_itt - n_finished,
    attrition  = sprintf("%.1f%%", 100 * n_dropped / n_itt),
    .groups = "drop"
  )
print(as.data.frame(attrition_summary))

# =============================================================================
# Attrition by format and text (marginal)
# =============================================================================

d_itt_dropout <- d_itt |> mutate(dropped = as.integer(Finished != "1"))

cat("\n\n=== Attrition by Format ===\n\n")
format_attrition <- d_itt_dropout |>
  group_by(format) |>
  summarise(
    n_itt = n(), n_dropped = sum(dropped),
    attrition = sprintf("%.1f%%", 100 * mean(dropped)),
    .groups = "drop"
  )
print(as.data.frame(format_attrition))

cat("\n=== Attrition by Text ===\n\n")
text_attrition <- d_itt_dropout |>
  group_by(text) |>
  summarise(
    n_itt = n(), n_dropped = sum(dropped),
    attrition = sprintf("%.1f%%", 100 * mean(dropped)),
    .groups = "drop"
  )
print(as.data.frame(text_attrition))

# =============================================================================
# Logistic regression: Dropout ~ Format * Text
# =============================================================================

cat("\n\n=== Logistic Regression: Dropout ~ Format * Text ===\n\n")

dropout_model <- glm(dropped ~ format * text, data = d_itt_dropout, family = binomial)
coef_stats <- tidy(dropout_model)

cat("Coefficients (logit scale):\n")
print(as.data.frame(coef_stats |>
  mutate(across(c(estimate, std.error, statistic, p.value), ~ round(.x, 3)))))

cat("\nReported in paper (z-statistics):\n")
fmt_coef <- coef_stats |> filter(term == "formatSummary")
cat(sprintf("  Format (Full vs Summary): z = %.3f, p = %.3f\n",
            fmt_coef$statistic, fmt_coef$p.value))

txt_coef <- coef_stats |> filter(term == "textHaidt")
cat(sprintf("  Text (Lewis vs Haidt):    z = %.3f, p = %.3f\n",
            txt_coef$statistic, txt_coef$p.value))

int_coef <- coef_stats |> filter(term == "formatSummary:textHaidt")
cat(sprintf("  Format x Text:            z = %.3f, p = %.3f\n",
            int_coef$statistic, int_coef$p.value))

# =============================================================================
# Attriter progress: how far into the text before dropping out
# =============================================================================

cat("\n\n=== Attriter Progress ===\n\n")

format_time <- function(seconds) {
  mins <- floor(seconds / 60)
  secs <- round(seconds %% 60)
  if (mins > 0) sprintf("%dm %ds", mins, secs) else sprintf("%ds", secs)
}

d_attriters <- d_itt |> filter(Finished != "1")
cat(sprintf("Total attriters: %d\n\n", nrow(d_attriters)))

if (nrow(d_attriters) > 0) {
  attriter_progress <- d_attriters |>
    group_by(text, format) |>
    summarise(
      n           = n(),
      pages_total = first(text_pages_total),
      pages_mean  = mean(text_pages_completed, na.rm = TRUE),
      pages_sd    = sd(text_pages_completed, na.rm = TRUE),
      time_mean   = mean(text_total_time, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      pages_summary = sprintf("%.1f / %d (SD = %.1f)", pages_mean, pages_total, pages_sd),
      time_summary  = sapply(time_mean, format_time)
    )

  print(as.data.frame(attriter_progress |>
    select(text, format, n, pages_summary, time_summary)))

  # Full-text attriters specifically (reported in paper)
  cat("\nFull-text attriters — mean pages completed:\n")
  full_attriters <- d_attriters |> filter(format == "Full")
  if (nrow(full_attriters) > 0) {
    full_progress <- full_attriters |>
      group_by(text) |>
      summarise(n = n(), mean_pages = mean(text_pages_completed, na.rm = TRUE),
                .groups = "drop")
    print(as.data.frame(full_progress))
  }
}

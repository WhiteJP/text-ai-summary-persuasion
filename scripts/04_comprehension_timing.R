# =============================================================================
# 04 · Comprehension Checks & Reading Time
# =============================================================================
# 2-way ANOVAs (text * format) for comprehension and reading time,
# with descriptive statistics, as reported in the paper.

library(dplyr)
library(broom)

d <- readRDS(here::here("data", "d.rds"))

# =============================================================================
# Comprehension
# =============================================================================

cat("\n=== Comprehension Checks ===\n\n")

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

# 2-way ANOVA
cat("\n2-way ANOVA: comp_total ~ text * format\n")
comp_anova <- aov(comp_total ~ text * format, data = d)
comp_tidy <- tidy(comp_anova) |> mutate(across(where(is.numeric), ~ round(.x, 3)))
print(as.data.frame(comp_tidy))

# =============================================================================
# Reading Time
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

# 2-way ANOVA
cat("\n2-way ANOVA: text_total_time ~ text * format\n")
time_anova <- aov(text_total_time ~ text * format, data = d)
time_tidy <- tidy(time_anova) |> mutate(across(where(is.numeric), ~ round(.x, 3)))
print(as.data.frame(time_tidy))

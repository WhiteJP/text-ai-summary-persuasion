# =============================================================================
# 02 · Sample Descriptives
# =============================================================================
# Reports sample flow, demographics (final analysis sample),
# and condition balance.

library(dplyr)
library(tidyr)

d_all <- readRDS(here::here("data", "d_all.rds"))
d_itt <- readRDS(here::here("data", "d_itt.rds"))
d     <- readRDS(here::here("data", "d.rds"))

# --- Sample Flow -------------------------------------------------------------

cat("\n=== Sample Flow ===\n\n")

d_passed_attn1 <- d_all |> filter(attncheck1_passed)
d_treated      <- d_all |> filter(treated)

sample_flow <- data.frame(
  Stage = c(
    "Started survey",
    "Passed attention check 1",
    "Received treatment",
    "ITT (treated + passed attn check 2)",
    "Finished survey (final sample)"
  ),
  N = c(nrow(d_all), nrow(d_passed_attn1), nrow(d_treated), nrow(d_itt), nrow(d))
)
sample_flow$Pct <- sprintf("%.1f%%", 100 * sample_flow$N / nrow(d_all))

print(sample_flow, row.names = FALSE)

# --- Demographics (final analysis sample) ------------------------------------

cat(sprintf("\n\n=== Demographics (Final Sample, N = %d) ===\n\n", nrow(d)))

cat("Gender:\n")
print(table(d$gender))

cat(sprintf(
  "\nAge: M = %.2f, SD = %.2f, range = %d - %d\n",
  mean(d$age, na.rm = TRUE), sd(d$age, na.rm = TRUE),
  min(d$age, na.rm = TRUE),  max(d$age, na.rm = TRUE)
))

cat(sprintf("White: %.2f%%\n", 100 * mean(d$race_white, na.rm = TRUE)))

cat("\nEducation:\n")
print(table(d$educ_cat))

cat("\nIncome:\n")
print(table(d$income_cat))

# --- Condition Balance (final sample) ----------------------------------------

cat("\n\n=== Condition Balance (Final Sample) ===\n\n")

condition_table <- d |>
  count(text, format) |>
  pivot_wider(names_from = format, values_from = n, values_fill = 0) |>
  mutate(Total = Full + Summary)
print(as.data.frame(condition_table))

balance_matrix <- d |>
  count(text, format) |>
  pivot_wider(names_from = format, values_from = n, values_fill = 0) |>
  tibble::column_to_rownames("text") |>
  as.matrix()

balance_test <- chisq.test(balance_matrix)
cat(sprintf(
  "\nChi-squared test for balance: X2(%d) = %.2f, p = %.3f\n",
  balance_test$parameter, balance_test$statistic, balance_test$p.value
))

# =============================================================================
# 02 · Sample Descriptives
# =============================================================================
# Part A — Original survey: sample flow, demographics (final analysis sample),
#   condition balance, and survey timing.
# Part B — Follow-up survey: sample flow, descriptives for follow-up starters
#   and follow-up finishers, condition balance.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

d_all <- readRDS(here::here("data", "d_all.rds"))
d_itt <- readRDS(here::here("data", "d_itt.rds"))
d     <- readRDS(here::here("data", "d.rds"))

# --- Helpers: Qualtrics session timing ---------------------------------------

cat_survey_timing <- function(df, title, start_var = "StartDate", end_var = "EndDate") {
  cat(sprintf("\n\n=== %s (N = %d) ===\n\n", title, nrow(df)))
  miss_start <- !start_var %in% names(df)
  miss_end <- !end_var %in% names(df)
  if (miss_start || miss_end) {
    cat(sprintf(
      "Skipped: column(s) not found — %s%s%s.\n",
      if (miss_start) start_var else "",
      if (miss_start && miss_end) ", " else "",
      if (miss_end) end_var else ""
    ))
    return(invisible(NULL))
  }
  sd <- df[[start_var]]
  ed <- df[[end_var]]
  if (all(is.na(sd))) {
    cat("All session start times are missing.\n")
    return(invisible(NULL))
  }
  cat(sprintf(
    "Session start — first: %s, last: %s\n",
    format(min(sd, na.rm = TRUE), usetz = FALSE),
    format(max(sd, na.rm = TRUE), usetz = FALSE)
  ))
  if (all(is.na(ed))) {
    cat("Session end — all missing.\n")
  } else {
    cat(sprintf(
      "Session end   — first: %s, last: %s\n",
      format(min(ed, na.rm = TRUE), usetz = FALSE),
      format(max(ed, na.rm = TRUE), usetz = FALSE)
    ))
  }
  tb <- table(as.Date(sd))
  ord <- order(as.Date(names(tb)))
  day_df <- data.frame(
    calendar_date = names(tb)[ord],
    n_sessions_started = as.integer(tb)[ord],
    stringsAsFactors = FALSE
  )
  cat("\nSessions started by calendar date:\n")
  print(day_df, row.names = FALSE)
  invisible(NULL)
}

# --- Demographics helper (reused for multiple samples) -----------------------

cat_demographics <- function(df, title) {
  cat(sprintf("\n\n=== %s (N = %d) ===\n\n", title, nrow(df)))

  cat("Gender:\n")
  print(table(df$gender))

  cat(sprintf(
    "\nAge: M = %.2f, SD = %.2f, range = %d - %d\n",
    mean(df$age, na.rm = TRUE), sd(df$age, na.rm = TRUE),
    min(df$age, na.rm = TRUE),  max(df$age, na.rm = TRUE)
  ))

  cat(sprintf("White: %.2f%%\n", 100 * mean(df$race_white, na.rm = TRUE)))

  cat("\nEducation:\n")
  print(table(df$educ_cat))

  cat("\nIncome:\n")
  print(table(df$income_cat))
}

# --- Condition balance helper ------------------------------------------------

cat_condition_balance <- function(df, title) {
  cat(sprintf("\n\n=== %s ===\n\n", title))

  condition_tbl <- df |>
    count(text, format) |>
    pivot_wider(names_from = format, values_from = n, values_fill = 0) |>
    mutate(Total = Full + Summary)
  print(as.data.frame(condition_tbl))

  balance_mat <- df |>
    count(text, format) |>
    pivot_wider(names_from = format, values_from = n, values_fill = 0) |>
    tibble::column_to_rownames("text") |>
    as.matrix()

  balance_test <- chisq.test(balance_mat)
  cat(sprintf(
    "\nChi-squared test for balance: X2(%d) = %.2f, p = %.5f\n",
    balance_test$parameter, balance_test$statistic, balance_test$p.value
  ))
}

cat("\n")
cat("\n=== PART A: ORIGINAL SURVEY ===\n")

# --- Sample Flow -------------------------------------------------------------

cat("\n=== Original Survey Sample Flow ===\n\n")

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

cat_survey_timing(d_itt, "Survey timing — Original survey (ITT sample)")

cat_demographics(d, "Demographics (Original Survey Final Sample)")

cat_condition_balance(d, "Condition Balance (Original Survey Final Sample)")

# --- Follow-up (requires data/d_*_with_followup.rds from 01b_link_followup_data.R) ---

d_with_fu_path <- here::here("data", "d_with_followup.rds")
d_itt_fu_path <- here::here("data", "d_itt_with_followup.rds")

if (!file.exists(d_with_fu_path) || !file.exists(d_itt_fu_path)) {
  cat("\n\n=== Follow-up sample descriptives ===\n\n")
  cat("Skipped: d_with_followup.rds and/or d_itt_with_followup.rds not found.\n")
  cat("Run scripts/01b_link_followup_data.R after original survey wrangling.\n")
} else {

  cat("\n\n=== PART B: FOLLOW-UP SURVEY ===\n")

  d_itt_with_fu <- readRDS(d_itt_fu_path)
  d_with_fu <- readRDS(d_with_fu_path)

  # Unique-PID versions for counting
  d_itt_uniq <- d_itt_with_fu |> distinct(PROLIFIC_PID, .keep_all = TRUE)
  d_comp_uniq <- d_with_fu |> distinct(PROLIFIC_PID, .keep_all = TRUE)

  n_itt       <- nrow(d_itt_uniq)
  n_itt_start <- sum(d_itt_uniq$fu_started)
  n_itt_fin   <- sum(d_itt_uniq$fu_finished)

  n_comp       <- nrow(d_comp_uniq)
  n_comp_start <- sum(d_comp_uniq$fu_started)
  n_comp_fin   <- sum(d_comp_uniq$fu_finished)

  cat("\n=== Follow-up Sample Flow ===\n\n")
  flow_fu <- data.frame(
    Stage = c(
      "ITT (original), unique participants",
      "  ... started follow-up",
      "  ... finished follow-up",
      "Original completers, unique participants",
      "  ... started follow-up",
      "  ... finished follow-up"
    ),
    N = c(n_itt, n_itt_start, n_itt_fin, n_comp, n_comp_start, n_comp_fin),
    Pct = c(
      "100.0%",
      sprintf("%.1f%%", 100 * n_itt_start / n_itt),
      sprintf("%.1f%%", 100 * n_itt_fin / n_itt),
      "100.0%",
      sprintf("%.1f%%", 100 * n_comp_start / n_comp),
      sprintf("%.1f%%", 100 * n_comp_fin / n_comp)
    )
  )
  print(flow_fu, row.names = FALSE)

  # --- Follow-up starters descriptives ----------------------------------------

  d_fu_starters <- d_with_fu |>
    filter(fu_started == 1L) |>
    distinct(PROLIFIC_PID, .keep_all = TRUE)

  cat_survey_timing(
    d_fu_starters,
    "Survey timing — Follow-up survey (starters)",
    start_var = "StartDate_fu",
    end_var = "EndDate_fu"
  )

  cat_demographics(d_fu_starters, "Demographics (Follow-up Starters)")
  cat_condition_balance(d_fu_starters, "Condition Balance (Follow-up Starters)")

  # --- Follow-up finishers descriptives ---------------------------------------

  d_fu_finishers <- d_with_fu |>
    filter(fu_finished == 1L) |>
    distinct(PROLIFIC_PID, .keep_all = TRUE)

  cat_survey_timing(
    d_fu_finishers,
    "Survey timing — Follow-up survey (finishers)",
    start_var = "StartDate_fu",
    end_var = "EndDate_fu"
  )

  cat_demographics(d_fu_finishers, "Demographics (Follow-up Finishers)")
  cat_condition_balance(d_fu_finishers, "Condition Balance (Follow-up Finishers)")

  # --- Time between original survey start and follow-up finish -----------------

  days_between <- as.numeric(
    difftime(d_fu_finishers$EndDate_fu, d_fu_finishers$StartDate, units = "days")
  )

  cat("\n\n=== Days Between Original Start and Follow-up Finish ===\n\n")
  print(summary(days_between))
}

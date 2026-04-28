# =============================================================================
# 03 · Attrition Analysis
# =============================================================================
# Part A — Original survey: differential attrition (logistic:
#   dropout ~ format * text) and how far attriters progressed.
# Part B — Follow-up survey: completion of the follow-up survey —
#   same factorial glm(fu_completed ~ format * text) from the ITT sample and
#   from original-survey completers (reference: Full × Lewis). Participants who
#   started the follow-up but did not finish are treated as non-completers.

suppressPackageStartupMessages({
  library(dplyr)
  library(broom)
})

d_itt <- readRDS(here::here("data", "d_itt.rds"))
d     <- readRDS(here::here("data", "d.rds"))

cat("\n=== PART A: ORIGINAL SURVEY ATTRITION ===\n")

# =============================================================================
# Attrition rates by condition
# =============================================================================

cat("\n=== Original Survey Attrition by Condition ===\n\n")

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

cat("\n\n=== Original Survey Attrition by Format ===\n\n")
format_attrition <- d_itt_dropout |>
  group_by(format) |>
  summarise(
    n_itt = n(), n_dropped = sum(dropped),
    attrition = sprintf("%.1f%%", 100 * mean(dropped)),
    .groups = "drop"
  )
print(as.data.frame(format_attrition))

cat("\n=== Original Survey Attrition by Text ===\n\n")
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

cat("\n\n=== Original Survey Logistic Regression: Dropout ~ Format * Text ===\n\n")

dropout_model <- glm(dropped ~ format * text, data = d_itt_dropout, family = binomial)
coef_stats <- tidy(dropout_model)

cat("Coefficients (logit scale):\n")
print(as.data.frame(coef_stats |>
  mutate(across(c(estimate, std.error, statistic), ~ round(.x, 3)),
         p.value = round(p.value, 5))))

cat("\nReported in paper (z-statistics):\n")
fmt_coef <- coef_stats |> filter(term == "formatSummary")
cat(sprintf("  Format (Summary vs Full): z = %.3f, p = %.5f\n",
            fmt_coef$statistic, fmt_coef$p.value))

txt_coef <- coef_stats |> filter(term == "textHaidt")
cat(sprintf("  Text (Haidt vs Lewis):    z = %.3f, p = %.5f\n",
            txt_coef$statistic, txt_coef$p.value))

int_coef <- coef_stats |> filter(term == "formatSummary:textHaidt")
cat(sprintf("  Format x Text:            z = %.3f, p = %.5f\n",
            int_coef$statistic, int_coef$p.value))

# =============================================================================
# Attriter progress: how far into the text before dropping out
# =============================================================================

cat("\n\n=== Original Survey Attriter Progress ===\n\n")

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

# =============================================================================
# Follow-up survey completion (requires 01b_link_followup_data.R)
# =============================================================================

summ_fu_attrition <- function(dat, title) {
  cat(title, "\n")
  dat <- dat |> distinct(PROLIFIC_PID, .keep_all = TRUE)

  rates <- dat |>
    group_by(text, format) |>
    summarise(
      n = n(),
      n_fu = sum(fu_completed),
      pct_fu = 100 * mean(fu_completed),
      .groups = "drop"
    )
  print(as.data.frame(rates), row.names = FALSE)

  g <- glm(fu_completed ~ format * text, data = dat, family = binomial)
  coef_fu <- tidy(g)

  cat("\nCoefficients (logit scale; reference: format = Full, text = Lewis):\n")
  print(as.data.frame(coef_fu |>
    mutate(across(c(estimate, std.error, statistic), ~ round(.x, 3)),
           p.value = round(p.value, 5))))

  cat("\n(z-statistics; parallel to original survey attrition above)\n")
  fmt_c <- coef_fu |> filter(term == "formatSummary")
  cat(sprintf("  Format (Summary vs Full): z = %.3f, p = %.5f\n",
              fmt_c$statistic, fmt_c$p.value))
  txt_c <- coef_fu |> filter(term == "textHaidt")
  cat(sprintf("  Text (Haidt vs Lewis):      z = %.3f, p = %.5f\n",
              txt_c$statistic, txt_c$p.value))
  int_c <- coef_fu |> filter(term == "formatSummary:textHaidt")
  cat(sprintf("  Format × Text:              z = %.3f, p = %.5f\n",
              int_c$statistic, int_c$p.value))

  invisible(g)
}

itt_fu_path <- here::here("data", "d_itt_with_followup.rds")
comp_fu_path <- here::here("data", "d_with_followup.rds")

if (!file.exists(itt_fu_path) || !file.exists(comp_fu_path)) {
  cat("\n\n=== Follow-up completion (logistic) ===\n\n")
  cat("Skipped: d_itt_with_followup.rds and/or d_with_followup.rds not found.\n")
  cat("Run scripts/01b_link_followup_data.R after original survey wrangling.\n")
} else {

  cat("\n\n=== PART B: FOLLOW-UP SURVEY ATTRITION ===\n")

  d_itt_fu <- readRDS(itt_fu_path)
  d_comp_fu <- readRDS(comp_fu_path)

  # --- Report started-but-not-finished before filtering them out --------------

  report_fu_incomplete <- function(dat, label) {
    dat <- dat |> distinct(PROLIFIC_PID, .keep_all = TRUE)
    n_total   <- nrow(dat)
    n_started <- sum(dat$fu_started)
    n_finished <- sum(dat$fu_finished)
    n_incomplete <- sum(dat$fu_started_not_finished)
    cat(sprintf("  %s: %d invited, %d started follow-up, %d finished, %d started but did not finish\n",
                label, n_total, n_started, n_finished, n_incomplete))
  }

  cat("\n=== Follow-up Survey Completion Overview ===\n\n")
  report_fu_incomplete(d_itt_fu, "ITT sample")
  report_fu_incomplete(d_comp_fu, "Original completers")

  # --- Within-followup attrition (among those who started) -------------------

  summ_within_fu_attrition <- function(dat, title) {
    cat(title, "\n")
    dat_started <- dat |>
      distinct(PROLIFIC_PID, .keep_all = TRUE) |>
      filter(fu_started == 1L) |>
      mutate(fu_dropped = as.integer(fu_finished == 0L))

    rates <- dat_started |>
      group_by(text, format) |>
      summarise(
        n_started  = n(),
        n_finished = sum(fu_finished),
        n_dropped  = sum(fu_dropped),
        attrition  = sprintf("%.1f%%", 100 * mean(fu_dropped)),
        .groups = "drop"
      )
    print(as.data.frame(rates), row.names = FALSE)

    cat(sprintf("\nTotal: %d started, %d finished, %d dropped (%.1f%%)\n",
                nrow(dat_started), sum(dat_started$fu_finished),
                sum(dat_started$fu_dropped),
                100 * mean(dat_started$fu_dropped)))

    if (sum(dat_started$fu_dropped) > 0 && length(unique(dat_started$fu_dropped)) > 1) {
      g <- glm(fu_dropped ~ format * text, data = dat_started, family = binomial)
      coef_fu <- tidy(g)

      cat("\nLogistic regression: Dropout ~ Format * Text (among follow-up starters)\n")
      cat("Coefficients (logit scale; reference: format = Full, text = Lewis):\n")
      print(as.data.frame(coef_fu |>
        mutate(across(c(estimate, std.error, statistic), ~ round(.x, 3)),
               p.value = round(p.value, 5))))

      fmt_c <- coef_fu |> filter(term == "formatSummary")
      cat(sprintf("\n  Format (Summary vs Full): z = %.3f, p = %.5f\n",
                  fmt_c$statistic, fmt_c$p.value))
      txt_c <- coef_fu |> filter(term == "textHaidt")
      cat(sprintf("  Text (Haidt vs Lewis):      z = %.3f, p = %.5f\n",
                  txt_c$statistic, txt_c$p.value))
      int_c <- coef_fu |> filter(term == "formatSummary:textHaidt")
      cat(sprintf("  Format × Text:              z = %.3f, p = %.5f\n",
                  int_c$statistic, int_c$p.value))
    } else {
      cat("\nNo within-followup attrition (all starters finished).\n")
    }
  }

  cat("\n=== Within-Followup Attrition (among those who started the follow-up) ===\n\n")

  summ_within_fu_attrition(
    d_comp_fu,
    "--- Original completers who started follow-up ---"
  )

  # --- Follow-up completion from original samples -----------------------------

  cat("\nParticipants who started the follow-up but did not finish are treated as\n",
      "non-completers (fu_completed = 0). Only those who finished count as complete.\n\n")

  d_itt_fu <- d_itt_fu |>
    mutate(fu_completed = fu_finished)

  d_comp_fu <- d_comp_fu |>
    mutate(fu_completed = fu_finished)

  cat("\n=== Follow-up Completion from Original Samples: Logistic Regression (format × text) ===\n\n")

  summ_fu_attrition(
    d_itt_fu,
    "--- ITT sample (original ITT; predict follow-up completion) ---"
  )

  cat("\n")
  summ_fu_attrition(
    d_comp_fu,
    "--- Original completers (predict follow-up completion) ---"
  )
}

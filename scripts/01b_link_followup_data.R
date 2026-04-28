# =============================================================================
# 01b · Link follow-up Qualtrics exports to wave-1 analysis datasets
# =============================================================================
# Expects two follow-up CSV exports in data/ whose names match
# followup*lewis*.csv and followup*haidt*.csv.
#
# Join key: PROLIFIC_PID (stable across Prolific and both survey waves).
# Follow-up item names reuse wave-1 *post* labels; all follow-up columns are
# suffixed with _fu before joining so they do not overwrite T1 measures.
#
# Writes: d_itt_with_followup.rds, d_with_followup.rds
# Linkage is validated against d (wave-1 completers); those are the participants
# invited to the follow-up.

suppressPackageStartupMessages({
  library(dplyr)
  library(here)
})

source(here("scripts", "functions", "read_csv_qualtrics.R"))
source(here("scripts", "functions", "validate_followup_exports.R"))
source(here("scripts", "functions", "add_t2_derived.R"))

paths_lewis <- Sys.glob(here("data", "followup*lewis*.csv"))
paths_haidt <- Sys.glob(here("data", "followup*[Hh]aidt*.csv"))

if (length(paths_lewis) < 1L || length(paths_haidt) < 1L) {
  stop(
    "Follow-up CSVs not found. Place both exports in data/ with names matching ",
    "followup*lewis*.csv and followup*haidt*.csv."
  )
}
if (length(paths_lewis) > 1L || length(paths_haidt) > 1L) {
  warning(
    "Multiple follow-up files matched; using first of each:\n  ",
    paths_lewis[1], "\n  ", paths_haidt[1],
    call. = FALSE,
    immediate. = TRUE
  )
}

fu_lewis_raw <- read_csv_qualtrics(paths_lewis[1], col_types = readr::cols())
fu_haidt_raw <- read_csv_qualtrics(paths_haidt[1], col_types = readr::cols())

rename_followup_block <- function(dat, survey_label) {
  lab <- survey_label
  dat |>
    mutate(followup_survey = lab, .before = 1L) |>
    rename_with(
      function(nm) paste0(nm, "_fu"),
      .cols = -dplyr::any_of(c("PROLIFIC_PID", "followup_survey", "ResponseId"))
    ) |>
    rename(ResponseId_fu = ResponseId)
}

fu_all <- bind_rows(
  rename_followup_block(fu_lewis_raw, "Lewis"),
  rename_followup_block(fu_haidt_raw, "Haidt")
)

# Count non-missing cells per row (character blanks count as missing). Used to
# resolve duplicate PROLIFIC_PID by keeping the most complete response.
add_n_nonmiss <- function(dat) {
  cols <- setdiff(names(dat), "PROLIFIC_PID")
  n_nonmiss <- vapply(seq_len(nrow(dat)), function(i) {
    row <- dat[i, cols, drop = FALSE]
    sum(vapply(row, function(x) {
      x <- x[[1L]]
      if (length(x) != 1L) return(FALSE)
      if (is.na(x)) return(FALSE)
      if (is.character(x) && !nzchar(trimws(as.character(x)))) return(FALSE)
      TRUE
    }, logical(1L)))
  }, integer(1L))
  dplyr::mutate(dat, n_nonmiss = n_nonmiss)
}

dup_pid <- fu_all |>
  count(PROLIFIC_PID, name = "n") |>
  filter(n > 1L)

if (nrow(dup_pid) > 0L) {
  fu_all <- add_n_nonmiss(fu_all)
  dup_ids <- sort(unique(dup_pid$PROLIFIC_PID))
  cat("\n")
  cat("========== Follow-up: duplicate PROLIFIC_PID ==========\n")
  cat(
    length(dup_ids),
    " participant ID(s) appear more than once in the combined follow-up export.\n",
    "Keeping one row per ID: the row with the most non-missing fields",
    " (ties: first row in combined Lewis-then-Haidt order).\n",
    sep = ""
  )
  for (pid in dup_ids) {
    sub <- fu_all |>
      dplyr::filter(.data$PROLIFIC_PID == pid) |>
      dplyr::arrange(dplyr::desc(.data$n_nonmiss), .data$ResponseId_fu)
    best <- sub |> dplyr::slice_head(n = 1L)
    dropped <- sub |> dplyr::filter(.data$ResponseId_fu != best$ResponseId_fu[1L])
    fmt_dates <- function(row) {
      s <- if ("StartDate_fu" %in% names(row)) as.character(row$StartDate_fu) else NA
      e <- if ("EndDate_fu"   %in% names(row)) as.character(row$EndDate_fu)   else NA
      paste0("started=", s %||% "NA", ", finished=", e %||% "NA")
    }
    cat("\n  ", pid, "\n", sep = "")
    cat(
      "    KEPT:    ResponseId_fu=", best$ResponseId_fu[1L],
      ", followup_survey=", best$followup_survey[1L],
      ", n_nonmiss=", best$n_nonmiss[1L],
      ", ", fmt_dates(best[1L, ]), "\n",
      sep = ""
    )
    if (nrow(dropped) > 0L) {
      cat("    DROPPED:\n")
      for (j in seq_len(nrow(dropped))) {
        cat(
          "             ResponseId_fu=", dropped$ResponseId_fu[j],
          ", followup_survey=", dropped$followup_survey[j],
          ", n_nonmiss=", dropped$n_nonmiss[j],
          ", ", fmt_dates(dropped[j, ]), "\n",
          sep = ""
        )
      }
    }
  }
  cat("=======================================================\n\n")

  fu_nodup <- fu_all |>
    dplyr::anti_join(dup_pid, by = "PROLIFIC_PID") |>
    dplyr::select(-"n_nonmiss")
  fu_kept <- fu_all |>
    dplyr::semi_join(dup_pid, by = "PROLIFIC_PID") |>
    dplyr::group_by(.data$PROLIFIC_PID) |>
    dplyr::arrange(dplyr::desc(.data$n_nonmiss), .data$ResponseId_fu, .by_group = TRUE) |>
    dplyr::slice_head(n = 1L) |>
    dplyr::ungroup() |>
    dplyr::select(-"n_nonmiss")
  fu_all <- dplyr::bind_rows(fu_nodup, fu_kept)
}

fu_outcome_cols <- paste0(
  c("trust_irs_post_1", "fav_irs_post_1", "agents_post_1",
    "servants_post_1", "fav_cs_post_1", "fav_ssa_post_1",
    "enforce_post_1", "doge_post_1", "trump_post_1",
    paste0("mvs", 1:6, "_post")),
  "_fu"
)

fu_all <- fu_all |>
  mutate(
    fu_completed_outcomes = as.integer(rowSums(!is.na(pick(all_of(fu_outcome_cols)))) ==
                                         length(fu_outcome_cols))
  )

n_outcomes_complete <- sum(fu_all$fu_completed_outcomes)
n_qualtrics_finished <- sum(!is.na(fu_all$Finished_fu) & fu_all$Finished_fu == "1")
cat(
  "Follow-up completion:\n",
  "  Qualtrics Finished=1: ", n_qualtrics_finished, "\n",
  "  All outcomes non-NA:  ", n_outcomes_complete, "\n",
  sep = ""
)

d_itt <- readRDS(here("data", "d_itt.rds"))
d <- readRDS(here("data", "d.rds"))

validate_followup_exports(fu_lewis_raw, fu_haidt_raw, fu_all, d)

join_fu <- function(base) {
  dplyr::left_join(base, fu_all, by = "PROLIFIC_PID")
}

d_itt_w <- join_fu(d_itt) |> add_t2_derived()
d_w     <- join_fu(d)     |> add_t2_derived()

saveRDS(d_itt_w, here("data", "d_itt_with_followup.rds"))
saveRDS(d_w, here("data", "d_with_followup.rds"))

n_fu_pid <- n_distinct(fu_all$PROLIFIC_PID)
n_d_rows <- sum(!is.na(d_w$ResponseId_fu))
n_d_pid <- n_distinct(d_w$PROLIFIC_PID[!is.na(d_w$ResponseId_fu)])
if (n_d_rows > n_d_pid) {
  warning(
    "d (completers) has duplicate PROLIFIC_PID for some participants; follow-up fields ",
    "are replicated on those rows (row links = ", n_d_rows, ", unique PIDs = ",
    n_d_pid, "). Decide which wave-1 row to keep before longitudinal models.",
    call. = FALSE
  )
}

cat(
  "Follow-up linkage complete.\n",
  "  Follow-up completes (unique PID): ", n_fu_pid, "\n",
  "  Linked to d_itt rows:             ", sum(!is.na(d_itt_w$ResponseId_fu)), "\n",
  "  Linked to d (completers) rows:    ", n_d_rows, "\n",
  sep = ""
)

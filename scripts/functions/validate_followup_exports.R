#' Validate follow-up Qualtrics exports against wave-1 completers
#'
#' Checks: (1) every T2 respondent is a wave-1 completer; (2) assigned text arm
#' matches which follow-up instrument they took; (3) T2 reuses the same item
#' names as wave-1 *post* blocks (outcomes + comprehension).
#'
#' Comprehension scoring in analysis uses the same correct-answer coding as
#' wave 1 (`== 1L` in scripts/functions/add_t2_derived.R, matching
#' scripts/01_wrangle_data.R).
#'
#' @param fu_lewis_raw Tibble from Lewis follow-up CSV (before `_fu` rename).
#' @param fu_haidt_raw Tibble from Haidt follow-up CSV.
#' @param fu_all Combined follow-up tibble after `rename_followup_block` + bind_rows.
#' @param d Wave-1 completers (`d.rds`).
#' @noRd
validate_followup_exports <- function(fu_lewis_raw, fu_haidt_raw, fu_all, d) {
  post_core <- c(
    "trust_irs_post_1", "fav_irs_post_1", "agents_post_1",
    "servants_post_1", "fav_cs_post_1", "fav_ssa_post_1",
    "enforce_post_1", "doge_post_1", "trump_post_1",
    paste0("mvs", 1:6, "_post")
  )

  miss_lew <- setdiff(post_core, names(fu_lewis_raw))
  miss_hai <- setdiff(post_core, names(fu_haidt_raw))
  if (length(miss_lew) > 0L || length(miss_hai) > 0L) {
    stop(
      "Follow-up CSVs missing shared outcome columns. Lewis: ",
      paste(miss_lew, collapse = ", "),
      " Haidt: ",
      paste(miss_hai, collapse = ", "),
      call. = FALSE
    )
  }

  if (!all(c("lewis_comp1", "lewis_comp2", "lewis_comp3") %in% names(fu_lewis_raw))) {
    stop("Lewis follow-up export must include lewis_comp1, lewis_comp2, lewis_comp3.", call. = FALSE)
  }
  if (!all(c("haidt_comp1", "haidt_comp2", "haidt_comp3") %in% names(fu_haidt_raw))) {
    stop("Haidt follow-up export must include haidt_comp1, haidt_comp2, haidt_comp3.", call. = FALSE)
  }

  miss_d <- setdiff(post_core, names(d))
  if (length(miss_d) > 0L) {
    stop(
      "Wave-1 d.rds missing columns expected to align with T2: ",
      paste(miss_d, collapse = ", "),
      call. = FALSE
    )
  }
  if (!all(c("lewis_comp1", "lewis_comp2", "lewis_comp3") %in% names(d)) ||
    !all(c("haidt_comp1", "haidt_comp2", "haidt_comp3") %in% names(d))) {
    stop("Wave-1 d.rds must include both lewis_comp* and haidt_comp* columns.", call. = FALSE)
  }

  fu_pid <- fu_all |> dplyr::filter(!is.na(.data$PROLIFIC_PID))

  d_uniq <- d |>
    dplyr::group_by(PROLIFIC_PID) |>
    dplyr::summarise(
      n_rows = dplyr::n(),
      n_text = dplyr::n_distinct(as.character(text)),
      text = dplyr::first(as.character(text)),
      .groups = "drop"
    )
  dup_arm <- d_uniq |> dplyr::filter(.data$n_text > 1L)
  if (nrow(dup_arm) > 0L) {
    stop(
      "Wave-1 completers d has conflicting `text` for duplicate PROLIFIC_PID(s): ",
      paste(utils::head(dup_arm$PROLIFIC_PID, 10L), collapse = ", "),
      call. = FALSE
    )
  }

  orphan <- fu_pid |>
    dplyr::anti_join(dplyr::select(d_uniq, "PROLIFIC_PID"), by = "PROLIFIC_PID")
  if (nrow(orphan) > 0L) {
    bad <- utils::head(unique(orphan$PROLIFIC_PID), 20L)
    stop(
      "Some follow-up respondents are not wave-1 completers (PROLIFIC_PID not in d). ",
      "First IDs: ", paste(bad, collapse = ", "),
      call. = FALSE
    )
  }

  chk <- fu_pid |>
    dplyr::inner_join(dplyr::select(d_uniq, "PROLIFIC_PID", "text"), by = "PROLIFIC_PID")
  bad_txt <- chk |>
    dplyr::filter(.data$text != .data$followup_survey)
  if (nrow(bad_txt) > 0L) {
    stop(
      "Wave-1 `text` does not match follow-up instrument (followup_survey) for ",
      nrow(bad_txt), " row(s). Example PID: ", bad_txt$PROLIFIC_PID[1L],
      call. = FALSE
    )
  }

  need_fu <- paste0(c(
    post_core,
    "lewis_comp1", "lewis_comp2", "lewis_comp3",
    "haidt_comp1", "haidt_comp2", "haidt_comp3"
  ), "_fu")
  miss_fu <- setdiff(need_fu, names(fu_all))
  if (length(miss_fu) > 0L) {
    stop(
      "Combined follow-up data missing renamed columns: ",
      paste(miss_fu, collapse = ", "),
      call. = FALSE
    )
  }

  if (!"free_text_recall_fu" %in% names(fu_all)) {
    warning(
      "Optional column free_text_recall_fu not found; open-ended recall section skipped in analyses.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

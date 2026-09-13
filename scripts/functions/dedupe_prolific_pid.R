#' Keep one row per non-missing PROLIFIC_PID (wave-1)
#'
#' For each PID: prefer treated; else prefer failed AC1; then earliest StartDate.
#'
#' @noRd
dedupe_prolific_pid <- function(dat) {
  required <- c("PROLIFIC_PID", "treated", "attncheck1_passed", "StartDate")
  missing_cols <- setdiff(required, names(dat))
  if (length(missing_cols) > 0L) {
    stop(
      "dedupe_prolific_pid() missing column(s): ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  pid <- dat$PROLIFIC_PID
  if (!any(duplicated(pid[!is.na(pid)]))) {
    return(dat)
  }

  keep <- is.na(pid)
  for (p in unique(stats::na.omit(pid))) {
    i <- which(pid == p)
    if (any(dat$treated[i])) {
      i <- i[dat$treated[i]]
    } else if (any(dat$attncheck1_passed[i] %in% FALSE)) {
      i <- i[dat$attncheck1_passed[i] %in% FALSE]
    }
    keep[i[which.min(dat$StartDate[i])]] <- TRUE
  }

  dat[keep, , drop = FALSE]
}

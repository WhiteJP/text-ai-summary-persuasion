# =============================================================================
# 11 · Text comparison tables
# =============================================================================
# Builds the paper's descriptive text-metric tables from the OSF files
# in data/: the four stimulus texts and gpt4o_topic_labels.csv (modal GPT-4o
# paragraph labels).
#
# Writes:
#   output/tables/text-metrics.tex
#   output/tables/text-metrics-control.tex
#   output/tables/text-metrics.csv
#   output/tables/text-metrics-control.csv
#
# Requires the project virtualenv from `python3 text_analysis/restore.py`.

suppressPackageStartupMessages(library(here))

needed <- c(
  "lewis_full_text_clean_noheads.txt",
  "lewis_ai_summary_clean_noheads.txt",
  "haidt_full_text_clean_noheads.txt",
  "haidt_ai_summary_clean_noheads.txt",
  "gpt4o_topic_labels.csv"
)
missing <- needed[!file.exists(here("data", needed))]

if (length(missing)) {
  message(
    "Text-table inputs not found in data/: ",
    paste(missing, collapse = ", "),
    "\nRe-run scripts/00_download_data.R. Skipping text tables."
  )
} else {
  py <- if (.Platform$OS.type == "windows") {
    here("text_analysis", ".venv", "Scripts", "python.exe")
  } else {
    here("text_analysis", ".venv", "bin", "python")
  }
  if (!file.exists(py)) {
    stop(
      "Missing ", py, ".\n",
      "From the repository root run:\n  python3 text_analysis/restore.py"
    )
  }

  script <- here("text_analysis", "build_tables.py")
  status <- system2(py, args = shQuote(script))
  if (!isTRUE(status == 0)) {
    stop(
      "text_analysis/build_tables.py failed (exit ", status, "). ",
      "Try: python3 text_analysis/restore.py"
    )
  }
}

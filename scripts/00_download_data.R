# =============================================================================
# 00 · Download data from OSF
# =============================================================================
# Downloads all files from the data/ subfolder on OSF node d2wun
# into the local data/ directory (survey CSVs, the four stimulus
# texts, and gpt4o_topic_labels.csv). Skips files that already exist.

source(here::here("scripts", "functions", "download_data.R"))
download_osf_data("d2wun", osf_path = "data")

expected_text_files <- c(
  "lewis_full_text_clean_noheads.txt",
  "lewis_ai_summary_clean_noheads.txt",
  "haidt_full_text_clean_noheads.txt",
  "haidt_ai_summary_clean_noheads.txt",
  "gpt4o_topic_labels.csv"
)
missing_text_files <- expected_text_files[
  !file.exists(file.path(here::here("data"), expected_text_files))
]
if (length(missing_text_files)) {
  warning(
    "Text-table inputs not on OSF / not in data/: ",
    paste(missing_text_files, collapse = ", "),
    call. = FALSE,
    immediate. = TRUE
  )
}

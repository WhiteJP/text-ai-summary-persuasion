# =============================================================================
# 00 · Download data from OSF
# =============================================================================
# Downloads text-ai-summary-qualtrics-data.csv from OSF node d2wun
# into the data/ directory. Skips if the file already exists.

source(here::here("scripts", "functions", "download_data.R"))
download_osf_data("d2wun")

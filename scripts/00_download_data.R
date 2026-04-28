# =============================================================================
# 00 · Download data from OSF
# =============================================================================
# Downloads all files from the data/ subfolder on OSF node d2wun
# into the local data/ directory. Skips files that already exist.

source(here::here("scripts", "functions", "download_data.R"))
download_osf_data("d2wun", osf_path = "data")

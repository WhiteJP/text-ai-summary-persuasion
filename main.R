# =============================================================================
# Text-AI Summary Persuasion: Master Analysis Script
# =============================================================================
#
# This script runs all analyses reported in the paper in sequence.
# It can be run end-to-end via source("main.R"), which will:
#   - Download data from OSF (if not already present)
#   - Wrangle and save analysis datasets
#   - Optionally link follow-up Qualtrics exports (Step 1b)
#   - Print all statistical results to the console
#   - Save Figure 1 to output/figures/main-results.png
#
# For a more exploratory experience, step through each script interactively
# in RStudio (in the order shown below).
#
# Prerequisites: run renv::restore() first to install required packages.
# =============================================================================

options(width = 200L)

suppressPackageStartupMessages(library(here))

cat("====================================================================\n")
cat("Text-AI Summary Persuasion: Reproducible Analysis Pipeline\n")
cat("====================================================================\n\n")

# Step 0: Download data from OSF
cat("\n##########################################################################\n")
cat("## Step 0: Downloading data from OSF\n")
cat("##########################################################################\n\n")
source(here("scripts", "00_download_data.R"))

# Step 1: Wrangle data
cat("\n##########################################################################\n")
cat("## Step 1: Wrangling data\n")
cat("##########################################################################\n\n")
source(here("scripts", "01_wrangle_data.R"))

# Step 1b: Link follow-up survey(s) when exports are present in data/
if (length(Sys.glob(here("data", "followup*lewis*.csv"))) >= 1L &&
    length(Sys.glob(here("data", "followup*[Hh]aidt*.csv"))) >= 1L) {
  cat("\n##########################################################################\n")
  cat("## Step 1b: Linking follow-up data\n")
  cat("##########################################################################\n\n")
  source(here("scripts", "01b_link_followup_data.R"))
} else {
  cat("\n##########################################################################\n")
  cat("## Step 1b: Skipped (no follow-up CSVs matching followup*)\n")
  cat("##########################################################################\n\n")
}

# Step 2: Sample descriptives
cat("\n##########################################################################\n")
cat("## Step 2: Sample descriptives\n")
cat("##########################################################################\n\n")
source(here("scripts", "02_sample_descriptives.R"))

# Step 3: Attrition analysis
cat("\n##########################################################################\n")
cat("## Step 3: Attrition analysis\n")
cat("##########################################################################\n\n")
source(here("scripts", "03_attrition_analysis.R"))

# Step 4: Comprehension and reading time
cat("\n##########################################################################\n")
cat("## Step 4: Comprehension and reading time\n")
cat("##########################################################################\n\n")
source(here("scripts", "04_comprehension_timing.R"))

# Step 5: T1 analyses (wave-1 ATEs, Bayes factors, forest inputs)
cat("\n##########################################################################\n")
cat("## Step 5: T1 analyses\n")
cat("##########################################################################\n\n")
source(here("scripts", "05_t1_analyses.R"))

# Step 6: T2 analyses (follow-up ATEs, mediation, forest inputs)
cat("\n##########################################################################\n")
cat("## Step 6: T2 analyses\n")
cat("##########################################################################\n\n")
source(here("scripts", "06_t2_analyses.R"))

# Step 7: Main figure (Figure 1)
cat("\n##########################################################################\n")
cat("## Step 7: Main figure\n")
cat("##########################################################################\n\n")
source(here("scripts", "07_main_figure.R"))

# Step 8: Robustness checks
cat("\n##########################################################################\n")
cat("## Step 8: Robustness checks\n")
cat("##########################################################################\n\n")
source(here("scripts", "08_robustness_checks.R"))

# Step 9: Heterogeneity by demographics (T1 + T2)
cat("\n##########################################################################\n")
cat("## Step 9: Heterogeneity by demographics\n")
cat("##########################################################################\n\n")
source(here("scripts", "09_heterogeneity_demographics.R"))

# Step 10: Internal consistency (Cronbach's alpha)
cat("\n##########################################################################\n")
cat("## Step 10: Cronbach's alpha\n")
cat("##########################################################################\n\n")
source(here("scripts", "10_cronbach_alpha.R"))

cat("\n====================================================================\n")
cat("All analyses complete.\n")
cat("Figure saved to: output/figures/main-results.png\n")
cat("Forest-plot inputs saved to: output/intermediate/forest_t1.rds, forest_t2.rds\n")
cat("====================================================================\n")

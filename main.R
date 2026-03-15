# =============================================================================
# Text-AI Summary Persuasion: Master Analysis Script
# =============================================================================
#
# This script runs all analyses reported in the paper in sequence.
# It can be run end-to-end via source("main.R"), which will:
#   - Download data from OSF (if not already present)
#   - Wrangle and save analysis datasets
#   - Print all statistical results to the console
#   - Save Figure 1 to output/figures/main-results.png
#
# For a more exploratory experience, step through each script interactively
# in RStudio (in the order shown below).
#
# Prerequisites: run renv::restore() first to install required packages.
# =============================================================================

library(here)

cat("====================================================================\n")
cat("Text-AI Summary Persuasion: Reproducible Analysis Pipeline\n")
cat("====================================================================\n\n")

# Step 0: Download data from OSF
cat("--- Step 0: Downloading data from OSF ---\n")
source(here("scripts", "00_download_data.R"))

# Step 1: Wrangle data
cat("\n--- Step 1: Wrangling data ---\n")
source(here("scripts", "01_wrangle_data.R"))

# Step 2: Sample descriptives
cat("\n--- Step 2: Sample descriptives ---\n")
source(here("scripts", "02_sample_descriptives.R"))

# Step 3: Attrition analysis
cat("\n--- Step 3: Attrition analysis ---\n")
source(here("scripts", "03_attrition_analysis.R"))

# Step 4: Comprehension and reading time
cat("\n--- Step 4: Comprehension and reading time ---\n")
source(here("scripts", "04_comprehension_timing.R"))

# Step 5: Main analyses (ATE, Bayes factors, Figure 1)
cat("\n--- Step 5: Main analyses ---\n")
source(here("scripts", "05_main_analyses.R"))

# Step 6: Robustness checks
cat("\n--- Step 6: Robustness checks ---\n")
source(here("scripts", "06_robustness_checks.R"))

# Step 7: Heterogeneity by demographics
cat("\n--- Step 7: Heterogeneity by demographics ---\n")
source(here("scripts", "07_heterogeneity_demographics.R"))

# Step 8: Internal consistency (Cronbach's alpha)
cat("\n--- Step 8: Cronbach's alpha ---\n")
source(here("scripts", "08_cronbach_alpha.R"))

cat("\n====================================================================\n")
cat("All analyses complete.\n")
cat("Figure saved to: output/figures/main-results.png\n")
cat("Tables saved to: output/tables/\n")
cat("====================================================================\n")

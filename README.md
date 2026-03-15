# text-ai-summary-persuasion

This repository contains the complete code to reproduce all analyses, figures, and tables for our research paper:

**White, J. P., Berinsky, A., & Rand, D. G. (2026). Popular social science nonfiction shifts values and partisan attitudes---and their AI summaries are just as persuasive.**

## Quick Start

### Prerequisites

- **R 4.5.0+** — Download from [r-project.org](https://www.r-project.org/)
- **RStudio** (recommended) — Download from [posit.co](https://posit.co/download/rstudio-desktop/)

### Reproduce all analyses

1. **Clone the repository:**

   ```bash
   git clone https://github.com/WhiteJP/text-ai-summary-persuasion.git
   cd text-ai-summary-persuasion
   ```

2. **Open the RStudio project:**
   Double-click `text-ai-summary-persuasion.Rproj`, or open RStudio and go to File → Open Project.

3. **Install dependencies:**

   ```r
   renv::restore()
   ```

   This will install all R packages at the exact versions used in our analyses. It may take 5–10 minutes on a first run.

4. **Run all analyses:**

   ```r
   source("main.R")
   ```

   This downloads the data from OSF (if not already present), runs every analysis script in order, prints all statistical output to the console, and saves the main figure and tables.

### Expected Output

- **Console output**: Sample flow, demographics, attrition tests, comprehension and reading time ANOVAs, ATEs with Bayes factors and prior-sensitivity analysis, robustness checks (baseline-carry-forward and Lee bounds), heterogeneity by demographics, and Cronbach's alpha — all printed to the console.
- **Figure**: `output/figures/main-results.png` (Figure 1 from the paper).
- **Tables**: `output/tables/ate-sensitivity.tex` (ATE sensitivity to attrition assumptions).
- **Runtime**: ~2–5 minutes total (after packages are installed).

### Alternative: Run scripts individually

If you prefer to explore interactively, run each script in RStudio in the order shown in `main.R`:

```r
source("scripts/00_download_data.R")       # Download data from OSF
source("scripts/01_wrangle_data.R")        # Wrangle raw data, create analysis samples
source("scripts/02_sample_descriptives.R") # Sample flow, demographics, balance
source("scripts/03_attrition_analysis.R")  # Attrition tests, attriter progress
source("scripts/04_comprehension_timing.R")# Comprehension and reading time ANOVAs
source("scripts/05_main_analyses.R")       # ATEs, Bayes factors, prior sensitivity, Figure 1
source("scripts/06_robustness_checks.R")   # Carry-forward, Lee bounds, ATE sensitivity table
source("scripts/07_heterogeneity_demographics.R")  # Treatment-effect moderation by demographics
source("scripts/08_cronbach_alpha.R")      # Internal consistency (Cronbach's alpha)
```

## Data

The data is hosted on the Open Science Framework (OSF): <https://osf.io/d2wun>

**Automatic download**: Running `main.R` (or `scripts/00_download_data.R`) automatically downloads `text-ai-summary-qualtrics-data.csv` into the `data/` directory. If the file already exists locally, the download is skipped.

## Repository Structure

```
text-ai-summary-persuasion/
├── main.R                              # Master script (runs everything)
├── DESCRIPTION                         # R package dependencies
├── renv.lock                           # Locked package versions
├── renv/                               # renv library management
├── scripts/
│   ├── functions/
│   │   ├── read_csv_qualtrics.R        # Qualtrics CSV reader
│   │   └── download_data.R             # OSF download helper
│   ├── 00_download_data.R              # Download data from OSF
│   ├── 01_wrangle_data.R              # Data wrangling & sample definitions
│   ├── 02_sample_descriptives.R       # Sample flow, demographics, balance
│   ├── 03_attrition_analysis.R        # Differential attrition tests
│   ├── 04_comprehension_timing.R      # Comprehension & reading time ANOVAs
│   ├── 05_main_analyses.R            # ATE estimation, Bayes factors, Figure 1
│   ├── 06_robustness_checks.R        # Carry-forward, Lee bounds, ATE sensitivity
│   ├── 07_heterogeneity_demographics.R  # Treatment-effect moderation by demographics
│   └── 08_cronbach_alpha.R           # Internal consistency (Cronbach's alpha)
├── data/                               # Downloaded at runtime (gitignored)
└── output/
    ├── figures/                        # Generated figures (gitignored)
    └── tables/                         # LaTeX tables (e.g., ate-sensitivity.tex)
```

## What Each Script Produces

| Script | Paper Section | Output |
|--------|--------------|--------|
| `02_sample_descriptives.R` | Methods: Participants | Sample sizes (N = 625 → 599 → 555), demographics |
| `03_attrition_analysis.R` | Results: Attrition | Logistic regression (dropout ~ format × text), attriter progress |
| `04_comprehension_timing.R` | Results: Comprehension & reading time | 2-way ANOVAs (text × format) |
| `05_main_analyses.R` | Results: ATEs & format equivalence | ATEs, Bayes factors (incl. prior sensitivity), **Figure 1** |
| `06_robustness_checks.R` | Results: Robustness | Carry-forward, Lee bounds, **ate-sensitivity.tex** |
| `07_heterogeneity_demographics.R` | Results: Heterogeneity | Moderation by gender, age, education, income, race, partisanship |
| `08_cronbach_alpha.R` | Methods: Measures | Cronbach's α for IRS, Civil Service, and MVS composites |

## System Requirements

**Software**: R (≥ 4.5.0), RStudio (recommended)

**R packages** (automatically installed via `renv`): dplyr, tidyr, readr, ggplot2, stringr, purrr, tibble, lubridate, estimatr, broom, BayesFactor, patchwork, ggh4x, osfr, here, fs, psych

**Hardware**: Any standard desktop or laptop.

**OS**: Tested on Windows 11, but should work on macOS and Linux.

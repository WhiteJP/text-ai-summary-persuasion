# text-ai-summary-persuasion

This repository contains the complete code to reproduce all analyses, figures, and tables for our research paper:

**White, J. P., Berinsky, A. J, Pastoriza, J. & Rand, D. G. (2026). Popular social science nonfiction can durably shift politically relevant attitudes---and their AI summaries are just as persuasive.**

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

   This downloads the data from OSF (if not already present), runs every analysis script in order, prints all statistical output to the console, and saves the main figure to `output/figures/`.

### Expected Output

- **Console output**: Sample flow, demographics, attrition tests, comprehension and reading time ANOVAs, T1 ATEs with Bayes factors and prior-sensitivity analysis, T2 (follow-up) ATEs and mediation, baseline-carry-forward robustness checks, heterogeneity by demographics, and Cronbach's alpha — all printed to the console.
- **Figure**: `output/figures/main-results.png` (Figure 1 from the paper).
- **Intermediate**: `output/intermediate/forest_t1.rds` and `forest_t2.rds` (forest-plot inputs consumed by `07_main_figure.R`).
- **Runtime**: ~2–5 minutes total (after packages are installed).

### Alternative: Run scripts individually

If you prefer to explore interactively, run each script in RStudio in the order shown in `main.R`:

```r
source("scripts/00_download_data.R")        # Download data from OSF
source("scripts/01_wrangle_data.R")         # Wrangle raw data, create analysis samples
source("scripts/01b_link_followup_data.R")  # Link followup data to original
source("scripts/02_sample_descriptives.R")  # Sample flow, demographics, balance
source("scripts/03_attrition_analysis.R")   # Attrition tests, attriter progress
source("scripts/04_comprehension_timing.R") # Comprehension and reading time ANOVAs
source("scripts/05_t1_analyses.R")          # Immediate post-treatment (T1) analyses
source("scripts/06_t2_analyses.R")          # Follow-up (T2) analyses
source("scripts/07_main_figure.R")          # Figure 1 (main results)
source("scripts/08_robustness_checks.R")    # Baseline-carry-forward attrition robustness checks
source("scripts/09_heterogeneity_demographics.R")  # Treatment-effect moderation by demographics
source("scripts/10_cronbach_alpha.R")       # Internal consistency (Cronbach's alpha)
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
│   │   ├── read_csv_qualtrics.R          # Qualtrics CSV reader
│   │   ├── download_data.R               # OSF download helper
│   │   ├── add_t2_derived.R              # Derive T2 change/composite columns
│   │   ├── forest_measure_specs.R        # DV specs shared across forest plots
│   │   ├── forest_plot_estimates.R       # Per-DV ATE / interaction estimator
│   │   └── validate_followup_exports.R   # Sanity checks for follow-up exports
│   ├── 00_download_data.R                # Download data from OSF
│   ├── 01_wrangle_data.R                 # Data wrangling & sample definitions
│   ├── 01b_link_followup_data.R          # Link follow-up Qualtrics exports (optional)
│   ├── 02_sample_descriptives.R          # Sample flow, demographics, balance
│   ├── 03_attrition_analysis.R           # Differential attrition tests
│   ├── 04_comprehension_timing.R         # Comprehension & reading time ANOVAs
│   ├── 05_t1_analyses.R                  # Wave-1 ATEs, Bayes factors, forest inputs
│   ├── 06_t2_analyses.R                  # Follow-up ATEs, mediation, forest inputs
│   ├── 07_main_figure.R                  # Figure 1
│   ├── 08_robustness_checks.R            # Baseline-carry-forward robustness
│   ├── 09_heterogeneity_demographics.R   # Treatment-effect moderation by demographics
│   └── 10_cronbach_alpha.R               # Internal consistency (Cronbach's alpha)
├── data/                               # Downloaded at runtime (gitignored)
└── output/
    ├── figures/                        # Generated figures (gitignored)
    └── intermediate/                   # RDS inputs for the main figure (gitignored)
```

## What Each Script Produces

| Script | Paper Section | Output |
|--------|--------------|--------|
| `02_sample_descriptives.R` | Methods: Participants | Original- and follow-up-survey sample flow, demographics, condition balance |
| `03_attrition_analysis.R` | Results: Attrition | Logistic regression (dropout ~ format × text), attriter progress, T2 attrition |
| `04_comprehension_timing.R` | Results: Comprehension & reading time | T1 2-way ANOVAs (text × format) for comprehension and reading time; T2 ANOVA and T1→T2 ANCOVA / paired t-tests when follow-up data are available |
| `05_t1_analyses.R` | Results: ATEs & format equivalence (wave 1) | T1 ATEs (Cohen's d), text × format interactions, binary above-midpoint LPM, SSA-within-format estimates, BF01 with medium / wide / ultrawide prior sensitivity, `forest_t1.rds` |
| `06_t2_analyses.R` | Results: Follow-up | T2 ATEs and text × format interactions, BF01 with prior sensitivity, moderated mediation (lavaan: persistence, direct, indirect effects with Full−Summary contrasts), T1 reanalysis on the T2 sample, `forest_t2.rds` |
| `07_main_figure.R` | Results: Main figure | **Figure 1** (`output/figures/main-results.png`): IRS pre/post panel + T1/T2 forest plots |
| `08_robustness_checks.R` | Results: Robustness | Three-scenario baseline-carry-forward (T1 ITT, T2 ITT, T2 from T1 completers): Cohen's d with 95% CIs, BF01 (medium prior), and BF01 prior-sensitivity grid |
| `09_heterogeneity_demographics.R` | Results: Heterogeneity | Treatment-effect moderation by gender, age, education, income, race, strong-Republican, and strong-conservative; raw + Holm-corrected p-values across moderators within each DV (T1 + T2) |
| `10_cronbach_alpha.R` | Methods: Measures | Cronbach's α (pre and post) for the IRS, Civil Service, and Material Values Scale composites |

## System Requirements

**Software**: R (≥ 4.5.0), RStudio (recommended)

**R packages** (automatically installed via `renv`): dplyr, tidyr, readr, ggplot2, stringr, purrr, tibble, lubridate, estimatr, broom, BayesFactor, patchwork, ggh4x, osfr, here, fs, psych, lavaan, car

**Hardware**: Any standard desktop or laptop.

**OS**: Tested on Windows 11 and macOS.

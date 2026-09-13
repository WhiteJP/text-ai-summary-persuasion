# text-ai-summary-persuasion

This repository contains the complete code to reproduce all analyses, figures, and tables for our research paper:

**White, J. P., Berinsky, A. J., Pastoriza, J. & Rand, D. G. (2026). Popular nonfiction can durably shift politically relevant attitudes---and so can much a much shorter AI summary**

## Quick Start

### Prerequisites

- **R 4.5.0+** — Download from [r-project.org](https://www.r-project.org/)
- **Python 3.10+** — for the text-comparison tables (3.12 recommended; `text_analysis/.python-version`)
- **RStudio** (recommended) — Download from [posit.co](https://posit.co/download/rstudio-desktop/) (Note, newest version 2026.04 bugs as at Apr 28 2026).
- **uv** (optional) — Download from [docs.astral.sh/uv](https://docs.astral.sh/uv/); used like renv to install the locked Python environment. `restore.py` falls back to `venv` + `pip` if uv is not installed.

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

   This installs all R packages at the exact versions in `renv.lock`. It may take 5–10 minutes on a first run.

   Then restore the Python environment used for the text-comparison tables (the counterpart of `renv::restore()`):

   ```bash
   python3 text_analysis/restore.py
   ```

   That creates `text_analysis/.venv` and installs the pinned packages from `text_analysis/uv.lock` (or the hashed `text_analysis/requirements.txt` if uv is not available), then downloads the NLTK corpora into `text_analysis/nltk_data/`.

4. **Run all analyses:**

   ```r
   source("main.R")
   ```

   This downloads the data from OSF (if not already present), runs every analysis script in order, prints all statistical output to the console, and saves the paper figures to `output/figures/`.

### Expected Output

- **Console output**: Sample flow, demographics, attrition tests, comprehension and reading time ANOVAs, T1 ATEs with Bayes factors and prior-sensitivity analysis, T2 (follow-up) ATEs and mediation, baseline-carry-forward robustness checks, heterogeneity by demographics, and Cronbach's alpha — all printed to the console.
- **Figures**: `irs-and-ates.png` (Figure 1), `format-interactions.png` (Figure 2), `bf-equivalence.png` (Figure 3).
- **Tables** (LaTeX for the paper): `bf-prior-sensitivity.tex`, `mediation-persistence.tex`, `ate-bcf-robustness.tex`, `te-heterogeneity.tex`, `text-metrics.tex`, `text-metrics-control.tex`.
- **Other tables** (CSV, plus some extra LaTeX companions): `bf01_t1.csv`, `bf01_t2.csv`, `bf01_sensitivity_t1.csv`, `bf01_sensitivity_t2.csv`, `mediation-persistence.csv`, `ate-bcf-robustness.csv`, `ate-bcf-bf01-long.csv`, `heterogeneity-demographics-t1.csv` / `-t1-summary.csv` / `-t2.csv` / `-t2-summary.csv`, `text-metrics.csv`, `text-metrics-control.csv`.
- **Intermediate**: `output/intermediate/forest_t1.rds` and `forest_t2.rds` (forest-plot inputs consumed by `07_figures.R`).
- **Runtime**: several minutes for frequentist analyses; Bayes-factor grids and bootstrapped mediation take longer.

### Alternative: Run scripts individually

If you prefer to explore interactively, run each numbered script in RStudio in the order shown in `main.R`. You do **not** source `scripts/functions/` first — each analysis script loads the helpers it needs. Later scripts do depend on RDS / CSV files written by earlier ones, so keep the order.

```r
source("scripts/00_download_data.R")        # Download data from OSF
source("scripts/01_wrangle_data.R")         # Wrangle raw data, create analysis samples
source("scripts/01b_link_followup_data.R")  # Link followup data to original
source("scripts/02_sample_descriptives.R")  # Sample flow, demographics, balance
source("scripts/03_attrition_analysis.R")   # Attrition tests, attriter progress
source("scripts/04_comprehension_timing.R") # Comprehension and reading time ANOVAs
source("scripts/05_t1_analyses.R")          # Immediate post-treatment (T1) analyses
source("scripts/06_t2_analyses.R")          # Follow-up (T2) analyses
source("scripts/07_figures.R")              # Figures 1–3 (IRS/ATEs, format, BF)
source("scripts/08_robustness_checks.R")    # Baseline-carry-forward checks + paper table
source("scripts/09_heterogeneity_demographics.R")  # Treatment-effect moderation by demographics
source("scripts/10_cronbach_alpha.R")       # Internal consistency (Cronbach's alpha)
source("scripts/11_text_metrics.R")         # Text comparison tables (Python)
```

## Data

The data is hosted on the Open Science Framework (OSF): <https://osf.io/d2wun>

**Automatic download**: Running `main.R` (or `scripts/00_download_data.R`) downloads everything in the OSF `data/` folder into the local `data/` directory: the wave-1 Qualtrics export, the two follow-up exports, the four de-headered stimulus texts (`lewis_full_text_clean_noheads.txt`, `lewis_ai_summary_clean_noheads.txt`, `haidt_full_text_clean_noheads.txt`, `haidt_ai_summary_clean_noheads.txt`), and `gpt4o_topic_labels.csv` (seven GPT-4o paragraph-label runs). Files that already exist locally are skipped.

## Repository Structure

```
text-ai-summary-persuasion/
├── main.R                              # Master script (runs everything)
├── DESCRIPTION                         # R package dependencies
├── renv.lock                           # Locked R package versions
├── renv/                               # renv library management
├── scripts/
│   ├── functions/
│   │   ├── read_csv_qualtrics.R          # Qualtrics CSV reader
│   │   ├── download_data.R               # OSF download helper
│   │   ├── add_t2_derived.R              # Derive T2 change/composite columns
│   │   ├── forest_measure_specs.R        # DV specs shared across forest plots
│   │   ├── forest_plot_estimates.R       # Per-DV ATE / interaction estimator
│   │   ├── analysis_helpers.R            # Shared BF01 / binary-LPM helpers
│   │   ├── dedupe_prolific_pid.R         # One row per PID in 01 (treated / AC1 / StartDate)
│   │   ├── supplementary_tables.R        # LaTeX writers for SM tables
│   │   └── validate_followup_exports.R   # Sanity checks for follow-up exports
│   ├── 00_download_data.R                # Download data from OSF
│   ├── 01_wrangle_data.R                 # Data wrangling & sample definitions
│   ├── 01b_link_followup_data.R          # Link follow-up Qualtrics exports (optional)
│   ├── 02_sample_descriptives.R          # Sample flow, demographics, balance
│   ├── 03_attrition_analysis.R           # Differential attrition tests
│   ├── 04_comprehension_timing.R         # Comprehension & reading time ANOVAs
│   ├── 05_t1_analyses.R                  # Wave-1 ATEs, Bayes factors, forest inputs
│   ├── 06_t2_analyses.R                  # Follow-up ATEs, mediation, forest inputs
│   ├── 07_figures.R                      # Figures 1–3
│   ├── 08_robustness_checks.R            # Baseline-carry-forward robustness + table
│   ├── 09_heterogeneity_demographics.R   # Treatment-effect moderation by demographics
│   ├── 10_cronbach_alpha.R               # Internal consistency (Cronbach's alpha)
│   └── 11_text_metrics.R                 # Calls text_analysis/build_tables.py
├── text_analysis/
│   ├── pyproject.toml                    # Declared Python deps (like DESCRIPTION)
│   ├── uv.lock                           # Locked Python versions (like renv.lock)
│   ├── requirements.txt                  # pip-installable lock (with hashes)
│   ├── restore.py                        # python3 text_analysis/restore.py
│   ├── .python-version                   # 3.12
│   ├── build_tables.py                   # Paper text-metric tables
│   ├── text_metrics.py                   # Length, overlap, style
│   ├── nrc_emotion.py                    # NRC Emotion Lexicon rates
│   ├── nltk_setup.py                     # Project-local NLTK corpora
│   └── label_topics.py                   # How gpt4o_topic_labels.csv was produced (API; not run here)
├── data/                               # Downloaded at runtime from OSF (gitignored)
└── output/
    ├── figures/                        # Generated figures (gitignored)
    ├── tables/                         # Generated LaTeX / CSV tables
    └── intermediate/                   # RDS inputs for Figures 1B and 2 (gitignored)
```

## What Each Script Produces

| Script | Paper Section | Output |
|--------|--------------|--------|
| `02_sample_descriptives.R` | Methods: Participants | Original- and follow-up-survey sample flow, demographics, condition balance |
| `03_attrition_analysis.R` | Results: Attrition | Logistic regression (dropout ~ format × text), attriter progress, T2 attrition |
| `04_comprehension_timing.R` | Results: Comprehension & reading time | T1 2-way ANOVAs (text × format) for comprehension and reading time; T2 ANOVA and T1→T2 ANCOVA / paired t-tests when follow-up data are available |
| `05_t1_analyses.R` | Results: ATEs & format equivalence (wave 1) | T1 ATEs (Cohen's d), text × format interactions, binary above-midpoint LPM, SSA-within-format estimates, BF01 with medium / wide / ultrawide prior sensitivity (`bf01_sensitivity_t1.csv`), `forest_t1.rds` |
| `06_t2_analyses.R` | Results: Follow-up / SM tables | T2 ATEs and text × format interactions, BF01 prior sensitivity, moderated mediation; writes `bf-prior-sensitivity.tex` and `mediation-persistence.tex` (plus CSVs), `forest_t2.rds` |
| `07_figures.R` | Results: Figures 1–3 | `irs-and-ates.png` (Figure 1), `format-interactions.png` (Figure 2), `bf-equivalence.png` (Figure 3) |
| `08_robustness_checks.R` | Results: Robustness | Completers vs baseline-carry-forward on ITT (T1 and T2): Cohen's d with 95% CIs and BF01; writes `ate-bcf-robustness.tex` |
| `09_heterogeneity_demographics.R` | Results: Heterogeneity | Treatment-effect moderation by gender, age, education, income, race, strong-Republican, and strong-conservative; raw + Holm-corrected p-values across moderators within each DV (T1 + T2); writes `te-heterogeneity.tex` |
| `10_cronbach_alpha.R` | Methods: Measures | Cronbach's α at pre, T1 post, and T2 for the IRS, Civil Service, and Material Values Scale composites (console) |
| `11_text_metrics.R` | Results: Text comparison | `text-metrics.tex` and `text-metrics-control.tex` |

## System Requirements

**Software**: R (≥ 4.5.0), Python ≥ 3.10 (text tables; 3.12 recommended), RStudio (recommended)

**R packages** (automatically installed via `renv`): dplyr, tidyr, readr, ggplot2, stringr, purrr, tibble, lubridate, estimatr, broom, BayesFactor, patchwork, ggh4x, osfr, here, fs, psych, lavaan, car

**Python packages** (automatically installed via `text_analysis/restore.py`): nltk, textstat, nrclex, textblob, plus pinned transitives in `text_analysis/uv.lock`

**Hardware**: Any standard desktop or laptop.

**OS**: Tested on Windows 11 and macOS.

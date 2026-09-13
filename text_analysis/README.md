# Text comparison metrics

Descriptive comparison of each chapter with its AI summary (Table 1 and SM Table S1 in the paper).

## What is computed

For each pair (Cyber Sleuth; Pursuit of Happiness):

- length and readability (normalized word count, sentence count, Flesch–Kincaid, MATTR)
- how extractive the summary is (compression, coverage, verbatim *n*-grams)
- how much of the original is represented (sentence Jaccard ≥ 0.50)
- first-person pronoun rates
- NRC emotion-word rates

For the Cyber Sleuth only, paragraph topics from seven GPT-4o runs in `data/gpt4o_topic_labels.csv` (tables use the modal label).

Definitions match Supplementary Methods, Section “Text Comparison Metrics.”

## Reproduce the tables

From the repository root, after `scripts/00_download_data.R` has populated `data/`:

```bash
python3 text_analysis/restore.py
text_analysis/.venv/bin/python text_analysis/build_tables.py
```

`restore.py` is the Python counterpart of `renv::restore()`: it creates `text_analysis/.venv`, installs the exact package versions from `uv.lock` (or the hashed `requirements.txt` if [uv](https://docs.astral.sh/uv/) is not installed), and downloads NLTK corpora into `text_analysis/nltk_data/`.

Or from R, after `renv::restore()` and `python3 text_analysis/restore.py`:

```r
source("scripts/11_text_metrics.R")
```

This writes `output/tables/text-metrics.tex` and `text-metrics-control.tex` (plus CSV copies). No API key is required.

Python **3.10+** is required (3.12 is pinned in `.python-version`). Direct dependencies are declared in `pyproject.toml`; exact versions of those packages and their transitives are locked in `uv.lock`.

To refresh the lock after changing `pyproject.toml`:

```bash
cd text_analysis
uv lock
uv export --frozen --no-dev --no-emit-project -o requirements.txt
```

## Stimulus texts

The four de-headered participant-facing texts live on OSF (`https://osf.io/d2wun`, in `data/`):

- `lewis_full_text_clean_noheads.txt`
- `lewis_ai_summary_clean_noheads.txt`
- `haidt_full_text_clean_noheads.txt`
- `haidt_ai_summary_clean_noheads.txt`

`00_download_data.R` downloads them with the survey files. These files already have titles and section heads removed.

## Topic labels

`data/gpt4o_topic_labels.csv` is the coding used in the paper (downloaded from OSF with the stimulus texts): seven independent GPT-4o runs of the SM prompt (temperature 0, seed 42, OpenAI only via OpenRouter). Each row is one paragraph in one run, with provenance (`labeled_at_utc`, `generation_id`, `system_fingerprint`, prompt and text hashes). `build_tables.py` takes the modal label per paragraph. Ties (if any) follow `TOPIC_ORDER` in that file.

The analysis never calls the API. `label_topics.py` is the script that produced the CSV deposited on OSF. Re-running it will not necessarily match.

```bash
export OPENROUTER_API_KEY=...
text_analysis/.venv/bin/python text_analysis/label_topics.py
```

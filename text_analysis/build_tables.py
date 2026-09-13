#!/usr/bin/env python3
"""Build the paper's text-comparison tables from the OSF files in data/.

Reads the four participant-facing texts and gpt4o_topic_labels.csv (seven
GPT-4o runs) from data/ after scripts/00_download_data.R. Topic shares
use the modal label per paragraph. Writes:

  output/tables/text-metrics.tex
  output/tables/text-metrics-control.tex
  output/tables/text-metrics.csv
  output/tables/text-metrics-control.csv

No API key is needed. To re-label paragraphs, see label_topics.py.
"""

import csv
import re
import sys
from collections import Counter, defaultdict
from pathlib import Path

HERE = Path(__file__).resolve().parent
REPO = HERE.parent
sys.path.insert(0, str(HERE))

from nrc_emotion import EMOTIONS, LABELS as NRC_LABELS, rates_per_k
from text_metrics import compare_pair, paragraphs, tokens

TEXT_FILES = {
    ("lewis", "full"): "lewis_full_text_clean_noheads.txt",
    ("lewis", "summary"): "lewis_ai_summary_clean_noheads.txt",
    ("haidt", "full"): "haidt_full_text_clean_noheads.txt",
    ("haidt", "summary"): "haidt_ai_summary_clean_noheads.txt",
}

TOPIC_ORDER = [
    "koopman_portrait",
    "civil_service_frame",
    "crypto_explainer",
    "cybercrime_unit_operations",
    "cases_summary",
    "case_silk_road",
    "case_welcome_to_video",
    "case_terror_finance",
    "case_binance",
    "other",
]

TOPIC_DISPLAY = {
    "koopman_portrait": "Koopman portrait",
    "civil_service_frame": "Civil-service frame",
    "crypto_explainer": "Crypto explainer",
    "cybercrime_unit_operations": "Cybercrime-unit operations",
    "cases_summary": "Cases summary (multi-case teaser)",
    "case_silk_road": "Case: Silk Road",
    "case_welcome_to_video": "Case: Welcome to Video",
    "case_terror_finance": "Case: Terror finance",
    "case_binance": "Case: Binance",
    "other": "Other",
}


def find_text(filename):
    path = REPO / "data" / filename
    if path.exists():
        return path
    raise FileNotFoundError(
        f"Missing {path}. Run scripts/00_download_data.R to fetch the OSF "
        f"texts into data/. Expected files:\n  "
        + "\n  ".join(TEXT_FILES.values())
    )


def load_pair(book):
    full = find_text(TEXT_FILES[(book, "full")]).read_text(encoding="utf-8")
    summary = find_text(TEXT_FILES[(book, "summary")]).read_text(encoding="utf-8")
    return full, summary


def modal_topic(votes):
    """Most common label. Ties go to the first topic in TOPIC_ORDER."""
    counts = Counter(votes)
    top = max(counts.values())
    winners = {topic for topic, n in counts.items() if n == top}
    if len(winners) == 1:
        return next(iter(winners)), top, False
    for topic in TOPIC_ORDER:
        if topic in winners:
            return topic, top, True
    return next(iter(winners)), top, True


def load_topic_labels():
    """Modal topic per paragraph from data/gpt4o_topic_labels.csv (OSF)."""
    path = REPO / "data" / "gpt4o_topic_labels.csv"
    if not path.exists():
        raise FileNotFoundError(
            f"Missing {path}. Run scripts/00_download_data.R to fetch "
            "gpt4o_topic_labels.csv from OSF into data/."
        )
    rows = list(csv.DictReader(path.open(encoding="utf-8")))
    if not rows:
        raise ValueError(f"{path} is empty.")
    if "run" not in rows[0]:
        raise ValueError(
            f"{path} needs a 'run' column (output of label_topics.py)."
        )

    grouped = defaultdict(list)
    for row in rows:
        key = (row["role"], int(row["paragraph_index"]))
        grouped[key].append(row["topic"])

    n_runs = len({row["run"] for row in rows})
    by_role = defaultdict(list)
    n_ties = 0
    for (role, idx), votes in sorted(grouped.items()):
        topic, n_votes, tied = modal_topic(votes)
        if tied:
            n_ties += 1
            print(
                f"  tie at {role} P{idx}: {dict(Counter(votes))}; "
                f"using {topic}"
            )
        by_role[role].append({
            "role": role,
            "paragraph_index": idx,
            "topic": topic,
            "n_votes": n_votes,
            "n_runs": len(votes),
        })
    print(f"Topic labels: mode of {n_runs} runs ({n_ties} ties).")
    return by_role


def topic_counts(text, labels):
    paras = paragraphs(text)
    if len(paras) != len(labels):
        raise ValueError(
            f"Paragraph count {len(paras)} does not match "
            f"{len(labels)} topic labels. The texts and gpt4o_topic_labels.csv "
            "are out of sync."
        )
    counts = defaultdict(int)
    for para, row in zip(paras, labels):
        counts[row["topic"]] += len(tokens(para))
    return counts, sum(counts.values())


def parse_num(s):
    s = s.strip().replace(",", "").replace("−", "-").replace("—", "").replace("–", "-")
    if not s or "/" in s:
        return None
    try:
        return float(s)
    except ValueError:
        return None


def fmt_diff(full, summary, decimals=None):
    a, b = parse_num(full), parse_num(summary)
    if a is None or b is None:
        return "—"
    d = b - a
    if decimals is None:
        decimals = len(summary.split(".", 1)[1]) if "." in summary else 0
    if decimals == 0:
        return f"{d:+,.0f}"
    return f"{d:+.{decimals}f}"


def core_rows(full_text, summary_text):
    m = compare_pair(full_text, summary_text)
    nrc_f = rates_per_k(full_text)
    nrc_s = rates_per_k(summary_text)

    def add(rows, section, metric, full, summary, decimals=None):
        rows.append((section, metric, full, summary, fmt_diff(full, summary, decimals)))

    rows = []
    add(rows, "Length and readability", "Word count (normalized tokens)",
        f"{m['n_tokens_full']:,d}", f"{m['n_tokens_summary']:,d}", 0)
    add(rows, "", "Sentence count",
        f"{m['n_sents_full']:,d}", f"{m['n_sents_summary']:,d}", 0)
    add(rows, "", "Flesch–Kincaid grade level",
        f"{m['fk_full']:.1f}", f"{m['fk_summary']:.1f}", 1)
    add(rows, "", "Lexical diversity (MATTR, window = 100)",
        f"{m['mattr_full']:.3f}", f"{m['mattr_summary']:.3f}", 3)

    add(rows, "Overlap with the original text", "Compression ratio (summary / full)",
        "—", f"{m['compression']:.3f}")
    add(rows, "", "Extractive coverage (share of summary words taken from original)",
        "—", f"{m['extractive_coverage']:.3f}")
    add(rows, "", "Verbatim 5-gram overlap", "—", f"{m['ngram5']:.3f}")
    add(rows, "", "Verbatim 20-gram overlap", "—", f"{m['ngram20']:.3f}")
    add(rows, "", "Longest verbatim run (tokens)", "—", f"{m['longest_run']:,d}")

    add(rows, "Coverage of the original text",
        "Source sentences represented (Jaccard ≥ 0.50)",
        "—", f"{m['source_coverage']:.3f}")

    add(rows, "Writing style (per 1,000 words)", "First-person singular",
        f"{m['fp1_full']:.2f}", f"{m['fp1_summary']:.2f}", 2)
    add(rows, "", "First-person plural",
        f"{m['fp2_full']:.2f}", f"{m['fp2_summary']:.2f}", 2)

    first = True
    for emo in EMOTIONS:
        add(
            rows,
            "Emotional tone (per 1,000 words)" if first else "",
            NRC_LABELS[emo],
            f"{nrc_f[emo]:.2f}",
            f"{nrc_s[emo]:.2f}",
            2,
        )
        first = False
    return rows


def topic_rows(full_text, summary_text, labels):
    full_n, full_tot = topic_counts(full_text, labels["full"])
    sum_n, sum_tot = topic_counts(summary_text, labels["summary"])
    rows = []
    first = True
    for topic in TOPIC_ORDER:
        nf, ns = full_n[topic], sum_n[topic]
        pf, ps = 100 * nf / full_tot, 100 * ns / sum_tot
        # Percentage-point difference from the displayed one-decimal shares.
        d_pp = round(ps, 1) - round(pf, 1)
        rows.append(
            (
                "Topics — n words (%)" if first else "",
                TOPIC_DISPLAY[topic],
                f"{nf:,d} ({pf:.1f}%)",
                f"{ns:,d} ({ps:.1f}%)",
                f"{ns - nf:+,d} ({d_pp:+.1f} pp)",
            )
        )
        first = False
    return rows


def insert_topics(rows, topic_block):
    idx = next(i for i, r in enumerate(rows) if r[0].startswith("Writing style"))
    return rows[:idx] + topic_block + rows[idx:]


def esc_tex(s):
    return (
        s.replace("&", r"\&")
        .replace("%", r"\%")
        .replace("#", r"\#")
        .replace("_", r"\_")
        .replace("≥", r"$\geq$")
        .replace("–", "--")
        .replace("—", "---")
        .replace("−", "$-$")
        .replace("Δ", r"$\Delta$")
    )


def latex_num(s):
    if s == "—":
        return "---"
    m = re.fullmatch(
        r"([+-][\d,]+(?:\.\d+)?)\s+\(([+-][\d.]+)(?:\s*pp)?\)",
        s.strip(),
    )
    if m:
        def signed(x):
            if x.startswith("+"):
                return f"$+{esc_tex(x[1:])}$"
            if x.startswith("-"):
                return f"$-{esc_tex(x[1:])}$"
            return esc_tex(x)
        pp = r"\,pp" if "pp" in s else ""
        return f"{signed(m.group(1))} ({signed(m.group(2))}{pp})"
    if s.startswith("+"):
        return f"$+{esc_tex(s[1:])}$"
    if s.startswith("-") and parse_num(s) is not None:
        return f"$-{esc_tex(s[1:])}$"
    return esc_tex(s)


def write_latex(path, rows, caption, label, note):
    lines = [
        r"\begin{table}[!htbp]",
        r"\centering",
        r"\caption{" + caption + "}",
        f"\\label{{{label}}}",
        r"\footnotesize",
        r"\setlength{\tabcolsep}{4pt}",
        r"\begin{tabular}{@{}lrrr@{}}",
        r"\toprule",
        r"Metric & \multicolumn{1}{c}{Full text} & \multicolumn{1}{c}{AI summary} "
        r"& \multicolumn{1}{c}{Difference} \\",
        r"\midrule",
    ]
    prev = None
    for section, metric, full, summ, diff in rows:
        if section and section != prev:
            if prev is not None:
                lines.append(r"\midrule")
            lines.append(r"\multicolumn{4}{@{}l}{\textit{" + esc_tex(section) + r"}} \\")
            prev = section
        lines.append(
            f"{esc_tex(metric)} & {latex_num(full)} & {latex_num(summ)} & {latex_num(diff)} \\\\"
        )
    lines += [
        r"\bottomrule",
        r"\end{tabular}",
        r"\begin{minipage}{\linewidth}",
        r"\vspace{0.5em}\footnotesize",
        r"\textit{Note.} " + note + r"\par",
        r"\end{minipage}",
        r"\end{table}",
        "",
    ]
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text("\n".join(lines), encoding="utf-8")


def write_csv(path, rows):
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", newline="", encoding="utf-8") as f:
        w = csv.writer(f)
        w.writerow(["Section", "Metric", "Full text", "AI summary", "Difference"])
        w.writerows(rows)


def main():
    lewis_full, lewis_summ = load_pair("lewis")
    haidt_full, haidt_summ = load_pair("haidt")
    labels = load_topic_labels()

    treatment = insert_topics(
        core_rows(lewis_full, lewis_summ),
        topic_rows(lewis_full, lewis_summ, labels),
    )
    control = core_rows(haidt_full, haidt_summ)

    out = REPO / "output" / "tables"
    treatment_note = (
        esc_tex(
            "Difference = AI summary − full text. Entries are blank where the "
            "metric is undefined on the full text. Metrics were computed on "
            "de-headered participant-facing texts. Writing-style and "
            "emotional-tone report rates per 1000 tokens. Topic entries are "
            "word counts and percents of total word count from "
        )
        + r"\texttt{openai/gpt-4o} generated labels; topic differences show "
        r"$\Delta n$ words ($\Delta$pp). For full details, see Supplementary "
        r"Methods Section~\ref{SM:text-metrics}."
    )
    control_note = (
        esc_tex(
            "Difference = AI summary − full text. Entries are blank where the "
            "metric is undefined on the full text. Metrics were computed on "
            "de-headered participant-facing texts. Writing-style and "
            "emotional-tone report rates per 1000 tokens. "
        )
        + r"For full details, see Supplementary Methods "
        r"Section~\ref{SM:text-metrics}."
    )

    write_latex(
        out / "text-metrics.tex",
        treatment,
        r"Descriptive text measures for \enquote{The Cyber Sleuth} and its AI summary.",
        "tab:text-metrics",
        treatment_note,
    )
    write_latex(
        out / "text-metrics-control.tex",
        control,
        r"Descriptive and derivation metrics comparing the control full text "
        r"(\enquote{The Pursuit of Happiness}) with its AI summary.",
        "tab:text-metrics-control",
        control_note,
    )
    write_csv(out / "text-metrics.csv", treatment)
    write_csv(out / "text-metrics-control.csv", control)

    print("Treatment (The Cyber Sleuth)")
    print(f"{'Metric':<55} {'Full':>16} {'Summary':>16} {'Diff':>16}")
    print("-" * 105)
    for section, metric, full, summ, diff in treatment:
        label = metric if not section else f"{section} / {metric}" if metric else section
        print(f"{label:<55} {full:>16} {summ:>16} {diff:>16}")

    print("\nControl (The Pursuit of Happiness)")
    print(f"{'Metric':<55} {'Full':>16} {'Summary':>16} {'Diff':>16}")
    print("-" * 105)
    for section, metric, full, summ, diff in control:
        label = metric if not section else f"{section} / {metric}" if metric else section
        print(f"{label:<55} {full:>16} {summ:>16} {diff:>16}")

    print(f"\nWrote tables to {out}")


if __name__ == "__main__":
    main()

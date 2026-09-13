#!/usr/bin/env python3
"""Optional: label Cyber Sleuth paragraphs with GPT-4o (seven independent runs).

This script is how data/gpt4o_topic_labels.csv was produced (then deposited on
OSF). The paper tables do not run it: build_tables.py reads that CSV
from data/ and takes the modal label for each paragraph. Re-running the
API will not necessarily match, even with a fixed seed.

Requires OPENROUTER_API_KEY.
"""

import csv
import hashlib
import json
import os
import sys
import urllib.request
from datetime import datetime, timezone
from pathlib import Path

HERE = Path(__file__).resolve().parent
REPO = HERE.parent
sys.path.insert(0, str(HERE))

from text_metrics import paragraphs

N_RUNS = 7
MODEL = "openai/gpt-4o"
PROVIDER = "openai"
SEED = 42
TEMPERATURE = 0
MAX_TOKENS = 3000
OUT_PATH = REPO / "data" / "gpt4o_topic_labels.csv"

FULL_FILE = "lewis_full_text_clean_noheads.txt"
SUMMARY_FILE = "lewis_ai_summary_clean_noheads.txt"

TOPICS = [
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

DESCRIPTIONS = """
- koopman_portrait: Material about Jared Koopman as a person—his background,
  personality, home life, hobbies (e.g. jiu-jitsu), and biographical framing
  of the investigator.
- civil_service_frame: Material whose main point is the public standing of the
  IRS or career civil servants—political controversy, budget cuts, stereotypes
  or “false narratives” about the agency, and defenses of dedicated government
  workers (including wishing people knew what they really do, or that their
  work remains underappreciated). Broader arguments about taxes and government
  also belong here. Mentions of agents’ concrete successes can still fit when
  they serve that reputational or political framing.
- crypto_explainer: Background explanation that teaches the reader what
  cryptocurrency or blockchain is (e.g. how bitcoin works, anonymity myths,
  basic technical premises)—general-audience education about the technology
  itself, not how investigators use it on the job.
- cybercrime_unit_operations: How the IRS Criminal Investigation cybercrime
  unit was built and how it works—resources, investigative methods and tracing
  techniques, comparisons to other agencies, historical precedents, and
  day-to-day work—without focusing on one named case narrative.
- cases_summary: Brief overviews that mention several investigations or
  outcomes together, without developing any single case in depth.
- case_silk_road: The Silk Road investigation (Ross Ulbricht / Dread Pirate
  Roberts) and related narrative details.
- case_welcome_to_video: The Welcome to Video investigation (an online
  child-abuse marketplace) and related rescues or arrests.
- case_terror_finance: Investigations into cryptocurrency fundraising linked
  to terrorist organizations (e.g. Hamas, ISIS, al-Qaeda).
- case_binance: The Binance investigation (e.g. Changpeng Zhao, sanctions or
  AML issues, and the settlement).
- other: No topic above clearly applies.
""".strip()

CSV_FIELDS = [
    "run",
    "labeled_at_utc",
    "model",
    "provider",
    "seed",
    "temperature",
    "system_fingerprint",
    "generation_id",
    "prompt_sha256",
    "source_file",
    "text_sha256",
    "role",
    "paragraph_index",
    "topic",
    "preview",
]


def sha256_text(text):
    return hashlib.sha256(text.encode("utf-8")).hexdigest()


def load_text(name):
    path = REPO / "data" / name
    if not path.exists():
        raise FileNotFoundError(path)
    return path.read_text(encoding="utf-8")


def prompt_for(paras):
    numbered = "\n\n".join(f"[P{i}]\n{p}" for i, p in enumerate(paras))
    return f"""Label each paragraph of the following text with exactly one topic from the following set:
{json.dumps(TOPICS)}

Below is a description of each topic:
{DESCRIPTIONS}

Rules:
- Prefer a case_* label when the paragraph is primarily about that one case.
- Use cases_summary when several cases are mentioned together without developing any one in depth.
- Use other when no topic clearly applies to a paragraph, which may be useful particularly for residual short transitions.

Return JSON only:
{{"labels": [ {{"paragraph_index": 0, "topic": "koopman_portrait"}} ]}}

PARAGRAPHS:
{numbered}
"""


def parse_json_content(content):
    content = (content or "").strip()
    if content.startswith("```"):
        content = content.strip("`")
        if content.startswith("json"):
            content = content[4:]
        content = content.strip()
    return json.loads(content)


def call_openrouter(prompt):
    api_key = os.environ.get("OPENROUTER_API_KEY")
    if not api_key:
        raise SystemExit("Set OPENROUTER_API_KEY to re-run topic labeling.")

    body = json.dumps({
        "model": MODEL,
        "messages": [{"role": "user", "content": prompt}],
        "temperature": TEMPERATURE,
        "seed": SEED,
        "max_tokens": MAX_TOKENS,
        "response_format": {"type": "json_object"},
        "provider": {
            "only": [PROVIDER],
            "allow_fallbacks": False,
            "require_parameters": True,
        },
    }).encode()
    req = urllib.request.Request(
        "https://openrouter.ai/api/v1/chat/completions",
        data=body,
        headers={
            "Authorization": f"Bearer {api_key}",
            "Content-Type": "application/json",
        },
    )
    with urllib.request.urlopen(req) as resp:
        result = json.loads(resp.read().decode())
    if result.get("error"):
        raise RuntimeError(result["error"])

    choice = result["choices"][0]
    parsed = parse_json_content(choice["message"].get("content"))
    meta = {
        "provider": result.get("provider") or PROVIDER,
        "system_fingerprint": (
            result.get("system_fingerprint")
            or choice.get("system_fingerprint")
            or ""
        ),
        "generation_id": result.get("id") or "",
        "model": result.get("model") or MODEL,
    }
    return parsed, meta


def labels_from_response(parsed, paras):
    label_map = {}
    for item in parsed.get("labels", []):
        if "paragraph_index" not in item:
            continue
        topic = str(item["topic"]).strip().lower()
        if topic not in TOPICS:
            topic = "other"
        label_map[int(item["paragraph_index"])] = topic

    rows = []
    for i, para in enumerate(paras):
        rows.append({
            "paragraph_index": i,
            "topic": label_map.get(i, "other"),
            "preview": para[:120].replace("\n", " "),
        })
    return rows


def label_text(role, source_file, text):
    paras = paragraphs(text)
    prompt = prompt_for(paras)
    parsed, meta = call_openrouter(prompt)
    rows = labels_from_response(parsed, paras)
    meta["prompt_sha256"] = sha256_text(prompt)
    meta["text_sha256"] = sha256_text(text)
    meta["source_file"] = source_file
    meta["role"] = role
    return rows, meta


def write_csv(path, rows):
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=CSV_FIELDS)
        writer.writeheader()
        writer.writerows(rows)


def main():
    full_text = load_text(FULL_FILE)
    summary_text = load_text(SUMMARY_FILE)
    all_rows = []

    for run in range(1, N_RUNS + 1):
        print(f"Run {run}/{N_RUNS} …", flush=True)
        for role, source_file, text in (
            ("full", FULL_FILE, full_text),
            ("summary", SUMMARY_FILE, summary_text),
        ):
            labeled_at = datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")
            labels, meta = label_text(role, source_file, text)
            print(
                f"  {role}: {meta['provider']}  "
                f"fingerprint={meta['system_fingerprint']}  "
                f"id={meta['generation_id']}",
                flush=True,
            )
            for row in labels:
                all_rows.append({
                    "run": run,
                    "labeled_at_utc": labeled_at,
                    "model": meta["model"],
                    "provider": meta["provider"],
                    "seed": SEED,
                    "temperature": TEMPERATURE,
                    "system_fingerprint": meta["system_fingerprint"],
                    "generation_id": meta["generation_id"],
                    "prompt_sha256": meta["prompt_sha256"],
                    "source_file": meta["source_file"],
                    "text_sha256": meta["text_sha256"],
                    "role": role,
                    "paragraph_index": row["paragraph_index"],
                    "topic": row["topic"],
                    "preview": row["preview"],
                })

    write_csv(OUT_PATH, all_rows)
    print(f"Wrote {len(all_rows)} rows ({N_RUNS} runs) to {OUT_PATH}")
    print("Tables use the modal label per paragraph; see build_tables.py.")


if __name__ == "__main__":
    main()

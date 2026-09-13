"""Text comparison metrics used in the paper tables.

Two tokenizations, matching the Supplementary Methods:

- Normalized tokens (lowercase; curly quotes straightened; em/en dashes
  treated as spaces; tokens = [a-z']+) for word counts, MATTR, n-gram
  overlap, extractive coverage, and first-person rates.
- Raw sentences via NLTK punkt_tab (paragraph by paragraph) for sentence
  counts and Flesch–Kincaid grade (syllables from textstat).
"""

import re
import unicodedata
from collections import Counter
from difflib import SequenceMatcher

import textstat
from nltk.tokenize.punkt import PunktTokenizer

from nltk_setup import configure as configure_nltk_data

_PUNKT_TAB = None

TOKEN_RE = re.compile(r"[a-z']+")
RAW_WORD_RE = re.compile(r"[A-Za-z0-9'\u2019]+(?:[-\u2013][A-Za-z0-9'\u2019]+)*")

FIRST_SINGULAR = {
    "i", "me", "my", "mine", "myself",
    "i'm", "i've", "i'd", "i'll",
}
FIRST_PLURAL = {
    "we", "us", "our", "ours", "ourselves",
    "we're", "we've", "we'd", "we'll",
}


def paragraphs(text):
    return [p.strip() for p in re.split(r"\n\s*\n", text) if p.strip()]


def normalize(text):
    text = unicodedata.normalize("NFC", text)
    text = text.replace("\u2018", "'").replace("\u2019", "'")
    text = text.replace("\u201c", '"').replace("\u201d", '"')
    text = text.replace("\u2014", " ").replace("\u2013", " ")
    return re.sub(r"\s+", " ", text.lower()).strip()


def tokens(text):
    return TOKEN_RE.findall(normalize(text))


def english_punkt_tab():
    global _PUNKT_TAB
    if _PUNKT_TAB is None:
        ensure_nltk()
        _PUNKT_TAB = PunktTokenizer("english")
    return _PUNKT_TAB


def sentences(text):
    tok = english_punkt_tab()
    out = []
    for para in paragraphs(text):
        out.extend(tok.tokenize(re.sub(r"\s+", " ", para)))
    return out


def raw_words(sentence):
    return RAW_WORD_RE.findall(sentence)


def ensure_nltk():
    configure_nltk_data()
    try:
        PunktTokenizer("english").tokenize("Dr. Smith went to Washington.")
    except LookupError:
        configure_nltk_data(download_missing=True)
        PunktTokenizer("english").tokenize("Dr. Smith went to Washington.")


def mattr(toks, window=100):
    """Moving-average type–token ratio (Covington & McFall, 2010)."""
    if len(toks) < window:
        raise ValueError(f"text shorter than MATTR window ({len(toks)} < {window})")
    counts = Counter(toks[:window])
    total = len(counts)
    n = len(toks) - window + 1
    for i in range(1, n):
        leaving, entering = toks[i - 1], toks[i + window - 1]
        counts[entering] += 1
        counts[leaving] -= 1
        if counts[leaving] == 0:
            del counts[leaving]
        total += len(counts)
    return total / (n * window)


def flesch_kincaid(sents):
    words = [w for s in sents for w in raw_words(s)]
    n_w, n_s = len(words), len(sents)
    n_syll = sum(max(1, textstat.syllable_count(w)) for w in words)
    return 0.39 * (n_w / n_s) + 11.8 * (n_syll / n_w) - 15.59


def ngram_overlap(summary, full, n):
    """Share of the summary's n-grams that appear verbatim in the full text."""
    full_set = {tuple(full[i : i + n]) for i in range(len(full) - n + 1)}
    grams = [tuple(summary[i : i + n]) for i in range(len(summary) - n + 1)]
    return sum(g in full_set for g in grams) / len(grams)


def longest_verbatim_run(summary, full):
    match = SequenceMatcher(None, summary, full, autojunk=False).find_longest_match(
        0, len(summary), 0, len(full)
    )
    return match.size


def extractive_coverage(full, summary):
    """Share of summary tokens belonging to a greedily matched extractive span.

    This is Grusky et al.'s (2018) coverage, not density.
    """
    covered = 0
    i = 0
    while i < len(summary):
        best = 0
        for j in range(len(full)):
            k = 0
            while (
                i + k < len(summary)
                and j + k < len(full)
                and summary[i + k] == full[j + k]
            ):
                k += 1
            if k > best:
                best = k
        if best:
            covered += best
            i += best
        else:
            i += 1
    return covered / len(summary) if summary else 0.0


def token_jaccard(a, b):
    if not a and not b:
        return 1.0
    if not a or not b:
        return 0.0
    ca, cb = Counter(a), Counter(b)
    inter = sum((ca & cb).values())
    union = sum((ca | cb).values())
    return inter / union if union else 0.0


def source_coverage(full_sents, summary_sents, threshold=0.50):
    """Share of full-text sentences with a summary sentence at Jaccard >= threshold."""
    full_tok = [tokens(s) for s in full_sents]
    sum_tok = [tokens(s) for s in summary_sents]
    hits = 0
    for cand in full_tok:
        best = max((token_jaccard(cand, s) for s in sum_tok), default=0.0)
        if best >= threshold:
            hits += 1
    return hits / len(full_tok) if full_tok else 0.0


def rate_per_k(toks, lexicon):
    return 1000.0 * sum(t in lexicon for t in toks) / len(toks)


def compare_pair(full_text, summary_text):
    """Return a dict of the core metrics for one full/summary pair."""
    ensure_nltk()
    ft, st = tokens(full_text), tokens(summary_text)
    fs, ss = sentences(full_text), sentences(summary_text)
    return {
        "n_tokens_full": len(ft),
        "n_tokens_summary": len(st),
        "n_sents_full": len(fs),
        "n_sents_summary": len(ss),
        "fk_full": flesch_kincaid(fs),
        "fk_summary": flesch_kincaid(ss),
        "mattr_full": mattr(ft),
        "mattr_summary": mattr(st),
        "compression": len(st) / len(ft),
        "extractive_coverage": extractive_coverage(ft, st),
        "ngram5": ngram_overlap(st, ft, 5),
        "ngram20": ngram_overlap(st, ft, 20),
        "longest_run": longest_verbatim_run(st, ft),
        "source_coverage": source_coverage(fs, ss),
        "fp1_full": rate_per_k(ft, FIRST_SINGULAR),
        "fp1_summary": rate_per_k(st, FIRST_SINGULAR),
        "fp2_full": rate_per_k(ft, FIRST_PLURAL),
        "fp2_summary": rate_per_k(st, FIRST_PLURAL),
    }

"""NRC Emotion Lexicon rates via NRCLex (Bailey, 2019).

NRCLex tokenizes with TextBlob and lemmatizes with WordNet, then looks
up each lemma in the Mohammad & Turney (2013) lexicon. We lowercase the
input first because the bundled keys are lowercase and NRCLex does not
case-fold. A token tagged with k emotions contributes k to the tag total.
Rates are tags per 1,000 lemmatized tokens.
"""

from nrclex import NRCLex
from nrclex.core import EMOTION_ORDER

from nltk_setup import configure as configure_nltk_data

EMOTIONS = [
    "positive",
    "negative",
    "fear",
    "anger",
    "anticipation",
    "trust",
    "surprise",
    "sadness",
    "disgust",
    "joy",
]

LABELS = {
    "positive": "Positive",
    "negative": "Negative",
    "fear": "Fear",
    "anger": "Anger",
    "anticipation": "Anticipation",
    "trust": "Trust",
    "surprise": "Surprise",
    "sadness": "Sadness",
    "disgust": "Disgust",
    "joy": "Joy",
}


def _ensure_wordnet():
    from textblob import TextBlob

    configure_nltk_data()
    try:
        lemmas = [w.lemmatize() for w in TextBlob("criminals crimes").words]
        if lemmas == ["criminal", "crime"]:
            return
    except Exception:
        pass

    configure_nltk_data(download_missing=True)


def rates_per_k(text):
    _ensure_wordnet()
    obj = NRCLex()
    obj.load_raw_text(text.lower())
    raw = {e: 0 for e in EMOTION_ORDER}
    raw.update(obj.raw_emotion_scores)
    n = len(obj.words)
    if not n:
        return {e: 0.0 for e in EMOTION_ORDER}
    return {e: 1000.0 * raw[e] / n for e in EMOTION_ORDER}

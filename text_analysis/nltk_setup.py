"""Put NLTK corpora in text_analysis/nltk_data/ and use only that folder."""

from pathlib import Path

DATA_DIR = Path(__file__).resolve().parent / "nltk_data"

NLTK_RESOURCES = ("punkt_tab", "wordnet", "omw-1.4")


def configure(download_missing=False):
    import nltk

    DATA_DIR.mkdir(exist_ok=True)
    nltk.data.path[:] = [str(DATA_DIR)]
    if download_missing:
        for res in NLTK_RESOURCES:
            ok = nltk.download(res, download_dir=str(DATA_DIR), quiet=True)
            if not ok:
                raise RuntimeError(f"Failed to download NLTK resource {res!r}")
    return DATA_DIR


if __name__ == "__main__":
    dest = configure(download_missing=True)
    print(f"NLTK corpora ready in {dest}")

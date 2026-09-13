#!/usr/bin/env python3
"""Install locked Python dependencies into a project-local virtualenv.

This is the Python counterpart of renv::restore(). From the repository root:

    python3 text_analysis/restore.py

Then run the text tables with:

    text_analysis/.venv/bin/python text_analysis/build_tables.py

or ``source("scripts/11_text_metrics.R")``.
"""

from __future__ import annotations

import os
import shutil
import subprocess
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
VENV = HERE / ".venv"
LOCK = HERE / "uv.lock"
REQUIREMENTS = HERE / "requirements.txt"


def venv_python() -> Path:
    if os.name == "nt":
        return VENV / "Scripts" / "python.exe"
    return VENV / "bin" / "python"


def run(cmd: list[str], **kwargs) -> None:
    print("+", " ".join(cmd))
    subprocess.check_call(cmd, **kwargs)


def restore_with_uv(uv: str) -> None:
    cmd = [uv, "sync", "--directory", str(HERE)]
    if LOCK.exists():
        cmd.insert(2, "--frozen")
    run(cmd)


def restore_with_pip() -> None:
    if not REQUIREMENTS.exists():
        raise SystemExit(
            "Need either uv (https://docs.astral.sh/uv/) or "
            f"{REQUIREMENTS.name}. Install uv, or clone this file from the repo."
        )
    python = sys.executable
    if not VENV.exists():
        run([python, "-m", "venv", str(VENV)])
    py = venv_python()
    run([str(py), "-m", "pip", "install", "--upgrade", "pip"])
    run([str(py), "-m", "pip", "install", "--require-hashes", "-r", str(REQUIREMENTS)])


def download_nltk_data() -> None:
    py = venv_python()
    run([str(py), str(HERE / "nltk_setup.py")])


def main() -> None:
    uv = shutil.which("uv")
    if uv:
        try:
            restore_with_uv(uv)
        except subprocess.CalledProcessError:
            print("uv sync failed; falling back to python -m venv + pip.")
            restore_with_pip()
    else:
        print("uv not found; falling back to python -m venv + pip.")
        restore_with_pip()

    download_nltk_data()
    py = venv_python()
    print(f"\nPython environment ready: {py}")
    print("This is the Python equivalent of renv::restore().")


if __name__ == "__main__":
    main()

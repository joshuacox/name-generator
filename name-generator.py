#!/usr/bin/env python
# This script mirrors the behaviour of name-generator.sh
# It respects the environment variables SEPARATOR, NOUN_FILE, ADJ_FILE and counto.
# If NOUN_FILE / ADJ_FILE are not set it picks a random file from the respective
# folders (nouns / adjectives).  The separator defaults to "-".
# The number of names emitted defaults to the terminal height (like `tput lines`).

import os
import random
import sys

# --------------------------------------------------------------------------- #
# Configuration (environment overrides possible)
# --------------------------------------------------------------------------- #
HERE = os.getcwd()
SEPARATOR = os.getenv("SEPARATOR", "-")
NOUN_FOLDER = os.getenv("NOUN_FOLDER", os.path.join(HERE, "nouns"))
ADJ_FOLDER = os.getenv("ADJ_FOLDER", os.path.join(HERE, "adjectives"))

def get_random_file(folder: str) -> str:
    """Return a random regular file from *folder*."""
    files = [
        os.path.join(folder, f)
        for f in os.listdir(folder)
        if os.path.isfile(os.path.join(folder, f))
    ]
    if not files:
        raise FileNotFoundError(f"No files found in folder {folder}")
    return random.choice(files)

def resolve_file(env_var: str, folder: str) -> str:
    """Return the file path from an environment variable or pick a random one."""
    path = os.getenv(env_var)
    if path:
        if not os.path.isfile(path):
            raise FileNotFoundError(f"Environment variable {env_var} points to non‑existent file {path}")
        return path
    return get_random_file(folder)

def debugger(debug_mode: bool, noun: str, adjective: str,
            noun_file: str, adj_file: str,
            noun_folder: str, adj_folder: str,
            countzero: int, counto: int) -> None:
    """Print debug information when DEBUG=true."""
    if not debug_mode:
        return
    print("DEBUG:")
    print(f"  noun          : {noun}")
    print(f"  adjective     : {adjective}")
    print(f"  NOUN_FILE     : {noun_file}")
    print(f"  ADJ_FILE      : {adj_file}")
    print(f"  NOUN_FOLDER   : {noun_folder}")
    print(f"  ADJ_FOLDER    : {adj_folder}")
    print(f"  countzero > counto : {countzero} > {counto}")

def main() -> None:
    # --------------------------------------------------------------------- #
    # Determine how many names to emit (counto)
    # --------------------------------------------------------------------- #
    try:
        max_count = int(os.getenv("counto", ""))
    except (TypeError, ValueError):
        # Fallback to terminal height (rows).  os.get_terminal_size() may raise OSError
        try:
            max_count = os.get_terminal_size().lines
        except OSError:
            max_count = 24  # sensible default

    if max_count <= 0:
        max_count = 1

    # --------------------------------------------------------------------- #
    # Resolve noun and adjective files (environment overrides possible)
    # --------------------------------------------------------------------- #
    noun_file = resolve_file("NOUN_FILE", NOUN_FOLDER)
    adj_file = resolve_file("ADJ_FILE", ADJ_FOLDER)

    # Load the word lists
    with open(noun_file, "r", encoding="utf-8") as f:
        nouns = [line.strip() for line in f if line.strip()]

    with open(adj_file, "r", encoding="utf-8") as f:
        adjectives = [line.strip() for line in f if line.strip()]

    # --------------------------------------------------------------------- #
    # Emit names
    # --------------------------------------------------------------------- #
    debug_mode = os.getenv("DEBUG", "").lower() == "true"
    countzero = 0
    while countzero < max_count:
        noun = random.choice(nouns).lower()
        adjective = random.choice(adjectives)  # keep original case, like the shell script
        debugger(debug_mode, noun, adjective,
                 noun_file, adj_file,
                 NOUN_FOLDER, ADJ_FOLDER,
                 countzero, max_count)
        print(f"{adjective}{SEPARATOR}{noun}")
        countzero += 1

if __name__ == "__main__":
    main()

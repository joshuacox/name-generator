#!/usr/bin/awk -f
# AWK implementation of name-generator.
#
# Behaves like `name-generator.sh`:
#   - Uses environment variables SEPARATOR, NOUN_FILE, ADJ_FILE,
#     NOUN_FOLDER, ADJ_FOLDER, counto, DEBUG.
#   - If NOUN_FILE / ADJ_FILE are not set, picks a random regular file
#     from the respective folder.
#   - Emits `counto` lines (default: terminal height via `tput lines`,
#     fallback 24).
#   - Noun is lower-cased, adjective keeps original case.
#   - Optional debug output when DEBUG=true.

function pick_random_file(folder,   cmd, f, count, files, idx) {
    cmd = "find \"" folder "\" -type f 2>/dev/null"
    count = 0
    while ((cmd | getline f) > 0) {
        if (length(f) > 0) {
            files[++count] = f
        }
    }
    close(cmd)
    if (count == 0) {
        print "Folder `" folder "` contains no regular files." > "/dev/stderr"
        exit 1
    }
    idx = int(rand() * count) + 1
    return files[idx]
}

function seed_random(  cmd, s) {
    cmd = "od -An -N4 -tu4 /dev/urandom 2>/dev/null"
    if ((cmd | getline s) > 0) {
        close(cmd)
        srand(s + 0)
    } else {
        srand()
    }
}

BEGIN {
    seed_random()

    # Separator
    separator = ("SEPARATOR" in ENVIRON && length(ENVIRON["SEPARATOR"]) > 0) ? ENVIRON["SEPARATOR"] : "-"

    # Folders
    noun_folder = ("NOUN_FOLDER" in ENVIRON && length(ENVIRON["NOUN_FOLDER"]) > 0) ? ENVIRON["NOUN_FOLDER"] : "nouns"
    adj_folder = ("ADJ_FOLDER" in ENVIRON && length(ENVIRON["ADJ_FOLDER"]) > 0) ? ENVIRON["ADJ_FOLDER"] : "adjectives"

    # Files
    if ("NOUN_FILE" in ENVIRON && length(ENVIRON["NOUN_FILE"]) > 0) {
        noun_file = ENVIRON["NOUN_FILE"]
    } else {
        noun_file = pick_random_file(noun_folder)
    }

    if ("ADJ_FILE" in ENVIRON && length(ENVIRON["ADJ_FILE"]) > 0) {
        adj_file = ENVIRON["ADJ_FILE"]
    } else {
        adj_file = pick_random_file(adj_folder)
    }

    # Count
    if ("counto" in ENVIRON && length(ENVIRON["counto"]) > 0) {
        counto = ENVIRON["counto"] + 0
    } else {
        cmd = "tput lines 2>/dev/null"
        if ((cmd | getline tput_val) > 0 && tput_val + 0 > 0) {
            counto = tput_val + 0
        } else {
            counto = 24
        }
        close(cmd)
    }

    # Read noun lines
    n_nouns = 0
    while ((getline line < noun_file) > 0) {
        sub(/^[ \t\r\n]+/, "", line)
        sub(/[ \t\r\n]+$/, "", line)
        if (length(line) > 0) {
            nouns[++n_nouns] = tolower(line)
        }
    }
    close(noun_file)

    if (n_nouns == 0) {
        print "File `" noun_file "` contains no non-empty lines." > "/dev/stderr"
        exit 1
    }

    # Read adjective lines
    n_adjs = 0
    while ((getline line < adj_file) > 0) {
        sub(/^[ \t\r\n]+/, "", line)
        sub(/[ \t\r\n]+$/, "", line)
        if (length(line) > 0) {
            adjectives[++n_adjs] = line
        }
    }
    close(adj_file)

    if (n_adjs == 0) {
        print "File `" adj_file "` contains no non-empty lines." > "/dev/stderr"
        exit 1
    }

    is_debug = ("DEBUG" in ENVIRON && ENVIRON["DEBUG"] == "true")

    for (i = 0; i < counto; i++) {
        n_idx = int(rand() * n_nouns) + 1
        a_idx = int(rand() * n_adjs) + 1
        noun = nouns[n_idx]
        adj = adjectives[a_idx]

        if (is_debug) {
            print adj > "/dev/stderr"
            print noun > "/dev/stderr"
            print adj_file > "/dev/stderr"
            print adj_folder > "/dev/stderr"
            print noun_file > "/dev/stderr"
            print noun_folder > "/dev/stderr"
            print i " > " counto > "/dev/stderr"
        }

        print adj separator noun
    }
    exit 0
}

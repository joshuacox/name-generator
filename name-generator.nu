#!/usr/bin/env nu
# Nushell implementation of name-generator
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

def pick_file [folder: string]: nothing -> string {
    let files = (ls $folder | where type == file | get name)
    if ($files | is-empty) {
        let nested = (glob $"($folder)/**/*" | where { |f| ($f | path type) == "file" })
        if ($nested | is-empty) {
            print -e $"Folder '($folder)' contains no regular files."
            exit 1
        }
        return ($nested | get (random int 0..<($nested | length)))
    }
    return ($files | get (random int 0..<($files | length)))
}

def main [] {
    let separator = ($env.SEPARATOR? | default "-")

    let counto = if ($env.counto? | is-not-empty) {
        $env.counto | into int
    } else {
        try { (tput lines | into int) } catch { 24 }
    }

    let here = ($env.PWD? | default (pwd))
    let noun_folder = ($env.NOUN_FOLDER? | default ([$here "nouns"] | path join))
    let adj_folder = ($env.ADJ_FOLDER? | default ([$here "adjectives"] | path join))

    let noun_file = if ($env.NOUN_FILE? | is-not-empty) {
        $env.NOUN_FILE
    } else {
        pick_file $noun_folder
    }

    let adj_file = if ($env.ADJ_FILE? | is-not-empty) {
        $env.ADJ_FILE
    } else {
        pick_file $adj_folder
    }

    let nouns = (open $noun_file | lines | each { str trim } | where ($it | str length) > 0)
    let adjectives = (open $adj_file | lines | each { str trim } | where ($it | str length) > 0)

    if ($nouns | is-empty) {
        print -e $"File '($noun_file)' contains no non-empty lines."
        exit 1
    }

    if ($adjectives | is-empty) {
        print -e $"File '($adj_file)' contains no non-empty lines."
        exit 1
    }

    let num_nouns = ($nouns | length)
    let num_adjs = ($adjectives | length)
    let is_debug = ($env.DEBUG? == "true")

    for i in 0..<$counto {
        let noun = ($nouns | get (random int 0..<($num_nouns)) | str lowercase)
        let adj = ($adjectives | get (random int 0..<($num_adjs)))

        if $is_debug {
            print -e $adj
            print -e $noun
            print -e $adj_file
            print -e $adj_folder
            print -e $noun_file
            print -e $noun_folder
            print -e $"($i) > ($counto)"
        }

        try {
            print $"($adj)($separator)($noun)"
        } catch {
            exit 0
        }
    }
}

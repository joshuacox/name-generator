#!/usr/bin/env tclsh
# Tcl implementation of name-generator.
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

proc get_env {var fallback} {
    if {[info exists ::env($var)] && [string length $::env($var)] > 0} {
        return $::env($var)
    }
    return $fallback
}

proc get_counto {} {
    if {[info exists ::env(counto)] && [string length $::env(counto)] > 0} {
        if {[string is integer -strict $::env(counto)]} {
            return $::env(counto)
        }
    }
    if {![catch {exec tput lines} out]} {
        set trimmed [string trim $out]
        if {[string is integer -strict $trimmed] && $trimmed > 0} {
            return $trimmed
        }
    }
    return 24
}

proc pick_random_file {folder} {
    set files [glob -nocomplain -directory $folder -types f *]
    if {[llength $files] == 0} {
        set files [glob -nocomplain -directory $folder -types f **/*]
    }
    if {[llength $files] == 0} {
        puts stderr "Folder '$folder' contains no regular files."
        exit 1
    }
    set idx [expr {int(rand() * [llength $files])}]
    return [file normalize [lindex $files $idx]]
}

proc read_non_empty_lines {file_path} {
    if {![file exists $file_path] || ![file isfile $file_path]} {
        puts stderr "File '$file_path' does not exist or is not a regular file."
        exit 1
    }
    set fp [open $file_path r]
    set data [read $fp]
    close $fp
    set lines {}
    foreach line [split $data "\n"] {
        set trimmed [string trim $line]
        if {[string length $trimmed] > 0} {
            lappend lines $trimmed
        }
    }
    if {[llength $lines] == 0} {
        puts stderr "File '$file_path' contains no non-empty lines."
        exit 1
    }
    return $lines
}

# Reseed PRNG
if {[file exists /dev/urandom]} {
    set fp [open /dev/urandom r]
    fconfigure $fp -translation binary
    binary scan [read $fp 4] i seed
    close $fp
    expr {srand($seed)}
} else {
    expr {srand([clock clicks])}
}

set here [pwd]
set separator [get_env SEPARATOR "-"]
set counto [get_counto]
set noun_folder [get_env NOUN_FOLDER [file join $here nouns]]
set adj_folder [get_env ADJ_FOLDER [file join $here adjectives]]

if {[info exists ::env(NOUN_FILE)] && [string length $::env(NOUN_FILE)] > 0} {
    set noun_file [file normalize $::env(NOUN_FILE)]
} else {
    set noun_file [pick_random_file $noun_folder]
}

if {[info exists ::env(ADJ_FILE)] && [string length $::env(ADJ_FILE)] > 0} {
    set adj_file [file normalize $::env(ADJ_FILE)]
} else {
    set adj_file [pick_random_file $adj_folder]
}

set nouns [read_non_empty_lines $noun_file]
set adjectives [read_non_empty_lines $adj_file]
set num_nouns [llength $nouns]
set num_adjs [llength $adjectives]

set is_debug [expr {[get_env DEBUG "false"] eq "true"}]

for {set i 0} {$i < $counto} {incr i} {
    set n_idx [expr {int(rand() * $num_nouns)}]
    set a_idx [expr {int(rand() * $num_adjs)}]
    set noun [string tolower [lindex $nouns $n_idx]]
    set adj [lindex $adjectives $a_idx]

    if {$is_debug} {
        puts stderr $adj
        puts stderr $noun
        puts stderr $adj_file
        puts stderr $adj_folder
        puts stderr $noun_file
        puts stderr $noun_folder
        puts stderr "$i > $counto"
    }

    if {[catch {puts "$adj$separator$noun"}]} {
        exit 0
    }
}

#!/usr/bin/env raku
# Raku implementation of the original name-generator.sh script.
# It respects the environment variables SEPARATOR, NOUN_FILE, ADJ_FILE,
# NOUN_FOLDER, ADJ_FOLDER, counto and DEBUG, providing the same behaviour.

use v6;

# -------------------------------------------------------------------------------
# Helper routines
# -------------------------------------------------------------------------------
sub pick-random-file(Str $folder --> Str) {
    my @files = dir($folder).grep({ .f });
    die "No regular files found in folder '$folder'" unless @files;
    return @files.pick.Str;
}

sub read-lines(Str $file --> Array) {
    return $file.IO.lines.map({ .chomp }).Array;
}

# -------------------------------------------------------------------------------
# Configuration (environment variables with defaults)
# -------------------------------------------------------------------------------
my $here        = $*CWD.Str;
my $separator   = %*ENV<SEPARATOR> // '-';
my $noun-folder = %*ENV<NOUN_FOLDER> // "$here/nouns";
my $adj-folder  = %*ENV<ADJ_FOLDER> // "$here/adjectives";

# Resolve the noun and adjective files (environment overrides or random pick)
my $noun-file = %*ENV<NOUN_FILE> // pick-random-file($noun-folder);
my $adj-file  = %*ENV<ADJ_FILE>  // pick-random-file($adj-folder);

# Load the contents of the selected files once (mirrors shuf -n 1 per iteration)
my @noun-lines = read-lines($noun-file);
my @adj-lines  = read-lines($adj-file);

# Determine how many names to emit.
my $counto = %*ENV<counto> // do {
    my $tput = qx/tput lines/.trim;
    $tput && $tput ~~ /^\d+$/ ?? $tput.Int !! 24;
};

# -------------------------------------------------------------------------------
# Main loop
# -------------------------------------------------------------------------------
my $debug = %*ENV<DEBUG> // '';
for ^$counto -> $i {
    my $noun = @noun-lines.pick.lc;          # lower‑case noun
    my $adj  = @adj-lines.pick;              # adjective as‑is

    # Debug output (mirrors the shell script's debugger function)
    if $debug eq 'true' {
        $*ERR.say("DEBUG:");
        $*ERR.say("  adjective : $adj");
        $*ERR.say("  noun      : $noun");
        $*ERR.say("  ADJ_FILE  : $adj-file");
        $*ERR.say("  ADJ_FOLDER: $adj-folder");
        $*ERR.say("  NOUN_FILE : $noun-file");
        $*ERR.say("  NOUN_FOLDER: $noun-folder");
        $*ERR.say("  countzero > counto");
    }

    # Emit the generated name
    say "$adj$separator$noun";
}

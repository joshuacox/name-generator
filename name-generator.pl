#!/usr/bin/env perl
#=====================================================================
#  Random adjective‑noun generator – Perl 5 rewrite of Josh Cox's sh
#=====================================================================
#  Behaviour (identical to the original):
#   • Pick one random *noun* file from $NOUN_FOLDER   (default: ./nouns)
#   • Pick one random *adjective* file from $ADJ_FOLDER (default: ./adjectives)
#   • Print $counto lines (default: number of rows of the terminal)
#   • Each line is   <adjective><SEPARATOR><noun>
#   • If the environment variable DEBUG is set to the literal string
#     "true", a small debug dump is printed for every iteration.
#=====================================================================

use strict;
use warnings;
use Cwd      qw( getcwd abs_path );
use File::Spec;
use List::Util qw( shuffle );
use Term::ReadKey;          # for terminal size (fallback if tput not found)

#---------------------------------------------------------------------
# Helper – run a command and return its trimmed STDOUT, or undef on error
#---------------------------------------------------------------------
sub run_cmd {
    my ($cmd) = @_;
    my $out = `$cmd`;
    chomp $out if defined $out;
    return $out;
}

#---------------------------------------------------------------------
# Environment defaults (the same “${VAR:=default}” semantics as the shell)
#---------------------------------------------------------------------
my $HERE = getcwd();

my $SEPARATOR = $ENV{SEPARATOR} // '-';

# Number of lines to emit – try `tput lines`, fall back to Term::ReadKey,
# finally default to 24 if nothing works.
my $counto = $ENV{counto};
unless ( defined $counto ) {
    $counto = run_cmd('tput lines');
    unless ( defined $counto && $counto =~ /^\d+$/ ) {
        eval { ReadMode('cbreak'); };
        my ($w, $h) = GetTerminalSize();
        $counto = $h // 24;
    }
}
$counto = int($counto);

my $NOUN_FOLDER = $ENV{NOUN_FOLDER}
                // File::Spec->catdir( $HERE, 'nouns' );
my $ADJ_FOLDER  = $ENV{ADJ_FOLDER}
                // File::Spec->catdir( $HERE, 'adjectives' );

#---------------------------------------------------------------------
# Pick a random file from a directory (only regular files, no recursion)
#---------------------------------------------------------------------
sub pick_random_file {
    my ($dir) = @_;
    opendir my $dh, $dir or die "Cannot open directory '$dir': $!";
    my @files = grep { -f File::Spec->catfile( $dir, $_ ) } readdir $dh;
    closedir $dh;
    die "No regular files found in '$dir'" unless @files;
    my $chosen = $files[ int( rand(@files) ) ];
    return abs_path( File::Spec->catfile( $dir, $chosen ) );
}

my $NOUN_FILE = $ENV{NOUN_FILE}
              // pick_random_file($NOUN_FOLDER);
my $ADJ_FILE  = $ENV{ADJ_FILE}
              // pick_random_file($ADJ_FOLDER);

#---------------------------------------------------------------------
# Load each file once – the original script calls `shuf -n 1` each loop,
# which is effectively “pick a random line”.  Loading the whole file
# into an array is far more efficient in Perl.
#---------------------------------------------------------------------
sub load_words {
    my ($file) = @_;
    open my $fh, '<', $file or die "Cannot open '$file': $!";
    my @words = map { chomp; $_ } <$fh>;
    close $fh;
    die "File '$file' appears to be empty" unless @words;
    return \@words;
}

my $noun_words_ref = load_words($NOUN_FILE);
my $adj_words_ref  = load_words($ADJ_FILE);

#---------------------------------------------------------------------
# Debug helper – prints the same information the original `debugger`
# function emitted when DEBUG=true.
#---------------------------------------------------------------------
sub debugger {
    my ( $adj, $noun ) = @_;
    return unless defined $ENV{DEBUG} && $ENV{DEBUG} eq 'true';
    # Turn on Perl's own trace of executed statements (similar to `set -x`)
    # This is optional; the original script only turned on shell tracing.
    # Here we simply print the variables.
    warn "DEBUG: adjective   = $adj\n";
    warn "DEBUG: noun        = $noun\n";
    warn "DEBUG: ADJ_FILE    = $ADJ_FILE\n";
    warn "DEBUG: ADJ_FOLDER  = $ADJ_FOLDER\n";
    warn "DEBUG: NOUN_FILE   = $NOUN_FILE\n";
    warn "DEBUG: NOUN_FOLDER = $NOUN_FOLDER\n";
    warn "DEBUG: countzero   > $counto\n";
}

#---------------------------------------------------------------------
# Main loop – produce $counto lines
#---------------------------------------------------------------------
for ( my $countzero = 0 ; $countzero < $counto ; $countzero++ ) {
    my $this_noun = $noun_words_ref->[ int( rand(@$noun_words_ref) ) ];
    $this_noun = lc $this_noun;                     # lower‑case noun
    my $this_adj  = $adj_words_ref->[ int( rand(@$adj_words_ref) ) ];

    debugger( $this_adj, $this_noun );

    printf "%s%s%s\n", $this_adj, $SEPARATOR, $this_noun;
}

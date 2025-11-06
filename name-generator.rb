#!/usr/bin/env ruby
# frozen_string_literal: true
#
#  Original shell script by Josh Cox
#  Re‑implemented in Ruby – works the same way, but is a little easier to read
#  and to extend.
#
#  Behaviour
#  ---------
#  * Pick *one* random file from  ./nouns   (or $NOUN_FOLDER)
#  * Pick *one* random file from  ./adjectives (or $ADJ_FOLDER)
#  * For each line of the terminal (default = `tput lines`) print
#        <adjective><separator><noun>
#    where the adjective is taken from the adjective file and the noun
#    (lower‑cased) from the noun file.
#  * If $DEBUG == 'true' a small dump of internal variables is printed.
#
#  Environment variables that can be overridden
#  --------------------------------------------
#  SEPARATOR   – string placed between adjective and noun (default "-")
#  COUNTO      – how many lines to emit (default = terminal height)
#  NOUN_FOLDER – directory that holds noun files      (default "./nouns")
#  ADJ_FOLDER  – directory that holds adjective files (default "./adjectives")
#  NOUN_FILE   – a specific noun file to use (overrides random pick)
#  ADJ_FILE    – a specific adjective file to use (overrides random pick)
#  DEBUG       – set to "true" to enable debug output
#
#  The script is deliberately defensive – if a folder is empty or a file
#  cannot be read it aborts with a clear message instead of producing
#  silent nonsense.

require 'open3'

# ----------------------------------------------------------------------
# Helper: run a command and return its stdout (or nil on error)
def cmd_output(command)
  out, status = Open3.capture2(command)
  status.success? ? out.strip : nil
end

# ----------------------------------------------------------------------
# Configuration – read from ENV, falling back to sensible defaults
HERE          = Dir.pwd
SEPARATOR     = ENV['SEPARATOR'] || '-'

# Number of lines to print – default = terminal height (`tput lines`)
COUNTO        = (ENV['counto'] || cmd_output('tput lines') || '24').to_i

NOUN_FOLDER   = ENV['NOUN_FOLDER'] || File.join(HERE, 'nouns')
ADJ_FOLDER    = ENV['ADJ_FOLDER']  || File.join(HERE, 'adjectives')

# ----------------------------------------------------------------------
# Pick a random file from a folder (or use the one supplied via ENV)
def pick_file(folder, env_var)
  path = ENV[env_var]
  return File.realpath(path) if path && File.file?(path)

  # Find *all* regular files under the folder (recursively)
  candidates = Dir.glob(File.join(folder, '**', '*')).select { |f| File.file?(f) }
  abort "❌ No files found in #{folder.inspect}" if candidates.empty?

  File.realpath(candidates.sample)
end

NOUN_FILE = pick_file(NOUN_FOLDER, 'NOUN_FILE')
ADJ_FILE  = pick_file(ADJ_FOLDER,  'ADJ_FILE')

# ----------------------------------------------------------------------
# Load the two source files once – this is much faster than reading them
# on every iteration (the original shell script re‑shuffled the whole file
# each time, which is unnecessary).
noun_lines = File.readlines(NOUN_FILE, chomp: true)
adj_lines  = File.readlines(ADJ_FILE,  chomp: true)

abort "❌ #{NOUN_FILE} is empty" if noun_lines.empty?
abort "❌ #{ADJ_FILE}  is empty"  if adj_lines.empty?

# ----------------------------------------------------------------------
# Debug helper – prints the same information the original `debugger`
# function printed when $DEBUG == 'true'.
def debug_print(adjective, noun, noun_file, adj_file, noun_folder, adj_folder, count, limit)
  return unless ENV['DEBUG'] == 'true'

  puts "DEBUG:"
  puts "  adjective   = #{adjective}"
  puts "  noun        = #{noun}"
  puts "  ADJ_FILE    = #{adj_file}"
  puts "  ADJ_FOLDER  = #{adj_folder}"
  puts "  NOUN_FILE   = #{noun_file}"
  puts "  NOUN_FOLDER = #{noun_folder}"
  puts "  #{count} > #{limit}"
  puts "-" * 40
end

# ----------------------------------------------------------------------
# Main loop – print exactly COUNTO lines (or fewer if the terminal is
# smaller than the requested count).
count = 0
while count < COUNTO
  this_noun       = noun_lines.sample.downcase
  this_adjective  = adj_lines.sample

  debug_print(this_adjective, this_noun,
              NOUN_FILE, ADJ_FILE,
              NOUN_FOLDER, ADJ_FOLDER,
              count + 1, COUNTO)

  puts "#{this_adjective}#{SEPARATOR}#{this_noun}"
  count += 1
end

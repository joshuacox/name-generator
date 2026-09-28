#!/usr/bin/env crystal
# Crystal implementation of the name-generator script.
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

require "process"

def cmd_output(command : String) : String?
  Process.run(command, shell: true, output: Process::Redirect::Pipe) do |proc|
    out = proc.output.gets_to_end.strip
    proc.wait.success? ? out : nil
  end
rescue
  nil
end

def pick_file(folder : String, env_var : String) : String
  if (path = ENV[env_var]?) && !path.empty?
    return File.realpath(path) if File.file?(path)
    STDERR.puts "Environment variable #{env_var} points to a non-regular file: #{path}"
    exit 1
  end

  candidates = [] of String
  if Dir.exists?(folder)
    Dir.glob(File.join(folder, "**", "*")) do |f|
      candidates << f if File.file?(f)
    end
  end

  if candidates.empty?
    STDERR.puts "Folder #{folder.inspect} contains no regular files."
    exit 1
  end

  File.realpath(candidates.sample)
end

def read_lines_stripped(path : String) : Array(String)
  if !File.file?(path)
    STDERR.puts "File #{path.inspect} does not exist or is not a regular file."
    exit 1
  end

  lines = [] of String
  File.each_line(path) do |line|
    trimmed = line.strip
    lines << trimmed unless trimmed.empty?
  end

  if lines.empty?
    STDERR.puts "File #{path.inspect} contains no non-empty lines."
    exit 1
  end

  lines
end

separator = ENV["SEPARATOR"]? || "-"
counto_env = ENV["counto"]?

counto = if counto_env && !counto_env.empty?
           counto_env.to_i? || 24
         elsif (tput_val = cmd_output("tput lines")) && !tput_val.empty?
           tput_val.to_i? || 24
         else
           24
         end

here = Dir.current
noun_folder = ENV["NOUN_FOLDER"]? || File.join(here, "nouns")
adj_folder = ENV["ADJ_FOLDER"]? || File.join(here, "adjectives")

noun_file = pick_file(noun_folder, "NOUN_FILE")
adj_file = pick_file(adj_folder, "ADJ_FILE")

nouns = read_lines_stripped(noun_file)
adjectives = read_lines_stripped(adj_file)

is_debug = ENV["DEBUG"]? == "true"

counto.times do |i|
  noun = nouns.sample.downcase
  adjective = adjectives.sample

  if is_debug
    STDERR.puts "#{adjective}"
    STDERR.puts "#{noun}"
    STDERR.puts "#{adj_file}"
    STDERR.puts "#{adj_folder}"
    STDERR.puts "#{noun_file}"
    STDERR.puts "#{noun_folder}"
    STDERR.puts "#{i} > #{counto}"
  end

  puts "#{adjective}#{separator}#{noun}"
end

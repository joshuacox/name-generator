#!/usr/bin/env elixir
# Elixir implementation of the original `name-generator.sh` script.
# It respects the same environment variables:
#   SEPARATOR, counto, NOUN_FOLDER, ADJ_FOLDER, NOUN_FILE, ADJ_FILE, DEBUG

defmodule NameGenerator do
  @default_separator "-"
  @default_fallback_lines 24

  # Entry point
  def main do
    cwd = File.cwd!()

    separator = System.get_env("SEPARATOR") || @default_separator

    counto =
      case System.get_env("counto") do
        nil -> terminal_lines()
        val -> parse_int(val, terminal_lines())
      end

    noun_folder = System.get_env("NOUN_FOLDER") || Path.join(cwd, "nouns")
    adj_folder = System.get_env("ADJ_FOLDER") || Path.join(cwd, "adjectives")

    noun_file = resolve_file("NOUN_FILE", noun_folder)
    adj_file = resolve_file("ADJ_FILE", adj_folder)

    noun_lines = read_nonempty_lines(noun_file)
    adj_lines = read_nonempty_lines(adj_file)

    0..(counto - 1)
    |> Enum.each(fn countzero ->
      noun = noun_lines |> Enum.random() |> String.downcase()
      adjective = adj_lines |> Enum.random()

      maybe_debug(countzero, counto, adjective, noun, noun_file, adj_file, noun_folder, adj_folder)

      IO.puts("#{adjective}#{separator}#{noun}")
    end)
  end

  # Resolve a file path from an environment variable or pick a random file from a folder.
  defp resolve_file(env_var, folder) do
    case System.get_env(env_var) do
      nil ->
        folder
        |> Path.expand()
        |> list_regular_files()
        |> Enum.random()
        |> Path.expand()

      path ->
        Path.expand(path)
    end
  end

  # List regular files (non‑directories) in a folder.
  defp list_regular_files(folder) do
    case File.ls(folder) do
      {:ok, entries} ->
        entries
        |> Enum.map(&Path.join(folder, &1))
        |> Enum.filter(&File.regular?/1)

      {:error, reason} ->
        raise "Cannot list folder #{folder}: #{reason}"
    end
  end

  # Read a file and return a list of non‑empty, trimmed lines.
  defp read_nonempty_lines(file_path) do
    file_path
    |> File.read!()
    |> String.split("\n", trim: true)
    |> Enum.filter(&(&1 != ""))
  end

  # Parse an integer, falling back to `fallback` on error.
  defp parse_int(str, fallback) do
    case Integer.parse(str) do
      {int, _} -> int
      :error -> fallback
    end
  end

  # Determine terminal height using `tput lines`; fall back to a constant.
  defp terminal_lines do
    case System.cmd("tput", ["lines"], stderr_to_stdout: true) do
      {output, 0} ->
        output
        |> String.trim()
        |> parse_int(@default_fallback_lines)

      {_output, _code} ->
        @default_fallback_lines
    end
  end

  # Print debug information when DEBUG=true.
  defp maybe_debug(countzero, counto, adjective, noun, noun_file, adj_file, noun_folder, adj_folder) do
    if System.get_env("DEBUG") == "true" do
      IO.puts(adjective)
      IO.puts(noun)
      IO.puts(adj_file)
      IO.puts(adj_folder)
      IO.puts(noun_file)
      IO.puts(noun_folder)
      IO.puts("#{countzero} > #{counto}")
    end
  end
end

NameGenerator.main()

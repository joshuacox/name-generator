// V implementation of name-generator
//
// Behaves like `name-generator.sh`:
//   - Uses environment variables SEPARATOR, NOUN_FILE, ADJ_FILE,
//     NOUN_FOLDER, ADJ_FOLDER, counto, DEBUG.
//   - If NOUN_FILE / ADJ_FILE are not set, picks a random regular file
//     from the respective folder.
//   - Emits `counto` lines (default: terminal height via `tput lines`,
//     fallback 24).
//   - Noun is lower-cased, adjective keeps original case.
//   - Optional debug output when DEBUG=true.

import os
import rand

fn get_env_or_default(key string, fallback string) string {
	val := os.getenv(key)
	if val.len > 0 {
		return val
	}
	return fallback
}

fn pick_random_file(folder string) string {
	if !os.is_dir(folder) {
		eprintln('Folder "${folder}" does not exist.')
		exit(1)
	}

	entries := os.ls(folder) or {
		eprintln('Cannot list folder "${folder}".')
		exit(1)
	}

	mut files := []string{}
	for entry in entries {
		full_path := os.join_path(folder, entry)
		if os.is_file(full_path) {
			files << full_path
		}
	}

	if files.len == 0 {
		eprintln('Folder "${folder}" contains no regular files.')
		exit(1)
	}

	return rand.element(files) or { files[0] }
}

fn read_lines_stripped(path string, lowercase bool) []string {
	if !os.is_file(path) {
		eprintln('File "${path}" does not exist or is not a regular file.')
		exit(1)
	}

	raw_lines := os.read_lines(path) or {
		eprintln('Cannot read file "${path}".')
		exit(1)
	}

	mut lines := []string{}
	for raw in raw_lines {
		trimmed := raw.trim_space()
		if trimmed.len > 0 {
			if lowercase {
				lines << trimmed.to_lower()
			} else {
				lines << trimmed
			}
		}
	}

	if lines.len == 0 {
		eprintln('File "${path}" contains no non-empty lines.')
		exit(1)
	}

	return lines
}

fn main() {
	separator := get_env_or_default('SEPARATOR', '-')
	noun_folder := get_env_or_default('NOUN_FOLDER', 'nouns')
	adj_folder := get_env_or_default('ADJ_FOLDER', 'adjectives')

	noun_file_env := os.getenv('NOUN_FILE')
	noun_file := if noun_file_env.len > 0 {
		noun_file_env
	} else {
		pick_random_file(noun_folder)
	}

	adj_file_env := os.getenv('ADJ_FILE')
	adj_file := if adj_file_env.len > 0 {
		adj_file_env
	} else {
		pick_random_file(adj_folder)
	}

	counto_env := os.getenv('counto')
	mut counto := 24
	if counto_env.len > 0 {
		counto = counto_env.int()
		if counto <= 0 {
			counto = 24
		}
	} else {
		res := os.execute('tput lines')
		if res.exit_code == 0 {
			tput_val := res.output.trim_space().int()
			if tput_val > 0 {
				counto = tput_val
			}
		}
	}

	is_debug := get_env_or_default('DEBUG', 'false') == 'true'

	nouns := read_lines_stripped(noun_file, true)
	adjectives := read_lines_stripped(adj_file, false)

	for i in 0 .. counto {
		noun := rand.element(nouns) or { nouns[0] }
		adj := rand.element(adjectives) or { adjectives[0] }

		if is_debug {
			eprintln('${adj}\n${noun}\n${adj_file}\n${adj_folder}\n${noun_file}\n${noun_folder}\n${i} > ${counto}')
		}

		println('${adj}${separator}${noun}')
	}
}

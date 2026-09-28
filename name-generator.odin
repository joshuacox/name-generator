package main

import "core:fmt"
import "core:os"
import "core:strings"
import "core:math/rand"
import "core:strconv"
import "core:time"

get_env_or_default :: proc(key, fallback: string) -> string {
	val, ok := os.lookup_env(key, context.allocator)
	if ok && len(val) > 0 {
		return val
	}
	return fallback
}

pick_random_file :: proc(folder: string) -> string {
	f, err := os.open(folder)
	if err != nil {
		fmt.eprintf("Cannot open folder '%s'\n", folder)
		os.exit(1)
	}
	defer os.close(f)

	infos, read_err := os.read_dir(f, -1, context.allocator)
	if read_err != nil {
		fmt.eprintf("Cannot read folder '%s'\n", folder)
		os.exit(1)
	}
	defer os.file_info_slice_delete(infos, context.allocator)

	files := make([dynamic]string)
	defer delete(files)

	for fi in infos {
		if fi.type == .Regular {
			append(&files, fi.fullpath)
		}
	}

	if len(files) == 0 {
		fmt.eprintf("Folder '%s' contains no regular files\n", folder)
		os.exit(1)
	}

	idx := rand.int_max(len(files))
	return strings.clone(files[idx])
}

read_lines :: proc(path: string, lowercase: bool) -> []string {
	data, err := os.read_entire_file(path, context.allocator)
	if err != nil {
		fmt.eprintf("Cannot read file '%s'\n", path)
		os.exit(1)
	}

	raw_lines := strings.split_lines(string(data))
	lines := make([dynamic]string)

	for line in raw_lines {
		trimmed := strings.trim_space(line)
		if len(trimmed) > 0 {
			if lowercase {
				append(&lines, strings.to_lower(trimmed))
			} else {
				append(&lines, strings.clone(trimmed))
			}
		}
	}

	if len(lines) == 0 {
		fmt.eprintf("File '%s' contains no non-empty lines\n", path)
		os.exit(1)
	}

	return lines[:]
}

main :: proc() {
	rand.reset(u64(time.to_unix_nanoseconds(time.now())))

	separator := get_env_or_default("SEPARATOR", "-")
	noun_folder := get_env_or_default("NOUN_FOLDER", "nouns")
	adj_folder := get_env_or_default("ADJ_FOLDER", "adjectives")

	noun_file_env, has_noun := os.lookup_env("NOUN_FILE", context.allocator)
	noun_file: string
	if has_noun && len(noun_file_env) > 0 {
		noun_file = noun_file_env
	} else {
		noun_file = pick_random_file(noun_folder)
	}

	adj_file_env, has_adj := os.lookup_env("ADJ_FILE", context.allocator)
	adj_file: string
	if has_adj && len(adj_file_env) > 0 {
		adj_file = adj_file_env
	} else {
		adj_file = pick_random_file(adj_folder)
	}

	counto_env, has_counto := os.lookup_env("counto", context.allocator)
	counto := 24
	if has_counto && len(counto_env) > 0 {
		val, ok := strconv.parse_int(strings.trim_space(counto_env), 10)
		if ok {
			counto = val
		}
	}

	is_debug := get_env_or_default("DEBUG", "false") == "true"

	nouns := read_lines(noun_file, true)
	adjectives := read_lines(adj_file, false)

	for i in 0..<counto {
		noun := nouns[rand.int_max(len(nouns))]
		adj := adjectives[rand.int_max(len(adjectives))]

		if is_debug {
			fmt.eprintf("%s\n%s\n%s\n%s\n%s\n%s\n%d > %d\n", adj, noun, adj_file, adj_folder, noun_file, noun_folder, i, counto)
		}

		fmt.printf("%s%s%s\n", adj, separator, noun)
	}
}

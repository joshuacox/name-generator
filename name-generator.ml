#!/usr/bin/env -S ocaml -I +unix unix.cma
(* --------------------------------------------------------
   OCaml implementation of the “name‑generator” behaviour described in
   name-generator.sh.

   Features
   --------
   * SEPARATOR   – defaults to " " (space) for a more casual output
   * COUNT_O     – number of lines to emit.
                   * env var “counto” (int) overrides,
                   * otherwise the result of `tput lines`,
                   * otherwise 24.
   * NOUN_FILE   – env var overrides, otherwise a random regular file
                   from NOUN_FOLDER (default "./nouns").
   * ADJ_FILE    – same logic, using ADJ_FOLDER (default "./adjectives").
   * DEBUG       – when set to "true" prints debugging information to
                   stderr (mirrors the shell script).

   The program prints COUNT_O lines of the form:
        <adjective><SEPARATOR><noun>
   where the noun is lower‑cased and the adjective keeps its original case.
   ------------------------------------------------------ *)

open Unix
open Printf

(* ---------- helpers --------------------------------------------------- *)

let env_opt name = try Some (Sys.getenv name) with Not_found -> None

let env_or_default name ~default =
  match env_opt name with
  | Some v when String.trim v <> "" -> v
  | _ -> default

let debug_enabled () =
  match env_opt "DEBUG" with
  | Some "true" -> true
  | _ -> false

(* Run `tput lines` and return the integer result, or raise if it fails. *)
let tput_lines () =
  let ic = Unix.open_process_in "tput lines 2>/dev/null" in
  let line = try input_line ic with End_of_file -> "" in
  let _ = Unix.close_process_in ic in
  int_of_string_opt (String.trim line)

(* Determine how many lines we should emit. *)
let get_count_o () =
  match env_opt "counto" with
  | Some s -> (try int_of_string (String.trim s) with Failure _ -> 24)
  | None -> (
      match tput_lines () with
      | Some n -> n
      | None -> 24)

(* Return a list of regular files (full absolute paths) inside [dir]. *)
let list_regular_files dir =
  let dh = Unix.opendir dir in
  let rec loop acc =
    match Unix.readdir dh with
    | entry when entry = "." || entry = ".." -> loop acc
    | entry ->
        let path = Filename.concat dir entry |> Unix.realpath in
        if (Unix.stat path).st_kind = S_REG then loop (path :: acc) else loop acc
    | exception End_of_file ->
        Unix.closedir dh;
        List.rev acc
  in
  loop []

(* Pick a random element from a non‑empty list. *)
let random_choice lst =
  let len = List.length lst in
  List.nth lst (Random.int len)

(* Resolve a file path from an environment variable or, if not set,
   pick a random regular file from the given folder. *)
let resolve_file ~var ~folder =
  match env_opt var with
  | Some v when String.trim v <> "" ->
      let p = Unix.realpath (String.trim v) in
      if (Unix.stat p).st_kind <> S_REG then
        failwith (sprintf "Environment variable %s points to a non‑regular file: %s" var v)
      else p
  | _ ->
      let files = list_regular_files folder in
      if files = [] then
        failwith (sprintf "Folder %s does not contain any regular files" folder)
      else random_choice files

(* Read all non‑empty, trimmed lines from a file. *)
let read_nonempty_lines file_path =
  let ic = open_in file_path in
  let rec loop acc =
    match input_line ic with
    | line ->
        let trimmed = String.trim line in
        let acc' = if trimmed = "" then acc else trimmed :: acc in
        loop acc'
    | exception End_of_file ->
        close_in ic;
        List.rev acc
  in
  loop []

(* ---------- main ------------------------------------------------------ *)

let () =
  Random.self_init ();

  (* configuration ------------------------------------------------------ *)
  (* Default separator changed to a space for a more casual output *)
  let separator = env_or_default "SEPARATOR" ~default:" " in
  let count_o = get_count_o () in

  let cwd = Sys.getcwd () in
  let noun_folder = env_or_default "NOUN_FOLDER" ~default:(Filename.concat cwd "nouns") in
  let adj_folder  = env_or_default "ADJ_FOLDER"  ~default:(Filename.concat cwd "adjectives") in

  let noun_file = resolve_file ~var:"NOUN_FILE" ~folder:noun_folder in
  let adj_file  = resolve_file ~var:"ADJ_FILE"  ~folder:adj_folder in

  let noun_lines = read_nonempty_lines noun_file in
  let adj_lines  = read_nonempty_lines adj_file in

  if noun_lines = [] then failwith (sprintf "Noun file %s is empty" noun_file);
  if adj_lines  = [] then failwith (sprintf "Adjective file %s is empty" adj_file);

  (* generation loop ---------------------------------------------------- *)
  for i = 0 to count_o - 1 do
    let noun_raw = random_choice noun_lines in
    let noun = String.lowercase_ascii noun_raw in
    let adjective = random_choice adj_lines in

    if debug_enabled () then (
      eprintf "DEBUG:\n";
      eprintf "  adjective : %s\n" adjective;
      eprintf "  noun      : %s\n" noun;
      eprintf "  ADJ_FILE  : %s\n" adj_file;
      eprintf "  ADJ_FOLDER: %s\n" adj_folder;
      eprintf "  NOUN_FILE : %s\n" noun_file;
      eprintf "  NOUN_FOLDER: %s\n" noun_folder;
      eprintf "  %d > %d\n%!" i count_o
    );

    printf "%s%s%s\n%!" adjective separator noun
  done

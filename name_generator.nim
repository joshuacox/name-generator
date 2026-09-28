#!/usr/bin/env -S nim r --hints:off
## Nim implementation of the name-generator script.
##
## Behaves like `name-generator.sh`:
##   - Uses environment variables SEPARATOR, NOUN_FILE, ADJ_FILE,
##     NOUN_FOLDER, ADJ_FOLDER, counto, DEBUG.
##   - If NOUN_FILE / ADJ_FILE are not set, picks a random regular file
##     from the respective folder.
##   - Emits `counto` lines (default: terminal height via `tput lines`,
##     fallback 24).
##   - Noun is lower-cased, adjective keeps original case.
##   - Optional debug output when DEBUG=true.
import std/[os, osproc, strutils, random]
proc getEnvOrDefault(key, fallback: string): string =
  let val = getEnv(key)
  if val.len > 0: val else: fallback
proc parseIntOr(s: string, fallback: int): int =
  try:
    parseInt(s)
  except ValueError:
    fallback
proc getCountO(): int =
  let envVal = getEnv("counto")
  if envVal.len > 0:
    return parseIntOr(envVal, 24)
  # Fallback to `tput lines`
  try:
    let (output, exitCode) = execCmdEx("tput lines")
    if exitCode == 0:
      return parseIntOr(output.strip(), 24)
  except CatchableError:
    discard
  return 24
proc pickRandomFile(dirPath: string): string =
  var files: seq[string] = @[]
  if dirExists(dirPath):
    for kind, path in walkDir(dirPath):
      if kind == pcFile:
        files.add(path)
  if files.len == 0:
    raise newException(IOError, "Folder `" & dirPath & "` contains no regular files.")
  return sample(files)
proc readNonEmptyLines(filePath: string): seq[string] =
  var lines: seq[string] = @[]
  if not fileExists(filePath):
    raise newException(IOError, "File does not exist: " & filePath)
  for line in lines(filePath):
    let stripped = line.strip()
    if stripped.len > 0:
      lines.add(stripped)
  if lines.len == 0:
    raise newException(IOError, "File `" & filePath & "` contains no non-empty lines.")
  return lines

proc maybeDebug(adjective, noun, nounFile, adjFile, nounFolder, adjFolder: string,
                iteration, counto: int) =
  if getEnv("DEBUG") == "true":
    stderr.writeLine("DEBUG iteration " & $iteration & "/" & $counto)
    stderr.writeLine("  adjective  : " & adjective)
    stderr.writeLine("  noun       : " & noun)
    stderr.writeLine("  NOUN_FILE  : " & nounFile)
    stderr.writeLine("  ADJ_FILE   : " & adjFile)
    stderr.writeLine("  NOUN_FOLDER: " & nounFolder)
    stderr.writeLine("  ADJ_FOLDER : " & adjFolder)
proc main() =
  randomize()
  let here = getCurrentDir()
  let separator = getEnvOrDefault("SEPARATOR", "-")
  let nounFolder = absolutePath(getEnvOrDefault("NOUN_FOLDER", here / "nouns"))
  let adjFolder = absolutePath(getEnvOrDefault("ADJ_FOLDER", here / "adjectives"))
  var nounFile = getEnv("NOUN_FILE")
  if nounFile.len > 0:
    nounFile = absolutePath(nounFile)
  else:
    nounFile = pickRandomFile(nounFolder)
  var adjFile = getEnv("ADJ_FILE")
  if adjFile.len > 0:
    adjFile = absolutePath(adjFile)
  else:
    adjFile = pickRandomFile(adjFolder)
  let nouns = readNonEmptyLines(nounFile)
  let adjectives = readNonEmptyLines(adjFile)
  let counto = getCountO()
  for i in 0 ..< counto:
    let noun = sample(nouns).toLowerAscii()
    let adjective = sample(adjectives)
    maybeDebug(adjective, noun, nounFile, adjFile, nounFolder, adjFolder, i, counto)
    stdout.writeLine(adjective & separator & noun)
when isMainModule:
  main()

// initial coversion from shell by gpt-oss:120b
// namegen.go
//
// Go implementation of the shell script posted by Josh Cox.
// Prints random "<adjective>-<noun>" strings, one per line.
// Debug output (when DEBUG=true) is sent to stderr.

package main

import (
    "bufio"
    "fmt"
    "log"
    "math/rand"
    "os"
    "path/filepath"
    "strconv"          // <‑‑ **added**
    "strings"
    "time"

    "golang.org/x/term"
)

// -------------------------------------------------------------------
// Helper utilities
// -------------------------------------------------------------------

// getEnv returns the value of an environment variable or a fallback if it is not set.
func getEnv(key, fallback string) string {
    if v, ok := os.LookupEnv(key); ok && v != "" {
        return v
    }
    return fallback
}

// listFiles returns absolute paths of all regular files directly under dir.
func listFiles(dir string) ([]string, error) {
    entries, err := os.ReadDir(dir)
    if err != nil {
        return nil, err
    }
    var files []string
    for _, e := range entries {
        if e.Type().IsRegular() {
            abs, err := filepath.Abs(filepath.Join(dir, e.Name()))
            if err != nil {
                return nil, err
            }
            files = append(files, abs)
        }
    }
    if len(files) == 0 {
        return nil, fmt.Errorf("no regular files found in %s", dir)
    }
    return files, nil
}

// readLines reads the whole file into a slice (one element per line).
func readLines(path string) ([]string, error) {
    f, err := os.Open(path)
    if err != nil {
        return nil, err
    }
    defer f.Close()

    var lines []string
    scanner := bufio.NewScanner(f)
    for scanner.Scan() {
        lines = append(lines, scanner.Text())
    }
    if err = scanner.Err(); err != nil {
        return nil, err
    }
    if len(lines) == 0 {
        return nil, fmt.Errorf("file %s is empty", path)
    }
    return lines, nil
}

// pickRandom returns a random element from a non‑empty slice.
func pickRandom[T any](s []T) T {
    return s[rand.Intn(len(s))]
}

// termHeight returns the current terminal height (rows) or 24 as a fallback.
func termHeight() int {
    if _, h, err := term.GetSize(int(os.Stdout.Fd())); err == nil && h > 0 {
        return h
    }
    return 24
}

// lowerCase is a thin wrapper around strings.ToLower (keeps the original intent clear).
func lowerCase(s string) string { return strings.ToLower(s) }

// debugger prints the same info the original shell script emitted when DEBUG=true.
func debugger(thisAdj, thisNoun, adjFile, adjFolder, nounFile, nounFolder string, countZero, countO int) {
    if os.Getenv("DEBUG") != "true" {
        return
    }
    fmt.Fprintln(os.Stderr, "DEBUG:")
    fmt.Fprintln(os.Stderr, "  adjective :", thisAdj)
    fmt.Fprintln(os.Stderr, "  noun      :", thisNoun)
    fmt.Fprintln(os.Stderr, "  ADJ_FILE  :", adjFile)
    fmt.Fprintln(os.Stderr, "  ADJ_FOLDER:", adjFolder)
    fmt.Fprintln(os.Stderr, "  NOUN_FILE :", nounFile)
    fmt.Fprintln(os.Stderr, "  NOUN_FOLDER:", nounFolder)
    fmt.Fprintf(os.Stderr, "  %d > %d\n", countZero, countO)
    fmt.Fprintln(os.Stderr, "---")
}

// strconvAtoi converts a string to int and returns a nice error if it fails.
func strconvAtoi(s string) (int, error) {
    i, err := strconv.Atoi(strings.TrimSpace(s))
    if err != nil {
        return 0, fmt.Errorf("cannot convert %q to integer: %w", s, err)
    }
    return i, nil
}

// -------------------------------------------------------------------
// main()
// -------------------------------------------------------------------
func main() {
    // Seed the RNG once.
    rand.Seed(time.Now().UnixNano())

    // ----- 1️⃣  Determine defaults (environment overrides possible) -----
    here, err := os.Getwd()
    if err != nil {
        log.Fatalf("cannot determine current directory: %v", err)
    }

    // Terminal height is the default for counto.
    defaultCount := termHeight()
    countoStr := getEnv("counto", fmt.Sprintf("%d", defaultCount))
    counto, err := strconvAtoi(countoStr)
    if err != nil || counto < 1 {
        log.Fatalf("invalid counto value %q: %v", countoStr, err)
    }

    nounFolder := getEnv("NOUN_FOLDER", filepath.Join(here, "nouns"))
    adjFolder := getEnv("ADJ_FOLDER", filepath.Join(here, "adjectives"))

    // ----- 2️⃣  Pick one random file from each folder -----
    nounFiles, err := listFiles(nounFolder)
    if err != nil {
        log.Fatalf("error listing noun files: %v", err)
    }
    adjFiles, err := listFiles(adjFolder)
    if err != nil {
        log.Fatalf("error listing adjective files: %v", err)
    }
    // Environment can force a specific file; otherwise pick a random one.
    nounFile := getEnv("NOUN_FILE", pickRandom(nounFiles))
    adjFile := getEnv("ADJ_FILE", pickRandom(adjFiles))

    // Resolve to absolute paths (mirrors `realpath` in the shell script).
    nounFile, err = filepath.Abs(nounFile)
    if err != nil {
        log.Fatalf("cannot resolve noun file path: %v", err)
    }
    adjFile, err = filepath.Abs(adjFile)
    if err != nil {
        log.Fatalf("cannot resolve adjective file path: %v", err)
    }

    // ----- 3️⃣  Load the two files into memory -----
    nounLines, err := readLines(nounFile)
    if err != nil {
        log.Fatalf("cannot read nouns from %s: %v", nounFile, err)
    }
    adjLines, err := readLines(adjFile)
    if err != nil {
        log.Fatalf("cannot read adjectives from %s: %v", adjFile, err)
    }

    // ----- 4️⃣  Produce `counto` random names -----
    for i := 0; i < counto; i++ {
        thisNoun := lowerCase(pickRandom(nounLines))
        thisAdj := pickRandom(adjLines)
        name := fmt.Sprintf("%s-%s", thisAdj, thisNoun)

        // Debug output goes to stderr.
        debugger(thisAdj, thisNoun, adjFile, adjFolder, nounFile, nounFolder, i, counto)

        // Normal output goes to stdout.
        fmt.Println(name)
    }
}

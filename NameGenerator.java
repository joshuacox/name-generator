// original conversion by gpt-oss:120b
import java.io.IOException;
import java.nio.file.*;
import java.util.*;
import java.util.stream.*;

public class NameGenerator {

    // -------------------------------------------------------------------------------//
    // Configuration – values are taken from the environment, otherwise a default
    // -------------------------------------------------------------------------------//
    private static final Path HERE = Paths.get("").toAbsolutePath();

    private static final int COUNT_O = getCountO();                     // number of lines to emit
    private static final Path NOUN_FOLDER = getPathEnv("NOUN_FOLDER", HERE.resolve("nouns"));
    private static final Path ADJ_FOLDER  = getPathEnv("ADJ_FOLDER",  HERE.resolve("adjectives"));
    private static final String SEPARATOR = System.getenv().getOrDefault("SEPARATOR", "-");
    private static final boolean DEBUG = "true".equalsIgnoreCase(System.getenv("DEBUG"));

    // The file paths may throw IOException during selection, so we initialise them in a static block.
    private static final Path NOUN_FILE;
    private static final Path ADJ_FILE;

    static {
        try {
            NOUN_FILE = getFileEnvOrRandom("NOUN_FILE", NOUN_FOLDER);
            ADJ_FILE  = getFileEnvOrRandom("ADJ_FILE",  ADJ_FOLDER);
        } catch (IOException e) {
            // Convert the checked exception into an unchecked one so that static initialisation can fail cleanly.
            throw new IllegalStateException("Failed to select noun or adjective file", e);
        }
    }

    // -------------------------------------------------------------------------------//
    // Main entry point
    // -------------------------------------------------------------------------------//
    public static void main(String[] args) {
        try {
            // 1️⃣ Load the whole content of the selected files (so we can pick a random line fast)
            List<String> nouns = readAllLines(NOUN_FILE);
            List<String> adjs  = readAllLines(ADJ_FILE);

            // 2️⃣ Produce the output
            Random rnd = new Random();
            for (int i = 0; i < COUNT_O; i++) {
                String noun = nouns.get(rnd.nextInt(nouns.size())).toLowerCase(Locale.ROOT);
                String adj  = adjs.get(rnd.nextInt(adjs.size()));

                String name = String.format("%s%s%s", adj, SEPARATOR, noun);
                debugPrint(i, NOUN_FILE, ADJ_FILE, noun, adj, name);
                System.out.println(name);
            }
        } catch (IOException e) {
            System.err.println("❌  I/O error: " + e.getMessage());
            System.exit(1);
        } catch (IllegalStateException e) {
            System.err.println("❌  Configuration error: " + e.getMessage());
            System.exit(2);
        }
    }

    // -------------------------------------------------------------------------------//
    // Helper methods
    // -------------------------------------------------------------------------------//

    /** Returns the number of lines the original script would have printed.
     *  It tries to ask the terminal for its height (via the JNA‑based
     *  `stty size` command).  If that fails, it falls back to the value of the
     *  environment variable `counto` or finally to a safe default of 24. */
    private static int getCountO() {
        // 1️⃣ Try to get the terminal height using `tput lines` (the same command
        //    the shell script uses).  If we cannot run it, ignore the exception.

        // 2️⃣ Check a user‑supplied env‑var (the script never used it, but it can be handy)
        String env = System.getenv("counto");
        if (env != null) {
            try {
                return Integer.parseInt(env);
            } catch (NumberFormatException ignored) {
            }
        }
        try {
            Process p = new ProcessBuilder("tput", "lines")
                    .redirectError(ProcessBuilder.Redirect.DISCARD)
                    .start();
            try (Scanner sc = new Scanner(p.getInputStream())) {
                if (sc.hasNextInt()) {
                    return sc.nextInt();
                }
            }
            p.waitFor();
        } catch (Exception ignored) {
        }

        // 3️⃣ Default
        return 24;
    }

    /** Returns the value of an environment variable interpreted as a {@link Path},
     *  or the supplied default if the variable is not set or empty. */
    private static Path getPathEnv(String varName, Path defaultPath) {
        String val = System.getenv(varName);
        return (val == null || val.isBlank()) ? defaultPath : Paths.get(val).toAbsolutePath();
    }

    /** Returns the path to a file to be used (either from an env‑var or a random
     *  regular file from the given folder). */
    private static Path getFileEnvOrRandom(String varName, Path folder) throws IOException {
        String val = System.getenv(varName);
        if (val != null && !val.isBlank()) {
            Path p = Paths.get(val).toAbsolutePath();
            if (!Files.isRegularFile(p)) {
                throw new IllegalStateException("Environment variable " + varName + " points to a non‑regular file: " + p);
            }
            return p;
        }
        // Fallback to a random regular file from the folder (same behaviour as the shell script)
        return pickRandomFile(folder);
    }

    /** Picks a random regular file (not a directory) from the given folder.
     *  Throws {@link IllegalStateException} if the folder does not exist or
     *  contains no regular files. */
    private static Path pickRandomFile(Path folder) throws IOException {
        try (Stream<Path> stream = Files.list(folder)) {
            List<Path> files = stream
                    .filter(Files::isRegularFile)
                    .collect(Collectors.toList());

            if (files.isEmpty()) {
                throw new IllegalStateException("Folder " + folder + " does not contain any regular files");
            }
            return files.get(new Random().nextInt(files.size()));
        }
    }

    /** Reads *all* lines from a file using UTF‑8.  Empty lines are kept – the
     *  original shell script would have treated them as valid candidates as well. */
    private static List<String> readAllLines(Path file) throws IOException {
        return Files.readAllLines(file);
    }

    /** Prints the same diagnostic information that the original `debugger`
     *  function printed, but only when `DEBUG=true`. */
    private static void debugPrint(int iteration,
                                   Path nounFile,
                                   Path adjFile,
                                   String noun,
                                   String adj,
                                   String combined) {
        if (!DEBUG) {
            return;
        }
        System.err.println("=== DEBUG ITERATION " + iteration + " ===");
        System.err.println("adjective   = " + adj);
        System.err.println("noun        = " + noun);
        System.err.println("combined    = " + combined);
        System.err.println("ADJ_FILE    = " + adjFile);
        System.err.println("ADJ_FOLDER  = " + ADJ_FOLDER);
        System.err.println("NOUN_FILE   = " + nounFile);
        System.err.println("NOUN_FOLDER = " + NOUN_FOLDER);
        System.err.println("countzero   = " + iteration);
        System.err.println("counto      = " + COUNT_O);
        System.err.println();
    }
}

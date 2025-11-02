/*
 * random_name.c
 *
 *  A C rewrite of the (broken) shell script that printed
 *  "adjective-noun" pairs until the height of the terminal was reached.
 *
 *  Build:
 *      gcc -Wall -Wextra -O2 -o random_name random_name.c
 *
 *  Usage:
 *      ./random_name            # normal mode
 *      ./random_name -d         # debug mode (prints internal state)
 *
 *  The program expects two sub‑directories in the directory where it is
 *  started:
 *
 *      nouns/      – any number of regular files, each containing one noun
 *                    per line.
 *
 *      adjectives/ – any number of regular files, each containing one
 *                    adjective per line.
 *
 *  It selects one random file from each directory, then picks a random
 *  line from those files on every iteration.
 *
 *  If a directory is empty, or a file cannot be opened, the program prints
 *  an error message and exits with a non‑zero status.
 */

#define _POSIX_C_SOURCE 200809L   /* for getline(), fdopen(), etc. */
#define PATH_MAX 1000

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <ctype.h>
#include <dirent.h>
#include <sys/ioctl.h>
#include <unistd.h>
#include <time.h>
#include <errno.h>
#include <sys/stat.h>

/* -------------------------------------------------------------------- *
 * Helper structures & prototypes
 * -------------------------------------------------------------------- */
typedef struct {
    char **entries;   /* array of strdup‑ed paths */
    size_t  count;    /* number of entries */
} strvec_t;

static void   strvec_free(strvec_t *v);
static strvec_t list_regular_files(const char *dirpath);
static char *pick_random_entry(const strvec_t *v);
static char *pick_random_line(const char *filepath);
static void   to_lowercase(char *s);
static int    terminal_lines(void);
static void   debug_print(const char *msg, ...);

/* -------------------------------------------------------------------- *
 * Global flag set by the command‑line option "--debug"
 * -------------------------------------------------------------------- */
static bool g_debug = false;

/* -------------------------------------------------------------------- *
 * Main
 * -------------------------------------------------------------------- */
int main(int argc, char *argv[])
{
    /* ---------- 1. command line parsing (only -d/--debug) ---------- */
    for (int i = 1; i < argc; ++i) {
        if (!strcmp(argv[i], "-d") || !strcmp(argv[i], "--debug"))
            g_debug = true;
        else {
            fprintf(stderr, "Usage: %s [-d|--debug]\n", argv[0]);
            return EXIT_FAILURE;
        }
    }

    /* ---------- 2. initialise random number generator ---------- */
    srand((unsigned)time(NULL) ^ (unsigned)getpid());

    /* ---------- 3. determine current directory (HERE) ---------- */
    char cwd[PATH_MAX];
    if (!getcwd(cwd, sizeof(cwd))) {
        perror("getcwd");
        return EXIT_FAILURE;
    }
    debug_print("HERE = %s\n", cwd);

    /* ---------- 4. terminal height (counto) ---------- */

    char *env_var_str;
    int counto;
    const char *env_var_name = "counto"; // Replace with your environment variable name

    // 1. Get the environment variable string
    env_var_str = getenv(env_var_name);

    if (env_var_str != NULL) {
        // 2. Convert the string to an integer
        counto = atoi(env_var_str);
        //printf("Environment variable '%s' value (as int): %d\n", env_var_name, counto);
    } else {
        //printf("Environment variable '%s' not found or is empty.\n", env_var_name);
        debug_print("terminal lines (counto) = %d\n", counto);
        counto = terminal_lines();
    }

    /* ---------- 5. build paths for the two sub‑directories ---------- */
    char noun_dir[PATH_MAX];
    char adj_dir[PATH_MAX];
    snprintf(noun_dir, sizeof(noun_dir), "%s/nouns", cwd);
    snprintf(adj_dir,  sizeof(adj_dir), "%s/adjectives", cwd);
    debug_print("NOUN_FOLDER = %s\n", noun_dir);
    debug_print("ADJ_FOLDER  = %s\n", adj_dir);

    /* ---------- 6. read the list of files in each directory ---------- */
    strvec_t noun_files = list_regular_files(noun_dir);
    strvec_t adj_files  = list_regular_files(adj_dir);

    if (noun_files.count == 0) {
        fprintf(stderr, "Error: no regular files found in \"%s\"\n", noun_dir);
        strvec_free(&noun_files);
        strvec_free(&adj_files);
        return EXIT_FAILURE;
    }
    if (adj_files.count == 0) {
        fprintf(stderr, "Error: no regular files found in \"%s\"\n", adj_dir);
        strvec_free(&noun_files);
        strvec_free(&adj_files);
        return EXIT_FAILURE;
    }

    /* ---------- 7. pick ONE random file from each directory ---------- */
    char *noun_file_path = pick_random_entry(&noun_files);
    char *adj_file_path  = pick_random_entry(&adj_files);
    debug_print("selected NOUN_FILE = %s\n", noun_file_path);
    debug_print("selected ADJ_FILE  = %s\n", adj_file_path);

    /* ---------- 8. main loop: produce counto adjective‑noun pairs ---------- */
    for (int i = 0; i < counto; ++i) {
        char *noun   = pick_random_line(noun_file_path);
        char *adj    = pick_random_line(adj_file_path);
        if (!noun || !adj) {   /* very unlikely, but guard against it */
            free(noun);
            free(adj);
            break;
        }

        to_lowercase(noun);    /* the original script forced the noun to lower case */

        /* Build "adjective-noun" */
        size_t out_len = strlen(adj) + 1 + strlen(noun) + 1;
        char *out = malloc(out_len);
        if (!out) {
            perror("malloc");
            free(noun);
            free(adj);
            break;
        }
        snprintf(out, out_len, "%s-%s", adj, noun);

        /* Print the result (and optionally the debug information) */
        printf("%s\n", out);
        if (g_debug) {
            debug_print("[debug] iteration %d: adj=\"%s\" noun=\"%s\" -> \"%s\"\n",
                        i + 1, adj, noun, out);
        }

        free(noun);
        free(adj);
        free(out);
    }

    /* ---------- 9. clean up ---------- */
    free(noun_file_path);
    free(adj_file_path);
    strvec_free(&noun_files);
    strvec_free(&adj_files);

    return EXIT_SUCCESS;
}

/* -------------------------------------------------------------------- *
 * Helper functions
 * -------------------------------------------------------------------- */

/* Free a strvec_t */
static void strvec_free(strvec_t *v)
{
    if (!v) return;
    for (size_t i = 0; i < v->count; ++i)
        free(v->entries[i]);
    free(v->entries);
    v->entries = NULL;
    v->count   = 0;
}

/* Return a vector containing the full path of every regular file
 * (not directories, symlinks, etc.) that lives directly inside 'dirpath'.
 *
 * The function allocates memory for the vector and for each entry.
 * Caller must free it with strvec_free().
 */
static strvec_t list_regular_files(const char *dirpath)
{
    strvec_t vec = {NULL, 0};

    DIR *dir = opendir(dirpath);
    if (!dir) {
        perror(dirpath);
        return vec;
    }

    struct dirent *de;
    while ((de = readdir(dir)) != NULL) {
        /* skip "." and ".." */
        if (de->d_name[0] == '.' && (de->d_name[1] == '\0' ||
            (de->d_name[1] == '.' && de->d_name[2] == '\0')))
            continue;

        char full[PATH_MAX];
        snprintf(full, sizeof(full), "%s/%s", dirpath, de->d_name);

        struct stat st;
        if (stat(full, &st) == -1) {
            perror(full);
            continue;
        }
        if (S_ISREG(st.st_mode)) {
            char *copy = strdup(full);
            if (!copy) {
                perror("strdup");
                continue;
            }
            /* grow the array */
            char **tmp = realloc(vec.entries, (vec.count + 1) * sizeof(char *));
            if (!tmp) {
                perror("realloc");
                free(copy);
                continue;
            }
            vec.entries = tmp;
            vec.entries[vec.count++] = copy;
        }
    }
    closedir(dir);
    return vec;
}

/* Pick one entry from a non‑empty strvec_t, duplicate it, and return it.
 * Caller must free() the returned string.
 */
static char *pick_random_entry(const strvec_t *v)
{
    if (!v || v->count == 0) return NULL;
    size_t idx = (size_t)rand() % v->count;
    return strdup(v->entries[idx]);
}

/* Pick a random line from a text file.
 * The whole line (without trailing '\n') is returned as a freshly allocated
 * string.  Caller must free() it.
 *
 * Implementation: first count the number of lines (fast, O(N)), then
 * rewind and stop at the randomly chosen line.
 */
static char *pick_random_line(const char *filepath)
{
    FILE *fp = fopen(filepath, "r");
    if (!fp) {
        perror(filepath);
        return NULL;
    }

    /* 1. count lines */
    size_t lines = 0;
    int ch;
    while ((ch = fgetc(fp)) != EOF) {
        if (ch == '\n')
            ++lines;
    }
    if (lines == 0) {   /* file is empty */
        fclose(fp);
        return NULL;
    }

    /* 2. choose a random line number (0‑based) */
    size_t target = (size_t)rand() % lines;

    /* 3. rewind and read until we hit the target line */
    rewind(fp);
    size_t cur = 0;
    char *line = NULL;
    size_t len = 0;
    while (cur <= target && getline(&line, &len, fp) != -1) {
        if (cur == target) {
            /* strip trailing newline */
            size_t l = strlen(line);
            if (l && line[l-1] == '\n')
                line[l-1] = '\0';
            fclose(fp);
            return line;      /* caller owns the buffer */
        }
        ++cur;
    }

    /* Should never get here, but be defensive */
    free(line);
    fclose(fp);
    return NULL;
}

/* Convert a string in‑place to lower case (ASCII only – sufficient for the
 * original script which only dealt with English words). */
static void to_lowercase(char *s)
{
    for (; *s; ++s)
        *s = (char)tolower((unsigned char)*s);
}

/* Return the height of the attached terminal in lines.
 * If we are not running on a tty, fall back to 24 (the traditional default). */
static int terminal_lines(void)
{
    struct winsize ws;
    if (isatty(STDOUT_FILENO) && ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) == 0)
        //printf("This terminal window is %d rows by %d columns\n", ws.ws_row, ws.ws_col);
        return (ws.ws_row > 0) ? ws.ws_row : 24;
    return 24;   /* fallback */
}

/* Simple wrapper around printf that only prints when debug mode is on */
#include <stdarg.h>
static void debug_print(const char *msg, ...)
{
    if (!g_debug)
        return;
    va_list ap;
    va_start(ap, msg);
    vfprintf(stderr, msg, ap);
    va_end(ap);
}




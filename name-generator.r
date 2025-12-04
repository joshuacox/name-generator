#!/usr/bin/env Rscript
# Name generator in R - implements similar functionality to name-generator.sh

# ------------------------------------------------------------------------------- #
# Helper functions
# ------------------------------------------------------------------------------- #

resolve_file <- function(env_var, folder) {
  # If environment variable is set and points to a regular file, use it
  env_val <- Sys.getenv(env_var)
  if (!is.na(env_val) && env_val != "") {
    file_path <- normalizePath(env_val)
    if (file.exists(file_path)) {
      return(file_path)
    }
  }
  
  # Otherwise pick a random regular file from the folder
  files <- dir(folder, full.names = TRUE, recursive = FALSE)
  files <- files[grepl("\\.", files)]  # Basic filter for files (not just directories)
  
  if (length(files) == 0) {
    stop(sprintf("Folder %s contains no regular files", folder))
  }
  
  return(sample(files, 1))
}

read_nonempty_lines <- function(file_path) {
  content <- readLines(file_path, ok = TRUE)
  lines <- gsub("\r\n?", "\n", content) # Normalize line endings
  lines <- gsub("^[ \t]+", "", lines)    # Trim leading whitespace
  lines <- gsub("[ \t]+$", "", lines)    # Trim trailing whitespace
  lines <- lines[lines != ""]            # Remove empty lines
  return(lines)
}

# ------------------------------------------------------------------------------- #
# Configuration - environment overrides with sensible defaults
# ------------------------------------------------------------------------------- #

separator <- Sys.getenv("SEPARATOR", "-")  # Default separator

noun_folder <- Sys.getenv("NOUN_FOLDER", "./nouns")
adj_folder <- Sys.getenv("ADJ_FOLDER", "./adjectives")

counto_default <- as.integer(Sys.getenv("counto", "24"))
if (is.na(counto_default) || counto_default < 1) {
  counto_default <- 24
}

noun_file <- resolve_file("NOUN_FILE", noun_folder)
adj_file <- resolve_file("ADJ_FILE", adj_folder)

# ------------------------------------------------------------------------------- #
# Debug helper - prints to stderr when DEBUG=true
# ------------------------------------------------------------------------------- #

maybe_debug <- function(adjective, noun) {
  if (Sys.getenv("DEBUG") == "true") {
    writeLines(paste0(
      "Adjective: ", adjective, "\n",
      "Noun: ", noun, "\n",
      "Adj File: ", adj_file, "\n", 
      "Noun File: ", noun_file
    ), stderr())
  }
}

# ------------------------------------------------------------------------------- #
# Main generation function
# ------------------------------------------------------------------------------- #

generate_names <- function(count = 24) {
  # Read adjective and noun lines
  adj_lines <- read_nonempty_lines(adj_file)
  noun_lines <- read_nonempty_lines(noun_file)
  
  if (length(adj_lines) == 0 | length(noun_lines) == 0) {
    stop("No valid entries found in files")
  }

  names <- character(count)
  set.seed(Sys.time())  # Ensure random seed
  
  for (i in 1:count) {
    adjective <- sample(adj_lines, 1)
    noun <- tolower(sample(noun_lines, 1))
    
    maybe_debug(adjective, noun)
    
    names[i] <- paste0(adjective, separator, noun)
  }
  
  return(names)
}

# ------------------------------------------------------------------------------- #
# Command line handling
# ------------------------------------------------------------------------------- #


# Simple command‑line argument parsing (no external packages)
# Supported flags:
#   --count N      Number of names to generate (default: 24)
#   --separator S  Separator between adjective and noun (default: env SEPARATOR or "-")
parse_args <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  # defaults
  count  <- counto_default
  sep    <- separator   # value already read from env (or "-")
  
  i <- 1
  while (i <= length(args)) {
    arg <- args[i]
    if (arg == "--count" && (i + 1) <= length(args)) {
      count <- as.integer(args[i + 1])
      i <- i + 1
    } else if (arg == "--separator" && (i + 1) <= length(args)) {
      sep <- args[i + 1]
      i <- i + 1
    } else {
      stop(sprintf("Unrecognised argument: %s", arg))
    }
    i <- i + 1
  }
  list(count = count, separator = sep)
}
# Main execution
args <- parse_args()
# Override the separator variable with the command‑line value (if any)
separator <- args$separator

names <- generate_names(as.integer(args$count))
cat(names, sep = "\n")

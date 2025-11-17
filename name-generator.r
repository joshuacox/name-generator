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

if (!require(docopt)) {
  install.packages("docopt")
  library(docopt)
}

usage <- "Usage: name-generator.r [options]

Options:
--count N      Number of names to generate [default: 24]
--separator S  Separator between adjective and noun [default: -]"

opts <- docopt(usage)

# Main execution
names <- generate_names(as.integer(opts$count))
cat(names, sep = "\n")

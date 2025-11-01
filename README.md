# Name Generator

This repository contains scripts and data for generating names. It supports generating names from English adjectives and nouns, as well as names inspired by Greek gods.

## Files


*   `name-generator.py`:  A Python script that generates names based on the provided data files.

*   `name-generator.bash`: A Bash script that generates names.

*   `name-generator.sh`: Another Bash script that generates names.

*   `name-generator.zsh`: A Zsh script that generates names.

*   `adjectives/english-adjectives.txt`: A text file containing a list of English adjectives.

*   `nouns/english-nouns.txt`: A text file containing a list of English nouns.

*   `nouns/greek_gods.txt`: A text file containing a list of Greek god names.

*   `.python-version`: Specifies the Python version used by the Python script.

*   `LICENSE`: The license for this repository.

## Usage

### Python

1.  Make sure you have Python installed.  Check your version with `python --version`. The required version is specified in `.python-version`.

2.  Run the script with `python name-generator.py <name>`.  The `<name>` argument is optional and will be used as a seed for the name generation. If no name is provided, a
random seed will be used.

### Bash/Zsh

1.  Run the script with `./name-generator.bash` or `./name-generator.zsh`.  These scripts will generate a name and print it to the console.

## Data Files

The `adjectives` and `nouns` directories contain text files with lists of words.  You can modify these files to customize the names that are generated. Each line in the text
file represents a single word.

## License

This project is licensed under the terms of the LICENSE file.

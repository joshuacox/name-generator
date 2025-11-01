#!/usr/bin/env python
# This was converted from the shell by llama3.2
import os
import random

def main():
    # Get the current working directory
    here = os.getcwd()

    # Define folder paths
    noun_folder = os.path.join(here, 'nouns')
    adjective_folder = os.path.join(here, 'adjectives')

    # Find and select a random noun file
    nouns = [f for f in os.listdir(noun_folder) if f.endswith('.list')]
    if not nouns:
        print("No noun files found.")
        return

    noun_file = random.choice(nouns)
    noun_path = os.path.join(noun_folder, noun_file)

    # Find and select a random adjective file
    adjectives = [f for f in os.listdir(adjective_folder) if f.endswith('.list')]
    if not adjectives:
        print("No adjective files found.")
        return

    adjective_file = random.choice(adjectives)
    adjective_path = os.path.join(adjective_folder, adjective_file)

    # Load and shuffle the noun and adjective files
    with open(noun_path, 'r') as f:
        nouns_list = [line.strip().lower() for line in f.readlines()]
    with open(adjective_path, 'r') as f:
        adjectives_list = [line.strip().lower() for line in f.readlines()]

    noun = random.choice(nouns_list)
    adjective = random.choice(adjectives_list)

    # Print the result
    print(f"{adjective}-{noun}")

if __name__ == "__main__":
    main()

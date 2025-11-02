#!/usr/bin/env python
# This was originally converted from the shell by llama4:scout
import os
import random
import shutil

# Define constants
HERE = os.getcwd()
NOUN_FOLDER = os.path.join(HERE, 'nouns')
ADJ_FOLDER = os.path.join(HERE, 'adjectives')

def get_random_file(folder):
    files = [os.path.join(folder, f) for f in os.listdir(folder) if os.path.isfile(os.path.join(folder, f))]
    return random.choice(files)

def debugger(debug_mode, noun, adjective):
    if debug_mode:
        print(f"NOUN: {noun}")
        print(f"ADJECTIVE: {adjective}")
        print(f"NOUN FILE: {get_random_file(NOUN_FOLDER)}")
        print(f"ADJ FILE: {get_random_file(ADJ_FOLDER)}")

def main():
    count = 0
    try:
      max_count = int(os.getenv("counto"))
    except:
      terminal_size = os.get_terminal_size()
      max_count = terminal_size.columns
    if isinstance(max_count, int):
        pass
        #print('good')
    else:
        print("max_count = ", max_count)
        print('WARN: max_count no good')
        max_count = 1
        #exit(1)

    noun_file = get_random_file(NOUN_FOLDER)
    adj_file = get_random_file(ADJ_FOLDER)

    with open(noun_file, 'r') as f:
        nouns = f.read().splitlines()

    with open(adj_file, 'r') as f:
        adjectives = f.read().splitlines()

    for _ in range(max_count):
        noun = random.choice(nouns).lower()
        adjective = random.choice(adjectives).lower()
        name = f"{adjective}-{noun}"
        print(name)
        count += 1

if __name__ == "__main__":
    main()

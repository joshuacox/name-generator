# Name Generator

This repository contains a simple name generator script that combines random nouns and adjectives to create unique names.

## Getting Started

To use this repository, follow these steps:

1. Clone the repository to your local machine using Git: `git clone https://github.com/your-username/name-generator.git`
2. Navigate to the cloned repository: `cd name-generator`
3. Run the script using Python: `python name-generator.py`

## Usage

The script will output a randomly generated name in the format "adjective-noun". You can run the script multiple times to generate different names.

```
./name-generator.sh
```

Or specify 

```
NOUN_FILE=alternative-nouns/greek_gods.list ADJ_FILE=alternative-adjectives/taste.list ./name-generator.sh
NOUN_FILE=alternative-nouns/us_presidents.list ADJ_FILE=alternative-adjectives/positive.list ./name-generator.sh
```

## Customization

To add your own nouns and adjectives, create text files in the `alternative-nouns` and `alternative-adjectives` directories. Each file should contain one word per line.

## Requirements

* Python 3.x (if you use the name-generatory.py script)
* A Unix-like operating system (tested on Linux)

## Troubleshooting

If you encounter any issues, check the following:

* Make sure you have Python installed on your system.
* Ensure that the `nouns` and `adjectives` directories are in the same directory as the script.
* Check that the text files in the `nouns` and `adjectives` directories are formatted correctly (one word per line).

## Contributing

If you'd like to contribute to this project, please fork the repository and submit a pull request with your changes.

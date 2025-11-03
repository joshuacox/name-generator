# Name Generator

Do you need to name a server, or something else?  I know 'treat em like cattle', but I detest names like dal2dc3c38r67, I say give it some personality, inspired by the docker mechanism that does the same, I needed a quick and dirty script to name a bunch of servers and wrote a quick one in bash, then decided to benchmark sh vs bash vs zsh, then threw python in the mix.   Well now I have sh, bash, zsh, c, python, javascript, golang, and rust, PRs welcome for more!

This repository contains a simple name generator script that combines random nouns and adjectives to create unique names. This was inpsired by the [docker mechanism](https://github.com/moby/moby/blob/39f7b2b6d0156811d9683c6cb0743118ae516a11/pkg/namesgenerator/names-generator.go#L134-L135) that generates names. 
		"agnesi",

e.g.

```
counto=5 NOUN_FILE=alternative-nouns/greek_gods.list ADJ_FILE=alternative-adjectives/taste.list ./name-generator.sh
tangy-apheliotes
rancid-styx
delicious-dysis
sweet-proioxis
rotten-hedone
```

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

# benchmarks

`./benchark.sh`

The rust implementation was able to achieve 2 million results with at max 3 duplicates max in under 35 seconds

## Language Deathmatch!!!

what language is fastest? I have sh, bash, zsh, c, python, javascript, and rust now in this project:
https://github.com/joshuacox/name-generator
TLDR at low counts C dominates, but rust and python quickly take over at higher counts and rust is king at very large iterations

also created 9,530,416 unique names out of 10 million in under two minutes:

```
export counto=10000000 && time /home/thoth/.cargo/target/debug/name-generator |sort|uniq -c|wc -l  
9530416
/home/thoth/.cargo/target/debug/name-generator  30.70s user 11.94s system 56% cpu 1:15.45 total
sort  72.52s user 9.85s system 73% cpu 1:51.85 total
uniq -c  2.84s user 0.32s system 2% cpu 1:51.85 total
```

## Customization

Originally I was going to just throw files in the noun and adjective directories, however, this lead to heavier 'weights' for the lines in the smaller files, so I am concatenating into a single files at this point.

However, if you were to place files in the noun or adjectives directories they would be randomly picked from for the adjective and noun files.

To add your own nouns and adjectives, create text files in the `alternative-nouns` and `alternative-adjectives` directories. Each file should contain one word per line.

And then utilize them by specifying the env vars NOUN_FILE and ADJ_FILE i.e.:

```
NOUN_FILE=alternative-nouns/greek_gods.list ADJ_FILE=alternative-adjectives/taste.list ./name-generator.sh
NOUN_FILE=alternative-nouns/us_presidents.list ADJ_FILE=alternative-adjectives/positive.list ./name-generator.sh
```

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

## Funniest generations

worse-bunghole
inappropriate-middleman
sordid-pronoun
heavenly-lordstanly
joyous-snookie
overrated-truffle
staunch-djolu
discrete-concrete
facetious-tool
brainless-rebel
artless-lunch
unattractive-prevention
purported-teila
meaningless-nash
hazardous-master
ashamed-bimka
gasping-rennie
resentful-antibody
holistic-zisoux


# Testing

`./test.bats`

test.bats
 ✓ test name-generator at 10
 ✓ test name-generator at 100
 ✓ test name-generator_go at 10
 ✓ test name-generator_go at 100
 ✓ test name-generator.sh at 10
 ✓ test name-generator.sh at 100
 ✓ test name-generator.bash at 10
 ✓ test name-generator.bash at 100
 ✓ test name-generator.zsh at 10
 ✓ test name-generator.zsh at 100
 ✓ test name-generator.py at 10
 ✓ test name-generator.py at 100
 ✓ test name-generator.js at 10
 ✓ test name-generator.js at 100
 ✓ test name-generator-sync.js at 10
 ✓ test name-generator-sync.js at 100
 ✓ test /home/thoth/.cargo/target/debug/name-generator at 10
 ✓ test /home/thoth/.cargo/target/debug/name-generator at 100

18 tests, 0 failures


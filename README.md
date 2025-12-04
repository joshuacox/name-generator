# [Name Generator](https://joshuacox.github.io/name-generator/)

## [namgen](https://github.com/joshuacox/namgen)
This repo has a sister-repo [namgen](https://github.com/joshuacox/namgen) that is a further implementation of this concept in cpp after proving itself to be the performance king.  This repo will continue to exist to pit languages against each other in a deathmatch.  The other repo will continue on as I add features like command line arguments and cap/camel casing.

This repository contains a
[website](https://joshuacox.github.io/name-generator/)
and simple name generator script that combines random nouns and adjectives to create unique names. 
This was inpsired by the [docker mechanism](https://github.com/moby/moby/blob/39f7b2b6d0156811d9683c6cb0743118ae516a11/pkg/namesgenerator/names-generator.go#L134-L135) 
that generates names. 
This project differs in that I am attempting to have a much larger adjective and noun lists generating more variation.

## Why

Do you need to name a server, or something else?  I know 'treat em like cattle', but I detest names like dal2dc3c38r67, I say give it some personality, inspired by the docker mechanism that does the same, I needed a quick and dirty script to name a bunch of servers and wrote a quick one in bash, then decided to benchmark sh vs bash vs zsh, then threw python in the mix.   Well now I have 

1. bash
1. c
1. cpp
1. D
1. dart
1. elisp
1. elixir
1. erlang
1. fish
1. golang
1. haskell
1. java
1. javascript async + sync
1. julia
1. kotlin
1. lua
1. ocaml
1. octave
1. pascal
1. perl
1. php
1. python
1. R
1. racket
1. raku
1. ruby
1. rust
1. scala
1. sh
1. typescript
1. zsh

PRs welcome for more!

### Similar projects in that they aim to show one thing done in many ways.

1. [Arch is the best](https://wiki.archlinux.org/title/Arch_is_the_best)
1. [todoMVC](https://todomvc.com)
1. [turtlekitty's dice](https://github.com/TurtleKitty/Dice)
1. [pacman rosetta stone](https://wiki.archlinux.org/title/Pacman/Rosetta)


## Getting Started

#### web based

1. Simply visit the [site](https://joshuacox.github.io/name-generator/) in github.io.
1. Click generate.

#### CLI based

follow these steps:

1. Clone the repository to your local machine using Git: `git clone https://github.com/joshuacox/name-generator.git`
1. Navigate to the cloned repository: `cd name-generator`
1. Run the script using shell: `./name-generator.sh`
1. Run the script using Python: `./name-generator.py`
1. Refer to the `test/test.bats` or `benchmark.sh` script for examples of running all languages that are tested

## Usage

The script will output a randomly generated name in the format "adjective-noun". You can run the script multiple times to generate different names.

```
./name-generator.sh
```

Or specify NOUN_FILE and ADJ_FILE:

```
NOUN_FILE=alternative-nouns/greek_gods.list ADJ_FILE=alternative-adjectives/taste.list ./name-generator.sh
NOUN_FILE=alternative-nouns/us_presidents.list ADJ_FILE=alternative-adjectives/positive.list ./name-generator.sh
```

Specify a count:

e.g.

```
counto=5 NOUN_FILE=alternative-nouns/greek_gods.list ADJ_FILE=alternative-adjectives/taste.list ./name-generator.sh
tangy-apheliotes
rancid-styx
delicious-dysis
sweet-proioxis
rotten-hedone
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

```
make test                                           
./test/bats/bin/bats test/test.bats
test.bats
 ✓ test name-generator at 10
 ✓ test name-generator at 44
 ✓ test NameGenerator at 10
 ✓ test NameGenerator at 55
 ✓ test name-generator_go at 10
 ✓ test name-generator_go at 12399
 ✓ test name-generator.sh at 10
 ✓ test name-generator.sh at 22
 ✓ test name-generator.bash at 10
 ✓ test name-generator.bash at 33
 ✓ test name-generator.zsh at 10
 ✓ test name-generator.zsh at 66
 ✓ test name-generator.py at 10
 ✓ test name-generator.py at 77
 ✓ test name-generator.pl at 10
 ✓ test name-generator.pl at 75
 ✓ test name-generator.js at 10
 ✓ test name-generator.js at 88
 ✓ test name-generator-sync.js at 10
 ✓ test name-generator-sync.js at 99
 ✓ test /home/thoth/.cargo/target/debug/name-generator at 10
 ✓ test /home/thoth/.cargo/target/debug/name-generator at 33433
 ✓ test name-generator.rb at 10
 ✓ test name-generator.rb at 83
 ✓ test name-generator_haskell.hs at 10
 ✓ test name-generator_haskell.hs at 83
 ✓ test sh test/test
 ✓ test bash test/test
 ✓ test perl test/test
 ✓ test ruby test/test
 ✓ test zsh test/test
 ✓ test go test/test
 ✓ test java test/test
 ✓ test jssync test/test
 ✓ test js test/test
 ✓ test py test/test
 ✓ test rust test/test
 ✓ test haskell test/test
 ✓ test c test/test

39 tests, 0 failures
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

[![asciicast](https://asciinema.org/a/dRhBeZltkuInh3eF0pOREJACA.svg)](https://asciinema.org/a/dRhBeZltkuInh3eF0pOREJACA)

It is intersting to note that at `export counto=2` the c implementation is fastest, then go, rust, the shell implementations, javascript, then java, and lastly python

```
  ./name-generator ran
    1.23 ± 0.38 times faster than ./name-generator_go
    1.97 ± 0.73 times faster than /home/thoth/.cargo/target/debug/name-generator
    3.84 ± 1.03 times faster than ./name-generator.zsh
    3.84 ± 1.02 times faster than ./name-generator.sh
    3.94 ± 1.04 times faster than ./name-generator.bash
    9.48 ± 3.13 times faster than ./name-generator.js
   10.42 ± 3.49 times faster than ./name-generator-sync.js
   14.58 ± 3.99 times faster than java NameGenerator
   25.12 ± 6.79 times faster than ./name-generator.py
```

But at a counto of 12 go and rust surpass the C, the shells hanging in the middle of the pack, Java passes javascript

```
  ./name-generator_go ran
    1.61 ± 0.48 times faster than /home/thoth/.cargo/target/debug/name-generator
    3.28 ± 0.90 times faster than ./name-generator
    9.48 ± 1.85 times faster than ./name-generator.zsh
   10.82 ± 2.54 times faster than ./name-generator.bash
   10.86 ± 2.40 times faster than ./name-generator.sh
   11.63 ± 2.44 times faster than java NameGenerator
   13.68 ± 3.15 times faster than ./name-generator.js
   16.84 ± 3.30 times faster than ./name-generator-sync.js
   19.22 ± 3.80 times faster than ./name-generator.py
```

At a counto of 25 java and asynchronous javascript charge ahead of the shells, and python begins to gather steam, with synchronous javascript dead last

```
  ./name-generator_go ran
    1.74 ± 0.42 times faster than /home/thoth/.cargo/target/debug/name-generator
    5.85 ± 0.72 times faster than ./name-generator
   12.30 ± 2.17 times faster than java NameGenerator
   19.45 ± 2.48 times faster than ./name-generator.js
   20.63 ± 2.66 times faster than ./name-generator.zsh
   21.01 ± 3.30 times faster than ./name-generator.py
   21.74 ± 2.41 times faster than ./name-generator.bash
   21.76 ± 2.36 times faster than ./name-generator.sh
   29.58 ± 3.43 times faster than ./name-generator-sync.js
```

At a counto of 50 python overtakes javascript and zsh

```
  ./name-generator_go ran
    1.67 ± 0.48 times faster than /home/thoth/.cargo/target/debug/name-generator
   11.43 ± 2.45 times faster than ./name-generator
   11.96 ± 2.92 times faster than java NameGenerator
   21.74 ± 4.84 times faster than ./name-generator.py
   28.09 ± 6.11 times faster than ./name-generator.js
   38.01 ± 7.95 times faster than ./name-generator.zsh
   40.90 ± 8.50 times faster than ./name-generator.bash
   41.64 ± 8.71 times faster than ./name-generator.sh
   47.86 ± 10.91 times faster than ./name-generator-sync.js
```

At a counto of 100 java and python overtake the C

```
  ./name-generator_go ran
    1.73 ± 0.54 times faster than /home/thoth/.cargo/target/debug/name-generator
   13.64 ± 1.39 times faster than java NameGenerator
   21.70 ± 1.70 times faster than ./name-generator.py
   25.97 ± 1.98 times faster than ./name-generator
   53.89 ± 3.55 times faster than ./name-generator.js
   79.16 ± 4.51 times faster than ./name-generator.zsh
   81.36 ± 4.77 times faster than ./name-generator.sh
   83.00 ± 5.39 times faster than ./name-generator.bash
   83.13 ± 5.15 times faster than ./name-generator-sync.js
```

At a counto of 250 the synchronous javascript pulls ahead of the shells

```
  ./name-generator_go ran
    1.73 ± 0.65 times faster than /home/thoth/.cargo/target/debug/name-generator
   15.11 ± 4.05 times faster than java NameGenerator
   23.15 ± 6.14 times faster than ./name-generator.py
   66.67 ± 18.06 times faster than ./name-generator
  124.18 ± 33.03 times faster than ./name-generator.js
  206.49 ± 54.98 times faster than ./name-generator-sync.js
  208.77 ± 55.23 times faster than ./name-generator.zsh
  223.60 ± 59.11 times faster than ./name-generator.bash
  226.21 ± 59.86 times faster than ./name-generator.sh
```



# Sol

A mini Lua interpreter written in Haskell

### Table of contents
- [About the Project](#about-the-project)
- [Getting Started](#getting-started)
    - [Requirements](#requirements)
    - [Building and running the Project](#building-and-running-the-project)
- [Running Tests](#running-tests)
- [Code Documentation](#code-documentation)
- [Implemented Features](#implemented-features)
- [Language Grammar](#language-grammar)

## About the Project

Sol is a small [Lua](https://www.lua.org/) interpreter for Unix Systems written entirely in Haskell. It aims to conform as much as possible to the Lua 5.4 standard.

## Getting Started

### Requirements

- [The GHCup toolchain](https://www.haskell.org/ghcup/)
- [ICU](https://unicode-org.github.io/icu/) (Example install: `sudo apt-get install libicu-dev`)

### Building and running the Project

To build it, simply do `cabal build all` and check `dist-newstyle/build/` for the binary.

To run it with cabal, the command is `cabal run sol -- <lua file>`

## Running Tests

You can the unit test suites with `cabal test`

## Code Documentation

All modules under `lib` are documented with `haddock` comments. You can read these in the source code or run `cabal haddock --haddock-all` to generate documentation pages. Check the output of that command to access the web pages generated.

## Implemented features

- Parses all Lua 5.4 Syntax
- Implemented data types:
    - number (all numbers are doubles, subject to change in the future)
    - string
    - boolean
    - table
    - nil
    - function (no closures yet)
- All statement types are supported, except `for .. in` loops, since they require iterators, which require closures.
- There aren't many standard library functions (yet). Here are all that are implemented:
    - print
    - os.clock
    - os.execute
    - os.exit
    - os.getenv
    - os.time (without isdst)

> [!NOTE]
> Calling a function of a standard library table like `os` that does not exist will result in an error and exit the program. Others will simply be `nil`, since there are no table entries for them in the environment.

## Language Grammar

The syntax grammar is mostly the same as the [original](https://www.lua.org/manual/5.4/manual.html#9). Due to the the use of [parsec](https://hackage.haskell.org/package/parsec) as the language parser, which is right recursive, there were a few changes done.

You can check out the old and new syntax in `grammar.md`

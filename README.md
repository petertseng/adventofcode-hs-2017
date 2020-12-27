# Advent of Code

These are my solutions to http://adventofcode.com

All solutions are written in Haskell.

[![Build Status](https://travis-ci.org/petertseng/adventofcode-hs-2017.svg?branch=master)](https://travis-ci.org/petertseng/adventofcode-hs-2017)

## Input

In general, all solutions can be invoked in both of the following ways:

* Without command-line arguments, takes input on standard input.
* With 1+ command-line arguments, reads input from the first, which must be the path to an input file.
  Arguments beyond the first are ignored.

Some may additionally support other ways:

None yet.

## Highlights

None yet.

### Packages

As of 2019, building Haskell packages on my development machine is (mostly) broken, so I was forced to implement the following days without packages, when I otherwise would have preferred to use packages:

None yet.

### Closing Thoughts

Even when knot-tying is not directly possible, passing back a function that allows the function to pick up where it left off is a very interesting pattern.

Immutable array updates copy the entire array, so don't do that.
Mutable array updates or IntMap, depending on what's appropriate.

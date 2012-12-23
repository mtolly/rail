rail
====

A Rail interpreter and compiler in Haskell, by Michael Tolly.

For information on the Rail language, see [its page on Esolang](http://esolangs.org/wiki/Rail).

This includes a library and an executable, performing two functions: a Rail
program can be directly run by a Haskell interpreter, or it can be translated
to C code, which can then be compiled by any standard C99 compiler.

Usage
=====

After cloning the repository, install the [Haskell Platform](http://www.haskell.org/platform/),
and install the package using cabal:

    cd rail
    cabal install

This installs the `rail` package, and the `hrail` executable. To run the executable:

    hrail input.rail
    hrail input.rail output.c

The first form directly runs the Rail program, and the second generates a C program.

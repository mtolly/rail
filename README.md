rail
====

A Rail interpreter and compiler in Haskell, by [Michael Tolly](mailto:miketolly@gmail.com).

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

This installs the `rail` library, and the `hrail` executable. To run the executable:

    hrail input.rail
    hrail input.rail output.c

The first form directly runs the Rail program, and the second generates a C program.

Code generation
===============

Both the interpreter and the C generator start by statically traversing each function to build a
simplified control flow graph. This enables simple C code using goto statements to travel between
(more or less) basic blocks. As an example, the following Rail function (a cat program):

    $ 'main' (--):
     \
     | /---------\
     | |         |
     | \    /-io-/
     \---e-<
            \-#

becomes this C function:

    void fun_main() {
      goto E_5_4;
    E_5_4:
      builtin_eof();
      builtin_setbranch();
      if (condition) {
        goto done;
      } else {
        builtin_input();
        builtin_output();
        goto E_5_4;
      }
    done:
      return;
    }

where the only remnant of the grid is a single label (the point where two branching paths meet).

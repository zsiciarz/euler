euler
=====

My solutions for Project Euler in Haskell.

Installation
------------

    git clone https://github.com/zsiciarz/euler.git
    cd euler
    cabal sandbox init
    cabal install --only-dependencies
    cabal configure

To run a specific solution, pass the solution number to the program as a `--problem` commandline argument. For example, using `cabal run`:

    cabal run -- --problem=5

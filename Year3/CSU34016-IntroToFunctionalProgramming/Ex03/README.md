# Haskell Proof Checker

Checks proofs about Haskell code

## Installation

At the top-level do
`stack install`

This should result in an executable called `prfchk`

## Usage

The proof-checker expects all input files to be in the `theories` subdirectory. Input files can be Haskell source (`.hs`) or Theory files (`.thr`).

It also runs in two modes:

### Interactive Mode

Giving the command `prfchk` starts up the command-line interface. You can then issue commands to load theories, list laws and check theorems.

### Batch Mode

Giving command `prfchk ThryName` loads up theory `theories/ThryName.thr`, automatically checks every theorem present in that theory, and then exits.

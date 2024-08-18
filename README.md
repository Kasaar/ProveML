
ProveML
==========

[![MIT License](https://img.shields.io/badge/license-MIT-blue.svg?style=flat)](https://choosealicense.com/licenses/mit/)

ProveML is a custom tool for generating OCaml proofs by calculation. The aim is
to automate the calculation step when given an expression of the form L = R. Complex
proofs require some high-level guidance, mainly whether to use induction and lemmas.
Includes a custom lexer built on top of ocamllex, and a custom parser build on top of Menhir.

## Installation and Dependencies ##

ProveML uses the Dune build system. You will need to install it in order to compile the project.
It also depends on ocamllex and Menhir for the lexer and parser, respectively. The former should
be automatically installed with opam, but Menhir will need to be installed manually. Note that
the project is configured to use specific versions of Dune and Menhir: if you install different
versions than specified below, you will need to edit the `dune-project` file accordingly.

### MacOS ###

    brew install opam
    opam install dune.3.10.0
    opam install menhir.20230608

### Linux ###

    bash -c "sh <(curl -fsSL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)"
    opam install dune.3.10.0
    opam install menhir.20230608

### Windows ###

Using PowerShell:
    Invoke-Expression "& { $(Invoke-RestMethod https://raw.githubusercontent.com/ocaml/opam/master/shell/install.ps1) }"
    opam install dune.3.10.0
    opam install menhir.20230608

### Installation ###

The recommended way to obtain the source code is to clone the entire
[repository](https://github.com/Kasaar/KPress) from
[GitHub](https://github.com)

    git clone https://github.com/Kasaar/ProveML.git

Building the application requires running the following commands:

    cd ProveML
    eval $(opam config env)
    dune build

### Usage ###

After compiling, run the following command from the nested `ProveML` directory:
    
    ./_build/default/bin/main.exe --simple <target>

`<target>`: The full path from the nested `ProveML` directory to the
text/markdown file that contains the theorem(s) to be proved.

E.g. a working command with the example input:

    ./_build/default/bin/main.exe --simple proveme.txt
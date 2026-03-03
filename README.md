# AttoScheme

<p align="center">
  <img src="Lisp_logo.png" alt="Logo AttoScheme" />
</p>

<p align="center">
  "<i>An atto-subset for the Scheme programming language</i>"
</p>

### What is AttoScheme?
AttoScheme is a small interpreter for the Scheme programming language.

Why Scheme? I like its elegance (Lisp elegance in general).

AttoScheme is designed as a learning tool to help understand how interpreters work. It implements a minimal subset of Scheme and is primarily educational.

### Features
- Evaluate basic arithmetic and logical expressions
- Supports lists, symbols, and standard Scheme data types
- Simple REPL (Read-Eval-Print Loop)
  Note: lambda, closures, define, and other advanced Scheme features are not yet supported.

### Project Overview
AttoScheme was developed as a personal project to explore the implementation of interpreters in OCaml. It is not intended as a production-ready interpreter.

### How to Use
Make sure to have [Opam](https://opam.ocaml.org/doc/Install.html) and [Dune](https://dune.build/install) installed.

Clone the repository.

From the project root directory, run:

```bash
dune build
dune exec _build/default/bin/main.exe
```

Test:
```
dune runtest
```

Make sure to have [Opam](https://opam.ocaml.org/doc/Install.html) and [Dune](https://dune.build/install) installed.

### Examples
```
(+ 2 3) ; 5

(if #t 2 7) ; 2

(< 2 3); #t

(cdr '(a b c)); (b c)

(cons 3 (cons 2(cons 1 '()))); (3 2 1)

'(3 6 9); (3 6 9)

(bit-xor 6 3); 5
```

From the project root directory, run:

```bash
dune build
dune exec _build/default/bin/main.exe
```

### Project structure
```.
├── bin
│   ├── dune         # Dune file for the main executable
│   └── main.ml      # Entry point (REPL)
├── dune-project
├── lib
│   ├── dune         # Library dune file
│   ├── eval.ml      # Evaluation logic
│   └── parser.ml    # Parsing logic
├── lisp_interpreter.opam
└── test
    ├── dune         # Dune test configuration
    └── testing.ml   # Test cases
```

### Contributing
AttoScheme is still a work in progress. Any contribution to it will be very much appreciated!

What AttoScheme lacks right now:
- Closures
- Lambda expressions
- Define 
- Pairs

### License
MIT


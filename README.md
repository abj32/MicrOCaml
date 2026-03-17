# MicroCaml Interpreter

An interpreter and interactive REPL for **MicroCaml**, a small functional programming language based on a subset of OCaml.

This project implements the core components of a programming language runtime:

- **Lexical analysis**
- **Parsing into an Abstract Syntax Tree (AST)**
- **Environment-based evaluation**
- **Interactive REPL (Mutop)**

The interpreter was developed as part of **CMSC330: Organization of Programming Languages** at the University of Maryland.

<br>

## Demo

<p align="center">
  <img src="assets/ex.gif" width="800">
</p>

<p align="center">
  Example interaction with the MicroCaml Mutop REPL demonstrating variable definitions and expression evaluation.
</p>

<br>

## Interpreter Architecture

The project includes an interactive REPL (`Mutop`) built on top of the MicroCaml interpreter.

The execution flow is:

```
User Input
   ↓
bin/mutop.ml        (interactive REPL / CLI loop)
   ↓
src/lexer.ml        (tokenization)
   ↓
src/parser.ml       (parsing)
   ↓
src/eval.ml         (evaluation with environment)
   ↓
Result
```

Core components:

| File | Purpose |
|-----|--------|
| `src/lexer.ml` | Converts source text into tokens |
| `src/parser.ml` | Parses MicroCaml expressions and Mutop commands |
| `src/eval.ml` | Evaluates expressions and Mutop statements |
| `src/microCamlTypes.ml` | Defines AST and runtime types |
| `bin/mutop.ml` | Implements the interactive REPL loop |

`mutop.ml` reads user input until `;;`, invokes the lexer/parser/evaluator pipeline, prints results, and preserves the environment across commands such as `def x = 5;;`.

The evaluator implements **environment-based evaluation with closures**, supporting higher-order functions and recursion.

The formal operational semantics used to guide the evaluator implementation are included in:

```
docs/microcaml-opsem.pdf
```

<br>

## Mutop REPL

The project includes **Mutop**, an interactive environment for executing MicroCaml code.

Mutop supports two kinds of inputs.

### Evaluate an expression

```ocaml
mutop # 3 + 4;;
- : val: Int 7
```

### Define a variable

The `def` command stores variables in the REPL environment.

```ocaml
mutop # def x = 5;;
val x = Int 5
```

The variable can then be used in future expressions:

```ocaml
mutop # x + 2;;
- : val: Int 7
```

<br>

## Language Features

MicroCaml supports a subset of OCaml-style functional programming.

### Primitive values

```
int
bool
```

### Arithmetic operators

```
+  -  *  /
```

Example:

```ocaml
mutop # 1 + 2;;
- : val: Int 3
```

### Boolean operators

```
&&  ||  not
```

### Comparisons

```
=  <>  <  <=  >  >=
```

### Conditionals

```ocaml
if <expr> then <expr> else <expr>
```

Example:

```ocaml
mutop # if true then 1 else 2;;
- : val: Int 1
```

### Let bindings

```ocaml
let x = 6 in x + 4;;
```

### Functions

Anonymous functions:

```ocaml
fun x -> x + 1
```

Function application:

```ocaml
mutop # (fun x -> x + 1) 5;;
- : val: Int 6
```

### Closures

Functions capture their surrounding environment.

```ocaml
mutop # let x = 5 in
    let f = fun y -> x + y in
    f 3;;
- : val: Int 8
```

### Recursive functions

Recursive functions are supported using `let rec`.

```ocaml
mutop # let rec fact = fun n ->
    if n = 0 then 1
    else n * fact (n - 1)
  in fact 5;;
- : val: Int 120
```

<br>

## Project Structure

```
MicroCaml
│
├── src/        interpreter implementation
├── bin/        REPL entry point (mutop)
├── test/       unit and property tests
├── docs/       language semantics documentation
│
├── dune-project
├── dune-workspace
├── mutop.sh
└── README.md
```

<br>

## Requirements

The project uses the OCaml ecosystem with **opam** and the **Dune build system**.

Install dependencies:

```bash
opam install dune utop ounit ounit2 qcheck
```

<br>

## Build

From the repository root:

```bash
dune build
```

You may see deprecation warnings from the testing libraries (for example `QCheck.small_int`).  
These originate from the provided test suite and do not affect interpreter functionality.

<br>

## Running the Interpreter

Start the interactive REPL:

```bash
dune exec bin/mutop.bc
```

Alternatively, run the helper script:

```bash
./mutop.sh
```

<br>

## Running Tests

Run the test suite with:

```bash
dune runtest
```

The tests include both unit tests and property-based tests.

<br>

## Notes

This project builds on starter infrastructure provided in CMSC330, while the interpreter logic (lexing, parsing, and evaluation) was implemented as part of the assignment.
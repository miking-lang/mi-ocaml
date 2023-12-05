# Mi-ocaml

This repository contains an implementation of a subset of OCaml, implemented using Miking, and extended with a number of new features relating to our research.

Basic usage:

```bash
# Make sure the compiler is built
make

# Use the compiler to build an `.ml` file to an executable
build/mi-ml <input>.ml --output <executable>
```

## Limitations

This is a list of the limitations a user is most likely to run into (since this compiler only implements a subset of OCaml):

- We only support the purely functional part of the language; no references, mutable fields, classes, or objects.
- Top-level definitions with an expression on the right-hand side (i.e. `let`) must be terminated with `;;`.
- References to external modules has to be through fully qualified names (e.g., `List.fold_left`); we do not support `open`, nor local opens (e.g., `List.(some expression)`).
- References to external modules are assumed to be correct, i.e., the compiler assumes they exist, and gives them whatever type is needed to make the surrounding code type-check.
- `match` has to be surrounded by `begin` and `end`.
- User defined operators are presently not supported.
- `module` is not supported, nor functors.

## Extensions

### Reptypes

TODO(vipa, 2023-12-05): write a description here

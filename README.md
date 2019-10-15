# Robin

The extensible heterogeneous reasoning platform.

## What is Robin?

Robin is designed to serve as a base from which heterogeneous reasoners can quickly be constructed.
The core of robin is the ability to suggest new representations based on the current state of the problem;
when asked, the system is able to recommend an alternative representation to view the problem.

## Getting started

The project is tested on macOS, but written to be platform-agnostic.
Robin can be built using `make`. It requires the PolyML variant of Standard ML.

The full build instructions, assuming you have the source in the current directory, are:
```
$ make clean
$ make robin
```

To run `robin`, try the following:
```
$ ./dist/robin birds:natlang 5
```
This will make up to five suggestions for the birds problem, as initially stated in natural language.

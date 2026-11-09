# Transition matrix server module

Server-side logic for an editable transition matrix backed by the shared
canonical graph store.

## Usage

``` r
moduleTransitionMatrixServer(id, graphStore = NULL, ..., args = list())
```

## Arguments

- id:

  Module id (namespace) for this server module.

- graphStore:

  Shared `AlphaGraphStore`. A private store is created when omitted.

- ...:

  Additional parameters (reserved for future use).

- args:

  Named list of optional parameters passed to the module.

## Value

Invisibly NULL; registers server outputs and observers.

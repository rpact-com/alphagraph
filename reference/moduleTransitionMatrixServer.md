# Transition matrix server module

Server-side logic for rendering the transition matrix datatable. The
module obtains the example matrix from getTransitionMatrix() and renders
it using DT.

## Usage

``` r
moduleTransitionMatrixServer(id, ..., args = list())
```

## Arguments

- id:

  Module id (namespace) for this server module.

- ...:

  Additional parameters (reserved for future use).

- args:

  Named list of optional parameters passed to the module.

## Value

Invisibly NULL; registers server outputs and observers.

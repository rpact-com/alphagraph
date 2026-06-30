# Analysis panel server module

Server-side logic for the analysis panel. Currently this renders a
placeholder text; in future it can be extended to perform computations
and render results based on user inputs.

## Usage

``` r
moduleAnalysisServer(id, ..., args = list())
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

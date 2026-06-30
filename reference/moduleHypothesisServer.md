# Hypotheses server module

Server-side logic for the hypotheses editor. Renders a dynamic list of
inputs for hypothesis labels, weights and p-values which can be extended
later to perform validation and matrix creation.

## Usage

``` r
moduleHypothesisServer(id, ..., args = list())
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

# Graph editor server module

Server-side logic for the graph editor module. Handles rendering the
demonstration graph, managing node/edge actions and context menu events.

## Usage

``` r
moduleGraphServer(id, ..., args = list())
```

## Arguments

- id:

  Module id (namespace) for this server module.

- ...:

  Additional parameters (reserved for future use).

- args:

  Named list of optional parameters passed to the module.

## Value

Invisibly NULL; the function registers server observers and outputs.

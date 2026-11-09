# Graph editor server module

Server-side logic for the graph editor module. Handles rendering the
demonstration graph, managing node/edge actions and context menu events.

## Usage

``` r
moduleGraphServer(id, graphStore = NULL, ..., args = list())
```

## Arguments

- id:

  Module id (namespace) for this server module.

- graphStore:

  Shared `AlphaGraphStore`. A private store is created when omitted,
  which keeps the module usable in isolation.

- ...:

  Additional parameters (reserved for future use).

- args:

  Named list of optional parameters passed to the module. Supported
  entries include `positionDebounceMillis` and `zoomFactor`; the latter
  must be greater than one and defaults to `1.2`.

## Value

Invisibly NULL; the function registers server observers and outputs.

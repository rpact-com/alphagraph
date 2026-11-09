# Hypotheses server module

Server-side logic for the hypotheses table. Editable labels and weights
are synchronized with the shared graph store. Rendered labels are
read-only previews, while p-values remain local analysis inputs.

## Usage

``` r
moduleHypothesisServer(
  id,
  graphStore = NULL,
  projectStore = NULL,
  datasetId = "primary",
  ...,
  args = list()
)
```

## Arguments

- id:

  Module id (namespace) for this server module.

- graphStore:

  Shared `AlphaGraphStore`. A private store is created when omitted.

- projectStore:

  Optional shared `AlphaProjectStore`. When supplied, p-values are read
  from and written to its selected dataset instead of being kept as
  module-local state.

- datasetId:

  Project dataset used for p-values.

- ...:

  Additional parameters (reserved for future use).

- args:

  Named list of optional parameters passed to the module. The
  `maxHypothesisLabelLength` entry overrides the application-wide option
  `alphagraph.maxHypothesisLabelLength` for this module.

## Value

Invisibly NULL; registers server outputs and observers.

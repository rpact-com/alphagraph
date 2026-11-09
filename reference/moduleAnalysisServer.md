# Analysis panel server module

Runs the first end-to-end graphicalMCP calculation. The graph comes from
the project graph, raw p-values from a project dataset, and the derived
result is stored separately with an input fingerprint and engine
versions.

## Usage

``` r
moduleAnalysisServer(
  id,
  projectStore = NULL,
  analysisId = "shortcut-primary",
  datasetId = "primary",
  ...,
  args = list()
)
```

## Arguments

- id:

  Module id.

- projectStore:

  Shared `AlphaProjectStore`.

- analysisId:

  Shortcut analysis specification to update and run.

- datasetId:

  Dataset containing raw p-values.

- ...:

  Additional parameters reserved for future use.

- args:

  Optional module parameters.

## Value

Invisibly, the project store.

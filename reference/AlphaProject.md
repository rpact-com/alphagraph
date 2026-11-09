# JSON-friendly alphagraph project model

Stores a reusable `AlphaGraph` together with p-value datasets, analysis
specifications, optional derived results, project UI state, and
metadata. Raw and adjusted p-values deliberately remain outside the
graph definition.

## Value

An R6 class generator.

## Details

Create an instance with `AlphaProject$new()`. P-values use stable
hypothesis identifiers. Dataset and analysis updates remove affected
results; graph replacement removes all results. Use `$resultIsCurrent()`
before displaying a stored result and
`$validate(requireComputable = TRUE, analysisId = ...)` before a
calculation. Derived results contain input hashes and engine versions.
Mutating methods return the instance invisibly unless stated otherwise.

## Methods

### Public methods

- [`AlphaProject$new()`](#method-AlphaProject-initialize)

- [`AlphaProject$loadList()`](#method-AlphaProject-loadList)

- [`AlphaProject$toList()`](#method-AlphaProject-toList)

- [`AlphaProject$toJson()`](#method-AlphaProject-toJson)

- [`AlphaProject$graph()`](#method-AlphaProject-graph)

- [`AlphaProject$setGraph()`](#method-AlphaProject-setGraph)

- [`AlphaProject$datasets()`](#method-AlphaProject-datasets)

- [`AlphaProject$analyses()`](#method-AlphaProject-analyses)

- [`AlphaProject$results()`](#method-AlphaProject-results)

- [`AlphaProject$dataset()`](#method-AlphaProject-dataset)

- [`AlphaProject$analysis()`](#method-AlphaProject-analysis)

- [`AlphaProject$result()`](#method-AlphaProject-result)

- [`AlphaProject$upsertDataset()`](#method-AlphaProject-upsertDataset)

- [`AlphaProject$pValues()`](#method-AlphaProject-pValues)

- [`AlphaProject$upsertAnalysis()`](#method-AlphaProject-upsertAnalysis)

- [`AlphaProject$inputHash()`](#method-AlphaProject-inputHash)

- [`AlphaProject$resultIsCurrent()`](#method-AlphaProject-resultIsCurrent)

- [`AlphaProject$runShortcut()`](#method-AlphaProject-runShortcut)

- [`AlphaProject$validate()`](#method-AlphaProject-validate)

- [`AlphaProject$clone()`](#method-AlphaProject-clone)

------------------------------------------------------------------------

### `AlphaProject$new()`

Initialize a project containing a graph and empty datasets, analyses,
and results.

#### Usage

    AlphaProject$new(
      graph = createDemoAlphaGraph(),
      id = "project",
      label = id,
      description = NULL,
      ui = list(),
      metadata = list()
    )

#### Arguments

- `graph`:

  An AlphaGraph instance; the demo graph is used when omitted at
  initialization.

- `id`:

  Character scalar identifying the object within its collection.

- `label`:

  Human-readable character label.

- `description`:

  Optional character description; NULL omits it.

- `ui`:

  Named list of JSON-compatible project UI state; empty by default.

- `metadata`:

  Named list of JSON-compatible metadata; empty by default.

------------------------------------------------------------------------

### `AlphaProject$loadList()`

Restore a JSON-compatible project list and optionally validate it.

#### Usage

    AlphaProject$loadList(x, validate = TRUE)

#### Arguments

- `x`:

  Canonical JSON-compatible list.

- `validate`:

  Logical; whether to validate the imported, returned, or modified
  object.

------------------------------------------------------------------------

### `AlphaProject$toList()`

Return the JSON-compatible project list including stored results.

#### Usage

    AlphaProject$toList()

------------------------------------------------------------------------

### `AlphaProject$toJson()`

Serialize the project with jsonlite::toJSON(); ... supplies additional
JSON options.

#### Usage

    AlphaProject$toJson(pretty = TRUE, auto_unbox = TRUE, null = "null", ...)

#### Arguments

- `pretty`:

  Logical; whether to indent JSON output. Defaults to TRUE.

- `auto_unbox`:

  Logical; whether to unbox JSON scalars. Defaults to TRUE.

- `null`:

  Character NULL encoding passed to jsonlite::toJSON(); defaults to
  "null".

- `...`:

  Named arguments passed to the operation described above.

------------------------------------------------------------------------

### `AlphaProject$graph()`

Return an independent AlphaGraph restored from the embedded graph
definition.

#### Usage

    AlphaProject$graph()

------------------------------------------------------------------------

### `AlphaProject$setGraph()`

Replace the embedded graph and clear all derived results.

#### Usage

    AlphaProject$setGraph(graph)

#### Arguments

- `graph`:

  An AlphaGraph instance; the demo graph is used when omitted at
  initialization.

------------------------------------------------------------------------

### `AlphaProject$datasets()`

Return the list of supplied p-value datasets.

#### Usage

    AlphaProject$datasets()

------------------------------------------------------------------------

### `AlphaProject$analyses()`

Return the list of stored analysis specifications.

#### Usage

    AlphaProject$analyses()

------------------------------------------------------------------------

### `AlphaProject$results()`

Return the list of stored derived results, including any stale results.

#### Usage

    AlphaProject$results()

------------------------------------------------------------------------

### `AlphaProject$dataset()`

Return the dataset with the requested identifier; error if it does not
exist.

#### Usage

    AlphaProject$dataset(id)

#### Arguments

- `id`:

  Character scalar identifying the object within its collection.

------------------------------------------------------------------------

### `AlphaProject$analysis()`

Return the analysis with the requested identifier; error if it does not
exist.

#### Usage

    AlphaProject$analysis(id)

#### Arguments

- `id`:

  Character scalar identifying the object within its collection.

------------------------------------------------------------------------

### `AlphaProject$result()`

Return the latest stored result for an analysis, or NULL; use
resultIsCurrent() to check validity.

#### Usage

    AlphaProject$result(analysisId)

#### Arguments

- `analysisId`:

  Character identifier of an analysis specification.

------------------------------------------------------------------------

### `AlphaProject$upsertDataset()`

Insert or replace a p-value dataset and remove results for affected
analyses.

#### Usage

    AlphaProject$upsertDataset(
      id,
      pValues,
      label = id,
      description = NULL,
      metadata = list()
    )

#### Arguments

- `id`:

  Character scalar identifying the object within its collection.

- `pValues`:

  Named numeric vector or list of hypothesisId/value records, keyed by
  stable hypothesis IDs.

- `label`:

  Human-readable character label.

- `description`:

  Optional character description; NULL omits it.

- `metadata`:

  Named list of JSON-compatible metadata; empty by default.

------------------------------------------------------------------------

### `AlphaProject$pValues()`

Return a numeric p-value vector named by stable hypothesis IDs.

#### Usage

    AlphaProject$pValues(datasetId)

#### Arguments

- `datasetId`:

  Character identifier of a p-value dataset.

------------------------------------------------------------------------

### `AlphaProject$upsertAnalysis()`

Insert or replace an analysis specification and remove its derived
results.

#### Usage

    AlphaProject$upsertAnalysis(
      id,
      datasetId,
      method = c("shortcut", "closure"),
      alpha = 0.025,
      testGroups = list(),
      verbose = FALSE,
      testValues = FALSE,
      label = id,
      description = NULL,
      metadata = list()
    )

#### Arguments

- `id`:

  Character scalar identifying the object within its collection.

- `datasetId`:

  Character identifier of a p-value dataset.

- `method`:

  Character choice of "shortcut" or "closure"; only shortcut execution
  is implemented.

- `alpha`:

  Numeric significance level in (0, 1\]; defaults to 0.025.

- `testGroups`:

  List of closure groups; empty by default. Each group has
  hypothesisIds, optional id and label, testType (bonferroni,
  parametric, simes, or hochberg), and a correlation matrix for
  parametric tests. Groups must partition the hypotheses.

- `verbose`:

  Logical passed to graphicalMCP shortcut execution; defaults to FALSE.

- `testValues`:

  Logical passed as test_values to graphicalMCP; defaults to FALSE.

- `label`:

  Human-readable character label.

- `description`:

  Optional character description; NULL omits it.

- `metadata`:

  Named list of JSON-compatible metadata; empty by default.

------------------------------------------------------------------------

### `AlphaProject$inputHash()`

Return a fingerprint of the graph, dataset, and analysis inputs.

#### Usage

    AlphaProject$inputHash(analysisId)

#### Arguments

- `analysisId`:

  Character identifier of an analysis specification.

------------------------------------------------------------------------

### `AlphaProject$resultIsCurrent()`

Return TRUE only when a stored result matches the current input
fingerprint.

#### Usage

    AlphaProject$resultIsCurrent(analysisId)

#### Arguments

- `analysisId`:

  Character identifier of an analysis specification.

------------------------------------------------------------------------

### `AlphaProject$runShortcut()`

Run graphicalMCP shortcut testing and invisibly return the stored result
with input hash and engine versions.

#### Usage

    AlphaProject$runShortcut(analysisId)

#### Arguments

- `analysisId`:

  Character identifier of an analysis specification.

------------------------------------------------------------------------

### `AlphaProject$validate()`

Validate project structure and optionally computability; return TRUE on
success.

#### Usage

    AlphaProject$validate(
      requireComputable = FALSE,
      analysisId = NULL,
      tolerance = sqrt(.Machine$double.eps)
    )

#### Arguments

- `requireComputable`:

  Logical; whether to require complete numeric inputs for the selected
  analysis.

- `analysisId`:

  Character identifier of an analysis specification. Required when
  requireComputable is TRUE.

- `tolerance`:

  Numeric validation tolerance; defaults to sqrt(.Machine\$double.eps).

------------------------------------------------------------------------

### `AlphaProject$clone()`

The objects of this class are cloneable with this method.

#### Usage

    AlphaProject$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
project <- createDemoAlphaProject()
project$pValues("primary")
#> H1_NI  H1_S H2_NI  H2_S 
#> 0.012 0.001 0.030 0.162 
project$validate(requireComputable = TRUE, analysisId = "shortcut-primary")
#> [1] TRUE
```

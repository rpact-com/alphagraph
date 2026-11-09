# Reactive store for an AlphaProject

Owns project datasets, analysis specifications, and results while
exposing the embedded graph through the existing `AlphaGraphStore`. A
project snapshot always embeds the current graph-store snapshot.

## Value

An R6 class generator.

## Details

Snapshots embed the current graph-store snapshot. Dataset and analysis
updates invalidate affected results; graph edits are detected by input
hashes. `$result()` returns only results for current inputs. Use
reactive accessors in a reactive context or inside
[`shiny::isolate()`](https://rdrr.io/pkg/shiny/man/isolate.html).
Transactions commit project data; graph changes use the shared
`graphStore`.

## Public fields

- `graphStore`:

  AlphaGraphStore holding the current project graph; initialized from
  the supplied project and shared with graph views.

## Methods

### Public methods

- [`AlphaProjectStore$new()`](#method-AlphaProjectStore-initialize)

- [`AlphaProjectStore$revision()`](#method-AlphaProjectStore-revision)

- [`AlphaProjectStore$source()`](#method-AlphaProjectStore-source)

- [`AlphaProjectStore$error()`](#method-AlphaProjectStore-error)

- [`AlphaProjectStore$snapshot()`](#method-AlphaProjectStore-snapshot)

- [`AlphaProjectStore$project()`](#method-AlphaProjectStore-project)

- [`AlphaProjectStore$pValues()`](#method-AlphaProjectStore-pValues)

- [`AlphaProjectStore$setPValues()`](#method-AlphaProjectStore-setPValues)

- [`AlphaProjectStore$upsertAnalysis()`](#method-AlphaProjectStore-upsertAnalysis)

- [`AlphaProjectStore$result()`](#method-AlphaProjectStore-result)

- [`AlphaProjectStore$runShortcut()`](#method-AlphaProjectStore-runShortcut)

- [`AlphaProjectStore$replace()`](#method-AlphaProjectStore-replace)

- [`AlphaProjectStore$transact()`](#method-AlphaProjectStore-transact)

- [`AlphaProjectStore$clone()`](#method-AlphaProjectStore-clone)

------------------------------------------------------------------------

### `AlphaProjectStore$new()`

Initialize the session store from a validated independent copy.

#### Usage

    AlphaProjectStore$new(project = createDemoAlphaProject())

#### Arguments

- `project`:

  An AlphaProject instance; the demo project is used when omitted at
  initialization.

------------------------------------------------------------------------

### `AlphaProjectStore$revision()`

Return the reactive revision counter used by dependent views.

#### Usage

    AlphaProjectStore$revision()

------------------------------------------------------------------------

### `AlphaProjectStore$source()`

Return the reactive source label of the last committed store change.

#### Usage

    AlphaProjectStore$source()

------------------------------------------------------------------------

### `AlphaProjectStore$error()`

Return the reactive message from the last failed transaction, or NULL.

#### Usage

    AlphaProjectStore$error()

------------------------------------------------------------------------

### `AlphaProjectStore$snapshot()`

Return an independent snapshot without registering reactive
dependencies.

#### Usage

    AlphaProjectStore$snapshot(validate = FALSE)

#### Arguments

- `validate`:

  Logical; whether to validate the imported, returned, or modified
  object.

------------------------------------------------------------------------

### `AlphaProjectStore$project()`

Return an independent AlphaProject with the current graph and register
revision dependencies.

#### Usage

    AlphaProjectStore$project(validate = FALSE)

#### Arguments

- `validate`:

  Logical; whether to validate the imported, returned, or modified
  object.

------------------------------------------------------------------------

### `AlphaProjectStore$pValues()`

Return dataset p-values and register a reactive revision dependency.

#### Usage

    AlphaProjectStore$pValues(datasetId = "primary")

#### Arguments

- `datasetId`:

  Character identifier of a p-value dataset.

------------------------------------------------------------------------

### `AlphaProjectStore$setPValues()`

Insert or replace dataset p-values in a transaction, preserving existing
dataset metadata.

#### Usage

    AlphaProjectStore$setPValues(
      pValues,
      datasetId = "primary",
      label = datasetId,
      source = "project:p-values"
    )

#### Arguments

- `pValues`:

  Named numeric vector or list of hypothesisId/value records, keyed by
  stable hypothesis IDs.

- `datasetId`:

  Character identifier of a p-value dataset.

- `label`:

  Human-readable character label.

- `source`:

  Character scalar identifying the origin of a committed change.

------------------------------------------------------------------------

### `AlphaProjectStore$upsertAnalysis()`

Forward ... to AlphaProject\$upsertAnalysis() in a transaction.

#### Usage

    AlphaProjectStore$upsertAnalysis(..., source = "project:analysis")

#### Arguments

- `...`:

  Named arguments passed to the operation described above.

- `source`:

  Character scalar identifying the origin of a committed change.

------------------------------------------------------------------------

### `AlphaProjectStore$result()`

Return a stored analysis result only if its input fingerprint is
current; otherwise return NULL.

#### Usage

    AlphaProjectStore$result(analysisId)

#### Arguments

- `analysisId`:

  Character identifier of an analysis specification.

------------------------------------------------------------------------

### `AlphaProjectStore$runShortcut()`

Run shortcut testing in a validated transaction and invisibly return the
result.

#### Usage

    AlphaProjectStore$runShortcut(analysisId, source = "project:shortcut")

#### Arguments

- `analysisId`:

  Character identifier of an analysis specification.

- `source`:

  Character scalar identifying the origin of a committed change.

------------------------------------------------------------------------

### `AlphaProjectStore$replace()`

Replace the stored object and update its revision and source label.

#### Usage

    AlphaProjectStore$replace(project, source = "external")

#### Arguments

- `project`:

  An AlphaProject instance; the demo project is used when omitted at
  initialization.

- `source`:

  Character scalar identifying the origin of a committed change.

------------------------------------------------------------------------

### `AlphaProjectStore$transact()`

Apply mutate to a private candidate and commit after optional
validation. Failures preserve the previous state and are rethrown.
Return invisible TRUE on commit or FALSE for a nested transaction.

#### Usage

    AlphaProjectStore$transact(source, mutate, validate = FALSE)

#### Arguments

- `source`:

  Character scalar identifying the origin of a committed change.

- `mutate`:

  Function that modifies a private candidate object before it is
  committed.

- `validate`:

  Logical; whether to validate the imported, returned, or modified
  object.

------------------------------------------------------------------------

### `AlphaProjectStore$clone()`

The objects of this class are cloneable with this method.

#### Usage

    AlphaProjectStore$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
store <- createAlphaProjectStore()
store$snapshot()$pValues("primary")
#> H1_NI  H1_S H2_NI  H2_S 
#> 0.012 0.001 0.030 0.162 
shiny::isolate(store$revision())
#> [1] 0
```

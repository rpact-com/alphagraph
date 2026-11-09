# Reactive store for a shared AlphaGraph

Holds one canonical `AlphaGraph` per Shiny session. All mutations are
applied to a private copy and committed atomically by increasing a
revision counter. Views depend on the revision; view rendering never
writes back to the store, preventing graph/matrix feedback loops.

## Value

An R6 class generator.

## Details

Use `$transact(source, mutate)` for changes. Nested transactions are
ignored and failed transactions preserve the previous graph. Reactive
accessors must be used inside a reactive context, or inside
[`shiny::isolate()`](https://rdrr.io/pkg/shiny/man/isolate.html).
`$snapshot()` returns an independent graph without reactive
dependencies.

## Methods

### Public methods

- [`AlphaGraphStore$new()`](#method-AlphaGraphStore-initialize)

- [`AlphaGraphStore$graph()`](#method-AlphaGraphStore-graph)

- [`AlphaGraphStore$revision()`](#method-AlphaGraphStore-revision)

- [`AlphaGraphStore$source()`](#method-AlphaGraphStore-source)

- [`AlphaGraphStore$error()`](#method-AlphaGraphStore-error)

- [`AlphaGraphStore$snapshot()`](#method-AlphaGraphStore-snapshot)

- [`AlphaGraphStore$replace()`](#method-AlphaGraphStore-replace)

- [`AlphaGraphStore$transact()`](#method-AlphaGraphStore-transact)

- [`AlphaGraphStore$clone()`](#method-AlphaGraphStore-clone)

------------------------------------------------------------------------

### `AlphaGraphStore$new()`

Initialize the session store from a validated independent copy.

#### Usage

    AlphaGraphStore$new(graph = createDemoAlphaGraph())

#### Arguments

- `graph`:

  An AlphaGraph instance; the demo graph is used when omitted at
  initialization.

------------------------------------------------------------------------

### `AlphaGraphStore$graph()`

Return the current canonical AlphaGraph and register a reactive revision
dependency.

#### Usage

    AlphaGraphStore$graph()

------------------------------------------------------------------------

### `AlphaGraphStore$revision()`

Return the reactive revision counter used by dependent views.

#### Usage

    AlphaGraphStore$revision()

------------------------------------------------------------------------

### `AlphaGraphStore$source()`

Return the reactive source label of the last committed store change.

#### Usage

    AlphaGraphStore$source()

------------------------------------------------------------------------

### `AlphaGraphStore$error()`

Return the reactive message from the last failed transaction, or NULL.

#### Usage

    AlphaGraphStore$error()

------------------------------------------------------------------------

### `AlphaGraphStore$snapshot()`

Return an independent snapshot without registering reactive
dependencies.

#### Usage

    AlphaGraphStore$snapshot()

------------------------------------------------------------------------

### `AlphaGraphStore$replace()`

Replace the stored object and update its revision and source label.

#### Usage

    AlphaGraphStore$replace(graph, source = "external", validate = TRUE)

#### Arguments

- `graph`:

  An AlphaGraph instance; the demo graph is used when omitted at
  initialization.

- `source`:

  Character scalar identifying the origin of a committed change.

- `validate`:

  Logical; whether to validate the imported, returned, or modified
  object.

------------------------------------------------------------------------

### `AlphaGraphStore$transact()`

Apply mutate to a private candidate and commit after optional
validation. Failures preserve the previous state and are rethrown.
Return invisible TRUE on commit or FALSE for a nested transaction.

#### Usage

    AlphaGraphStore$transact(
      source,
      mutate,
      validate = TRUE,
      requireNumeric = FALSE
    )

#### Arguments

- `source`:

  Character scalar identifying the origin of a committed change.

- `mutate`:

  Function that modifies a private candidate object before it is
  committed.

- `validate`:

  Logical; whether to validate the imported, returned, or modified
  object.

- `requireNumeric`:

  Logical; whether weights and transitions must be numerically
  computable.

------------------------------------------------------------------------

### `AlphaGraphStore$clone()`

The objects of this class are cloneable with this method.

#### Usage

    AlphaGraphStore$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
store <- createAlphaGraphStore()
store$snapshot()$toHypotheses()
#> H1_NI  H1_S H2_NI  H2_S 
#>     1     0     0     0 
shiny::isolate(store$revision())
#> [1] 0
```

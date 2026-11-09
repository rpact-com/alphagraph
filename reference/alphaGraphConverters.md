# Create an AlphaGraph from external representations

Construct a canonical graph from a list, JSON, a transition matrix, a
graphicalMCP initial graph, or visNetwork node and edge data frames.

## Usage

``` r
alphaGraphFromList(x, validate = TRUE)

alphaGraphFromJson(json, validate = TRUE)

alphaGraphFromTransitionMatrix(
  transitions,
  hypotheses = NULL,
  type = c("value", "expression", "display"),
  nodeIds = NULL,
  nodeLabels = NULL,
  id = "graph",
  label = id,
  description = NULL,
  metadata = list(),
  settings = list(),
  validate = TRUE
)

alphaGraphFromGraphicalMCP(
  graph,
  id = "graph",
  label = id,
  description = NULL,
  metadata = list(),
  settings = list(),
  validate = TRUE
)

alphaGraphFromVisNetworkData(
  nodes,
  edges,
  id = "graph",
  label = id,
  description = NULL,
  metadata = list(),
  settings = list(),
  validate = TRUE
)
```

## Arguments

- x:

  A JSON-compatible alphagraph list.

- validate:

  Whether to validate the resulting graph.

- json:

  JSON text, a connection, or a file path accepted by
  [`jsonlite::fromJSON()`](https://jeroen.r-universe.dev/jsonlite/reference/fromJSON.html).

- transitions:

  Square transition matrix or data frame.

- hypotheses:

  Optional hypothesis weights, in matrix order.

- type:

  Interpretation of non-zero transition cells.

- nodeIds:

  Stable node identifiers. Defaults to matrix row names.

- nodeLabels:

  Display labels. Defaults to `nodeIds`.

- id, label, description, metadata, settings:

  Graph-level properties.

- graph:

  An `initial_graph` object returned by
  [`graphicalMCP::graph_create()`](https://openpharma.github.io/graphicalMCP/reference/graph_create.html)
  or an equivalent list with `hypotheses` and `transitions`.

- nodes, edges:

  Data frames accepted by
  [`visNetwork::visNetwork()`](https://rdrr.io/pkg/visNetwork/man/visNetwork.html).

## Value

An `AlphaGraph` object.

# JSON-friendly alphagraph graph model

R6 container for a canonical alphagraph graph definition. The internal
data model is a plain R list that can be serialized with jsonlite and
converted to graphicalMCP inputs, editable transition matrices, and
visNetwork data.

## Value

An R6 class generator.

## Details

Create an instance with `AlphaGraph$new()`. Nodes represent hypotheses,
edges represent directed transitions, and nested `vis` fields hold
presentation properties. Symbolic drafts remain editable; numeric
validation is required before passing inputs to graphicalMCP. Mutating
methods return the instance invisibly unless their description states
otherwise.

## Methods

### Public methods

- [`AlphaGraph$new()`](#method-AlphaGraph-initialize)

- [`AlphaGraph$addParameter()`](#method-AlphaGraph-addParameter)

- [`AlphaGraph$setParameter()`](#method-AlphaGraph-setParameter)

- [`AlphaGraph$addNode()`](#method-AlphaGraph-addNode)

- [`AlphaGraph$removeNode()`](#method-AlphaGraph-removeNode)

- [`AlphaGraph$addEdge()`](#method-AlphaGraph-addEdge)

- [`AlphaGraph$removeEdge()`](#method-AlphaGraph-removeEdge)

- [`AlphaGraph$setNodeLabel()`](#method-AlphaGraph-setNodeLabel)

- [`AlphaGraph$setNodeWeight()`](#method-AlphaGraph-setNodeWeight)

- [`AlphaGraph$setNodeVisLabel()`](#method-AlphaGraph-setNodeVisLabel)

- [`AlphaGraph$setNodeXY()`](#method-AlphaGraph-setNodeXY)

- [`AlphaGraph$setNodeColor()`](#method-AlphaGraph-setNodeColor)

- [`AlphaGraph$setNodeFontSize()`](#method-AlphaGraph-setNodeFontSize)

- [`AlphaGraph$setNodeVis()`](#method-AlphaGraph-setNodeVis)

- [`AlphaGraph$setEdgeLabel()`](#method-AlphaGraph-setEdgeLabel)

- [`AlphaGraph$setEdgeTransition()`](#method-AlphaGraph-setEdgeTransition)

- [`AlphaGraph$setTransitionWeight()`](#method-AlphaGraph-setTransitionWeight)

- [`AlphaGraph$setEdgeColor()`](#method-AlphaGraph-setEdgeColor)

- [`AlphaGraph$setEdgeFontSize()`](#method-AlphaGraph-setEdgeFontSize)

- [`AlphaGraph$setEdgeSmooth()`](#method-AlphaGraph-setEdgeSmooth)

- [`AlphaGraph$setEdgeVis()`](#method-AlphaGraph-setEdgeVis)

- [`AlphaGraph$setVisOptions()`](#method-AlphaGraph-setVisOptions)

- [`AlphaGraph$numberFormat()`](#method-AlphaGraph-numberFormat)

- [`AlphaGraph$setNumberFormat()`](#method-AlphaGraph-setNumberFormat)

- [`AlphaGraph$parseNumber()`](#method-AlphaGraph-parseNumber)

- [`AlphaGraph$formatNumber()`](#method-AlphaGraph-formatNumber)

- [`AlphaGraph$loadList()`](#method-AlphaGraph-loadList)

- [`AlphaGraph$updateFromTransitionMatrix()`](#method-AlphaGraph-updateFromTransitionMatrix)

- [`AlphaGraph$updateFromVisNetworkData()`](#method-AlphaGraph-updateFromVisNetworkData)

- [`AlphaGraph$fromJson()`](#method-AlphaGraph-fromJson)

- [`AlphaGraph$toList()`](#method-AlphaGraph-toList)

- [`AlphaGraph$toJson()`](#method-AlphaGraph-toJson)

- [`AlphaGraph$toJSON()`](#method-AlphaGraph-toJSON)

- [`AlphaGraph$toJason()`](#method-AlphaGraph-toJason)

- [`AlphaGraph$toTransitionMatrix()`](#method-AlphaGraph-toTransitionMatrix)

- [`AlphaGraph$toHypotheses()`](#method-AlphaGraph-toHypotheses)

- [`AlphaGraph$toGraphicalMCPInput()`](#method-AlphaGraph-toGraphicalMCPInput)

- [`AlphaGraph$toGraphicalMCP()`](#method-AlphaGraph-toGraphicalMCP)

- [`AlphaGraph$toVisNetworkData()`](#method-AlphaGraph-toVisNetworkData)

- [`AlphaGraph$toVisNetwork()`](#method-AlphaGraph-toVisNetwork)

- [`AlphaGraph$validate()`](#method-AlphaGraph-validate)

- [`AlphaGraph$clone()`](#method-AlphaGraph-clone)

------------------------------------------------------------------------

### `AlphaGraph$new()`

Initialize an empty canonical graph with default presentation settings.

#### Usage

    AlphaGraph$new(
      id = "graph",
      label = id,
      description = NULL,
      metadata = list(),
      settings = list()
    )

#### Arguments

- `id`:

  Character scalar identifying the object within its collection.

- `label`:

  Human-readable character label.

- `description`:

  Optional character description; NULL omits it.

- `metadata`:

  Named list of JSON-compatible metadata; empty by default.

- `settings`:

  Named list of graph settings merged with the defaults.

------------------------------------------------------------------------

### `AlphaGraph$addParameter()`

Add a symbolic parameter, optionally replacing an existing parameter.

#### Usage

    AlphaGraph$addParameter(
      id,
      value = NULL,
      label = id,
      description = NULL,
      replace = FALSE
    )

#### Arguments

- `id`:

  Character scalar identifying the object within its collection.

- `value`:

  Optional numeric scalar supplying the stored numeric value.

- `label`:

  Human-readable character label.

- `description`:

  Optional character description; NULL omits it.

- `replace`:

  Logical; whether an existing identifier may be replaced.

------------------------------------------------------------------------

### `AlphaGraph$setParameter()`

Update non-NULL properties of an existing symbolic parameter.

#### Usage

    AlphaGraph$setParameter(id, value = NULL, label = NULL, description = NULL)

#### Arguments

- `id`:

  Character scalar identifying the object within its collection.

- `value`:

  Optional numeric scalar supplying the stored numeric value.

- `label`:

  Human-readable character label.

- `description`:

  Optional character description; NULL omits it.

------------------------------------------------------------------------

### `AlphaGraph$addNode()`

Add a hypothesis with a stable identifier, weight, and presentation
properties.

#### Usage

    AlphaGraph$addNode(
      id,
      label = id,
      weight = 0,
      weightLabel = NULL,
      weightExpression = NULL,
      weightValue = NULL,
      x = NULL,
      y = NULL,
      visLabel = NULL,
      shape = "circle",
      colorBackground = "white",
      colorBorder = "black",
      borderWidth = 2,
      fontSize = 14,
      vis = list(),
      metadata = list(),
      replace = FALSE
    )

#### Arguments

- `id`:

  Character scalar identifying the object within its collection.

- `label`:

  Human-readable character label.

- `weight`:

  Numeric weight, fraction string, or symbolic expression.

- `weightLabel`:

  Optional character display label for the hypothesis weight.

- `weightExpression`:

  Optional character expression for the hypothesis weight.

- `weightValue`:

  Optional numeric value for the hypothesis weight.

- `x`:

  Numeric scalar or fraction string to parse or format.

- `y`:

  Vertical numeric coordinate, or NULL for automatic placement.

- `visLabel`:

  Optional character network label; derived from label and weight by
  default.

- `shape`:

  Character visNetwork node shape; defaults to "circle".

- `colorBackground`:

  Node background color; defaults to "white".

- `colorBorder`:

  Node border color; defaults to "black".

- `borderWidth`:

  Numeric node border width; defaults to 2.

- `fontSize`:

  Numeric label font size in pixels.

- `vis`:

  Named list of additional visNetwork presentation properties.

- `metadata`:

  Named list of JSON-compatible metadata; empty by default.

- `replace`:

  Logical; whether an existing identifier may be replaced.

------------------------------------------------------------------------

### `AlphaGraph$removeNode()`

Remove a hypothesis and, by default, its incident edges.

#### Usage

    AlphaGraph$removeNode(id, removeEdges = TRUE)

#### Arguments

- `id`:

  Character scalar identifying the object within its collection.

- `removeEdges`:

  Logical; whether incident edges are also removed. Defaults to TRUE.

------------------------------------------------------------------------

### `AlphaGraph$addEdge()`

Add a directed transition between distinct existing hypotheses.

#### Usage

    AlphaGraph$addEdge(
      from,
      to,
      transition = NULL,
      label = NULL,
      expression = NULL,
      value = NULL,
      id = NULL,
      arrows = "to",
      smooth = FALSE,
      fontSize = 15,
      fontAlign = "top",
      color = NULL,
      vis = list(),
      metadata = list(),
      replace = FALSE
    )

#### Arguments

- `from`:

  Character identifier of the source hypothesis.

- `to`:

  Character identifier of the target hypothesis.

- `transition`:

  Optional numeric transition, fraction string, or symbolic expression.

- `label`:

  Human-readable character label.

- `expression`:

  Optional character expression for the stored weight or transition.

- `value`:

  Optional numeric scalar supplying the stored numeric value.

- `id`:

  Character scalar identifying the object within its collection.

- `arrows`:

  visNetwork arrow specification; defaults to "to".

- `smooth`:

  Logical or list specifying visNetwork edge smoothing.

- `fontSize`:

  Numeric label font size in pixels.

- `fontAlign`:

  Character edge-label alignment; defaults to "top".

- `color`:

  Optional visNetwork color specification.

- `vis`:

  Named list of additional visNetwork presentation properties.

- `metadata`:

  Named list of JSON-compatible metadata; empty by default.

- `replace`:

  Logical; whether an existing identifier may be replaced.

------------------------------------------------------------------------

### `AlphaGraph$removeEdge()`

Remove a directed transition and refresh reciprocal-edge smoothing.

#### Usage

    AlphaGraph$removeEdge(edge = NULL, from = NULL, to = NULL)

#### Arguments

- `edge`:

  Optional character edge identifier; otherwise use from and to.

- `from`:

  Character identifier of the source hypothesis.

- `to`:

  Character identifier of the target hypothesis.

------------------------------------------------------------------------

### `AlphaGraph$setNodeLabel()`

Set a canonical hypothesis label and optionally refresh its network
label.

#### Usage

    AlphaGraph$setNodeLabel(id, label, updateVisLabel = TRUE)

#### Arguments

- `id`:

  Character scalar identifying the object within its collection.

- `label`:

  Human-readable character label.

- `updateVisLabel`:

  Logical; whether to regenerate the network label. Defaults to TRUE.

------------------------------------------------------------------------

### `AlphaGraph$setNodeWeight()`

Update the weight specification and optionally refresh the network
label.

#### Usage

    AlphaGraph$setNodeWeight(
      id,
      weight = NULL,
      label = NULL,
      expression = NULL,
      value = NULL,
      updateVisLabel = TRUE
    )

#### Arguments

- `id`:

  Character scalar identifying the object within its collection.

- `weight`:

  Numeric weight, fraction string, or symbolic expression.

- `label`:

  Human-readable character label.

- `expression`:

  Optional character expression for the stored weight or transition.

- `value`:

  Optional numeric scalar supplying the stored numeric value.

- `updateVisLabel`:

  Logical; whether to regenerate the network label. Defaults to TRUE.

------------------------------------------------------------------------

### `AlphaGraph$setNodeVisLabel()`

Set the presentation label without changing the canonical hypothesis
label.

#### Usage

    AlphaGraph$setNodeVisLabel(id, label)

#### Arguments

- `id`:

  Character scalar identifying the object within its collection.

- `label`:

  Human-readable character label.

------------------------------------------------------------------------

### `AlphaGraph$setNodeXY()`

Set the stored network coordinates of a hypothesis.

#### Usage

    AlphaGraph$setNodeXY(id, x, y)

#### Arguments

- `id`:

  Character scalar identifying the object within its collection.

- `x`:

  Horizontal numeric coordinate, or NULL for automatic placement.

- `y`:

  Vertical numeric coordinate, or NULL for automatic placement.

------------------------------------------------------------------------

### `AlphaGraph$setNodeColor()`

Update non-NULL node color properties.

#### Usage

    AlphaGraph$setNodeColor(
      id,
      background = NULL,
      border = NULL,
      highlight = NULL,
      hover = NULL
    )

#### Arguments

- `id`:

  Character scalar identifying the object within its collection.

- `background`:

  Optional node background color.

- `border`:

  Optional node border color.

- `highlight`:

  Optional visNetwork highlight color or color list.

- `hover`:

  Optional visNetwork hover color or color list.

------------------------------------------------------------------------

### `AlphaGraph$setNodeFontSize()`

Set the node label font size.

#### Usage

    AlphaGraph$setNodeFontSize(id, size)

#### Arguments

- `id`:

  Character scalar identifying the object within its collection.

- `size`:

  Numeric font size in pixels, or NULL to clear the stored size.

------------------------------------------------------------------------

### `AlphaGraph$setNodeVis()`

Merge named presentation properties from ... into the node vis list.

#### Usage

    AlphaGraph$setNodeVis(id, ...)

#### Arguments

- `id`:

  Character scalar identifying the object within its collection.

- `...`:

  Named arguments passed to the operation described above.

------------------------------------------------------------------------

### `AlphaGraph$setEdgeLabel()`

Update the transition display label and optional expression or numeric
value.

#### Usage

    AlphaGraph$setEdgeLabel(
      edge = NULL,
      label,
      from = NULL,
      to = NULL,
      expression = NULL,
      value = NULL
    )

#### Arguments

- `edge`:

  Optional character edge identifier; otherwise use from and to.

- `label`:

  Human-readable character label.

- `from`:

  Character identifier of the source hypothesis.

- `to`:

  Character identifier of the target hypothesis.

- `expression`:

  Optional character expression for the stored weight or transition.

- `value`:

  Optional numeric scalar supplying the stored numeric value.

------------------------------------------------------------------------

### `AlphaGraph$setEdgeTransition()`

Update the transition specification and refresh its network label.

#### Usage

    AlphaGraph$setEdgeTransition(
      edge = NULL,
      transition = NULL,
      label = NULL,
      expression = NULL,
      value = NULL,
      from = NULL,
      to = NULL
    )

#### Arguments

- `edge`:

  Optional character edge identifier; otherwise use from and to.

- `transition`:

  Optional numeric transition, fraction string, or symbolic expression.

- `label`:

  Human-readable character label.

- `expression`:

  Optional character expression for the stored weight or transition.

- `value`:

  Optional numeric scalar supplying the stored numeric value.

- `from`:

  Character identifier of the source hypothesis.

- `to`:

  Character identifier of the target hypothesis.

------------------------------------------------------------------------

### `AlphaGraph$setTransitionWeight()`

Set a numeric transition between 0 and 1, removing an edge when its
weight is zero.

#### Usage

    AlphaGraph$setTransitionWeight(from, to, weight)

#### Arguments

- `from`:

  Character identifier of the source hypothesis.

- `to`:

  Character identifier of the target hypothesis.

- `weight`:

  Numeric weight, fraction string, or symbolic expression.

------------------------------------------------------------------------

### `AlphaGraph$setEdgeColor()`

Set the presentation color of an existing edge.

#### Usage

    AlphaGraph$setEdgeColor(edge = NULL, color = NULL, from = NULL, to = NULL)

#### Arguments

- `edge`:

  Optional character edge identifier; otherwise use from and to.

- `color`:

  Optional visNetwork color specification.

- `from`:

  Character identifier of the source hypothesis.

- `to`:

  Character identifier of the target hypothesis.

------------------------------------------------------------------------

### `AlphaGraph$setEdgeFontSize()`

Set the edge label font size.

#### Usage

    AlphaGraph$setEdgeFontSize(edge = NULL, size, from = NULL, to = NULL)

#### Arguments

- `edge`:

  Optional character edge identifier; otherwise use from and to.

- `size`:

  Numeric font size in pixels, or NULL to clear the stored size.

- `from`:

  Character identifier of the source hypothesis.

- `to`:

  Character identifier of the target hypothesis.

------------------------------------------------------------------------

### `AlphaGraph$setEdgeSmooth()`

Set the edge smoothing specification.

#### Usage

    AlphaGraph$setEdgeSmooth(edge = NULL, smooth, from = NULL, to = NULL)

#### Arguments

- `edge`:

  Optional character edge identifier; otherwise use from and to.

- `smooth`:

  Logical or list specifying visNetwork edge smoothing.

- `from`:

  Character identifier of the source hypothesis.

- `to`:

  Character identifier of the target hypothesis.

------------------------------------------------------------------------

### `AlphaGraph$setEdgeVis()`

Merge named presentation properties from ... into the edge vis list.

#### Usage

    AlphaGraph$setEdgeVis(edge = NULL, from = NULL, to = NULL, ...)

#### Arguments

- `edge`:

  Optional character edge identifier; otherwise use from and to.

- `from`:

  Character identifier of the source hypothesis.

- `to`:

  Character identifier of the target hypothesis.

- `...`:

  Named arguments passed to the operation described above.

------------------------------------------------------------------------

### `AlphaGraph$setVisOptions()`

Merge named options from ... into the graph network settings.

#### Usage

    AlphaGraph$setVisOptions(...)

#### Arguments

- `...`:

  Named arguments passed to the operation described above.

------------------------------------------------------------------------

### `AlphaGraph$numberFormat()`

Return the current number-format settings as a named list.

#### Usage

    AlphaGraph$numberFormat()

------------------------------------------------------------------------

### `AlphaGraph$setNumberFormat()`

Update non-NULL number-format settings and regenerate numeric display
labels.

#### Usage

    AlphaGraph$setNumberFormat(
      fractions = NULL,
      maxDenominator = NULL,
      tolerance = NULL,
      digits = NULL
    )

#### Arguments

- `fractions`:

  Optional logical enabling fraction display; NULL keeps the current
  setting.

- `maxDenominator`:

  Optional positive integer limiting displayed fraction denominators.

- `tolerance`:

  Numeric tolerance for the checks or approximation performed by this
  method.

- `digits`:

  Optional positive integer giving significant digits for decimal
  display.

------------------------------------------------------------------------

### `AlphaGraph$parseNumber()`

Parse one finite decimal number or fraction string.

#### Usage

    AlphaGraph$parseNumber(x, name = "value")

#### Arguments

- `x`:

  Numeric scalar or fraction string to parse or format.

- `name`:

  Character name used in validation errors; defaults to "value".

------------------------------------------------------------------------

### `AlphaGraph$formatNumber()`

Format a number using the graph number-format settings.

#### Usage

    AlphaGraph$formatNumber(x)

#### Arguments

- `x`:

  Numeric scalar or fraction string to parse or format.

------------------------------------------------------------------------

### `AlphaGraph$loadList()`

Replace the graph from a canonical list and normalize stored settings.

#### Usage

    AlphaGraph$loadList(x, validate = TRUE)

#### Arguments

- `x`:

  Canonical JSON-compatible list.

- `validate`:

  Logical; whether to validate the imported, returned, or modified
  object.

------------------------------------------------------------------------

### `AlphaGraph$updateFromTransitionMatrix()`

Update transitions while preserving canonical hypothesis order.

#### Usage

    AlphaGraph$updateFromTransitionMatrix(
      transitions,
      type = c("expression", "display", "value"),
      nodeKeys = c("id", "label"),
      removeZero = TRUE,
      validate = TRUE
    )

#### Arguments

- `transitions`:

  Square transition matrix or data frame in hypothesis order.

- `type`:

  Character choice of "value", "expression", or "display"
  representation.

- `nodeKeys`:

  Character choice of "id" or "label" used to match matrix dimnames.

- `removeZero`:

  Logical; whether zero cells remove existing edges. Defaults to TRUE.

- `validate`:

  Logical; whether to validate the imported, returned, or modified
  object.

------------------------------------------------------------------------

### `AlphaGraph$updateFromVisNetworkData()`

Merge network data into the canonical graph using stable identifiers.

#### Usage

    AlphaGraph$updateFromVisNetworkData(
      nodes,
      edges,
      removeMissing = TRUE,
      updateCanonicalLabels = FALSE,
      validate = TRUE
    )

#### Arguments

- `nodes`:

  visNetwork node data frame containing stable node identifiers.

- `edges`:

  visNetwork edge data frame containing from and to identifiers.

- `removeMissing`:

  Logical; whether nodes and edges absent from the input are removed.

- `updateCanonicalLabels`:

  Logical; whether network labels also replace canonical labels.
  Defaults to FALSE.

- `validate`:

  Logical; whether to validate the imported, returned, or modified
  object.

------------------------------------------------------------------------

### `AlphaGraph$fromJson()`

Load a canonical graph using jsonlite::fromJSON().

#### Usage

    AlphaGraph$fromJson(json, validate = TRUE)

#### Arguments

- `json`:

  JSON text, connection, or file path accepted by jsonlite::fromJSON().

- `validate`:

  Logical; whether to validate the imported, returned, or modified
  object.

------------------------------------------------------------------------

### `AlphaGraph$toList()`

Return the JSON-compatible canonical graph list.

#### Usage

    AlphaGraph$toList()

------------------------------------------------------------------------

### `AlphaGraph$toJson()`

Serialize the canonical graph with jsonlite::toJSON(); ... supplies
additional JSON options.

#### Usage

    AlphaGraph$toJson(pretty = TRUE, auto_unbox = TRUE, null = "null", ...)

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

### `AlphaGraph$toJSON()`

Compatibility alias forwarding ... to toJson().

#### Usage

    AlphaGraph$toJSON(...)

#### Arguments

- `...`:

  Named arguments passed to the operation described above.

------------------------------------------------------------------------

### `AlphaGraph$toJason()`

Compatibility alias forwarding ... to toJson().

#### Usage

    AlphaGraph$toJason(...)

#### Arguments

- `...`:

  Named arguments passed to the operation described above.

------------------------------------------------------------------------

### `AlphaGraph$toTransitionMatrix()`

Return an n by n transition matrix or data frame in canonical node
order.

#### Usage

    AlphaGraph$toTransitionMatrix(
      type = c("display", "value", "expression"),
      parameters = list(),
      asDataFrame = TRUE,
      rowNames = c("label", "id"),
      zero = NULL,
      validate = FALSE
    )

#### Arguments

- `type`:

  Character choice of "value", "expression", or "display"
  representation.

- `parameters`:

  Named list of numeric parameter overrides for expression evaluation.

- `asDataFrame`:

  Logical; return a data frame when TRUE, a matrix otherwise.

- `rowNames`:

  Character choice of "label" or "id" for row and column names.

- `zero`:

  Optional fill value for missing transitions; defaults to numeric or
  character zero.

- `validate`:

  Logical; whether to validate the imported, returned, or modified
  object.

------------------------------------------------------------------------

### `AlphaGraph$toHypotheses()`

Return a weight vector in canonical node order, named by stable
hypothesis IDs.

#### Usage

    AlphaGraph$toHypotheses(
      type = c("value", "display", "expression"),
      parameters = list(),
      validate = FALSE
    )

#### Arguments

- `type`:

  Character choice of "value", "expression", or "display"
  representation.

- `parameters`:

  Named list of numeric parameter overrides for expression evaluation.

- `validate`:

  Logical; whether to validate the imported, returned, or modified
  object.

------------------------------------------------------------------------

### `AlphaGraph$toGraphicalMCPInput()`

Return a list of hypotheses, transitions, and hyp_names with matching
names and dimensions.

#### Usage

    AlphaGraph$toGraphicalMCPInput(
      parameters = list(),
      validate = TRUE,
      hypNames = c("id", "label")
    )

#### Arguments

- `parameters`:

  Named list of numeric parameter overrides for expression evaluation.

- `validate`:

  Logical; whether to validate the imported, returned, or modified
  object.

- `hypNames`:

  Character choice of "id" or "label" for unique engine hypothesis
  names.

------------------------------------------------------------------------

### `AlphaGraph$toGraphicalMCP()`

Create a graphicalMCP initial_graph from numeric canonical inputs.

#### Usage

    AlphaGraph$toGraphicalMCP(
      parameters = list(),
      validate = TRUE,
      hypNames = c("id", "label")
    )

#### Arguments

- `parameters`:

  Named list of numeric parameter overrides for expression evaluation.

- `validate`:

  Logical; whether to validate the imported, returned, or modified
  object.

- `hypNames`:

  Character choice of "id" or "label" for unique engine hypothesis
  names.

------------------------------------------------------------------------

### `AlphaGraph$toVisNetworkData()`

Return a list containing nodes and edges data frames for visNetwork.

#### Usage

    AlphaGraph$toVisNetworkData()

------------------------------------------------------------------------

### `AlphaGraph$toVisNetwork()`

Return a visNetwork htmlwidget using the stored graph presentation.

#### Usage

    AlphaGraph$toVisNetwork(width = NULL, height = NULL, applyDefaults = TRUE)

#### Arguments

- `width`:

  Optional widget width; falls back to graph settings and then "100%".

- `height`:

  Optional widget height; falls back to graph settings and then "400px".

- `applyDefaults`:

  Logical; whether to apply saved network settings. Defaults to TRUE.

------------------------------------------------------------------------

### `AlphaGraph$validate()`

Validate graph structure and optionally numeric weights and transitions;
return TRUE on success.

#### Usage

    AlphaGraph$validate(
      requireNumeric = FALSE,
      parameters = list(),
      tolerance = sqrt(.Machine$double.eps)
    )

#### Arguments

- `requireNumeric`:

  Logical; whether weights and transitions must be numerically
  computable.

- `parameters`:

  Named list of numeric parameter overrides for expression evaluation.

- `tolerance`:

  Numeric validation tolerance; defaults to sqrt(.Machine\$double.eps).

------------------------------------------------------------------------

### `AlphaGraph$clone()`

The objects of this class are cloneable with this method.

#### Usage

    AlphaGraph$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
graph <- createDemoAlphaGraph()
graph$validate(requireNumeric = TRUE)
#> [1] TRUE
graph$toTransitionMatrix(type = "value", rowNames = "id")
#>       H1_NI H1_S H2_NI H2_S
#> H1_NI     0  0.5   0.5  0.0
#> H1_S      0  0.0   0.5  0.5
#> H2_NI     0  0.5   0.0  0.5
#> H2_S      0  0.0   0.0  0.0
```

# alphagraph graph format, version 1

The canonical graph is a plain R list and is JSON-safe with
`jsonlite::toJSON(auto_unbox = TRUE, null = "null")`. It is the single source
of truth for statistical graph data, transition-matrix data, and visNetwork
presentation data.

Top-level fields:

- `format` and `version` identify the contract (`alphagraph.graph`, version 1).
- `id`, `label`, `description`, and `metadata` describe the graph.
- `parameters` defines named values used by symbolic expressions.
- `nodes` defines hypotheses, initial weights, and node presentation.
- `edges` defines directed transitions and edge presentation.
- `settings` contains graph-wide visNetwork and editor options.

Every weight or transition uses a three-part value specification:

- `display`: text shown in the app, for example `1-ν`.
- `expression`: evaluable R expression using parameter ids, for example
  `1 - nu`.
- `value`: optional resolved numeric value. It is recomputed from `expression`
  when the expression changes.

Node and edge `vis` objects accept visNetwork properties as nested lists. They
are flattened to data-frame columns such as `color.background` and `font.size`
when passed to visNetwork. Unknown presentation properties are preserved,
which keeps the format extensible without coupling the statistical model to a
specific set of visual options.

Main conversion API:

- `AlphaGraph$toList()` / `alphaGraphFromList()`
- `AlphaGraph$toJson()` / `alphaGraphFromJson()`
- `AlphaGraph$toTransitionMatrix()` /
  `alphaGraphFromTransitionMatrix()` / `updateFromTransitionMatrix()`
- `AlphaGraph$toGraphicalMCP()` / `alphaGraphFromGraphicalMCP()`
- `AlphaGraph$toVisNetworkData()` / `alphaGraphFromVisNetworkData()` /
  `updateFromVisNetworkData()`

Matrix edits should call `updateFromTransitionMatrix()` on the current graph.
This adds, changes, or removes canonical edges while retaining the visual
properties of unchanged edges. Network edits should call
`updateFromVisNetworkData()` on that same graph. Views can then be regenerated
from the updated canonical object.

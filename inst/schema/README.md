# alphagraph persistence formats, version 1

alphagraph uses two composable JSON contracts:

- `alphagraph-graph-v1.schema.json` describes a reusable graphical multiple
  comparison procedure.
- `alphagraph-project-v1.schema.json` embeds that graph and adds user-entered
  datasets, analysis specifications, optional derived results, UI state, and
  project metadata.

Raw p-values and calculation results are intentionally not graph properties.
The same graph can therefore be reused with multiple datasets and analyses.
The hypotheses table displayed by the application is a joined view of graph,
dataset, and result data.

## Graph format

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

For hypothesis nodes, `label` stores the editable canonical text. One
underscore introduces a Unicode subscript when rendered: `H1_ni` renders as
H1 with subscript ni, while `H1_(ni)` includes subscript parentheses. The
derived representation is stored in `vis.label`, followed by the displayed
hypothesis weight on a second line. Only Unicode-supported subscript
characters are accepted.

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

JSON Schema validates the serial form. `AlphaGraph$validate()` additionally
enforces cross-record and statistical invariants which JSON Schema cannot
express conveniently, including unique ids, valid edge references, probability
bounds, row sums, and a zero transition diagonal. Draft graphs may contain
unresolved expressions; calculations require
`validate(requireNumeric = TRUE)`.

## Project format

Top-level project fields:

- `graph` is a complete `alphagraph.graph` object referenced by the project
  schema rather than duplicated in it.
- `datasets` stores named p-value records keyed by stable `hypothesisId`.
- `analyses` stores the method, overall alpha, dataset reference, test groups,
  and calculation options.
- `results` stores optional derived summaries with an input hash, timestamp,
  and the alphagraph and graphicalMCP versions used for the calculation.
- `ui` and `metadata` store project-wide presentation and descriptive data.

`AlphaProject$validate()` checks references across these collections. Before an
analysis runs, computable validation also requires exactly one p-value for every
graph node and a fully numeric graph. Correlation matrices for parametric test
groups must have matching dimensions, be symmetric with diagonal 1, and be
positive semidefinite.

Changing a dataset or analysis removes its saved result. Changing the graph
makes an existing result non-current because its input hash no longer matches.

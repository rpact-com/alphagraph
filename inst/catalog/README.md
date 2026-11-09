# Graph description catalog

`graph-description-catalog.json` contains curated text for known example
graphs. A catalog entry is matched to an `AlphaGraph` by its stable `id`.

Each entry may define:

- `summary`: introductory Markdown text;
- `hypotheses`: hypothesis `id` and its clinical/statistical `meaning`;
- `context`: a list of contextual statements;
- `interpretation`: a list of interpretation notes;
- `literature`: citations, optionally with a URL.

All fields except the graph `id` are optional. Information stored directly in
the graph, such as node meanings or graph metadata, is also included. A node's
own `metadata.meaning` or `metadata.description` takes precedence over its
catalog meaning.

The corresponding JSON Schema is
`../schema/graph-description-catalog-v1.schema.json`.

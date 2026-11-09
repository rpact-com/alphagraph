# Generate a Markdown description template for an AlphaGraph

Builds an editable description from the canonical graph and enriches
known example graphs with curated context from a JSON catalog.

## Usage

``` r
getGraphDescription(graph = createDemoAlphaGraph(), catalogPath = NULL)
```

## Arguments

- graph:

  An `AlphaGraph` instance.

- catalogPath:

  Optional path to the curated graph-description JSON.

## Value

Character scalar containing a Markdown description template.

# Create a demo visNetwork graph for the UI

Build and return a visNetwork object illustrating a small example
transition graph. The function is intended for use in the UI module as a
demonstration / initial view.

## Usage

``` r
getDemoGraph(ns, graph = createDemoAlphaGraph())
```

## Arguments

- ns:

  Shiny namespace function (usually obtained via session\$ns).

- graph:

  Canonical `AlphaGraph` to render.

## Value

A visNetwork object ready to be rendered.

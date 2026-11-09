# Description panel server module

Server-side logic for the description editor and preview panel. Manages
the reactive state of the Markdown content and toggling between edit and
preview modes.

## Usage

``` r
moduleDescriptionServer(id, graphStore = NULL, ..., args = list())
```

## Arguments

- id:

  Module id (namespace) for this server module.

- graphStore:

  Shared `AlphaGraphStore`. If omitted, the module creates a private
  store for backwards compatibility.

- ...:

  Additional parameters (reserved for future use).

- args:

  Named list of optional parameters. `catalogPath` can point to an
  alternative graph-description catalog.

## Value

Invisibly NULL; registers server observers and outputs.

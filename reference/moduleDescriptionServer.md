# Description panel server module

Server-side logic for the description editor and preview panel. Manages
the reactive state of the Markdown content and toggling between edit and
preview modes.

## Usage

``` r
moduleDescriptionServer(id, ..., args = list())
```

## Arguments

- id:

  Module id (namespace) for this server module.

- ...:

  Additional parameters (reserved for future use).

- args:

  Named list of optional parameters passed to the module.

## Value

Invisibly NULL; registers server observers and outputs.

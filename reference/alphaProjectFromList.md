# Create an AlphaProject from a JSON-compatible list

Restore a project from its canonical list, optionally validating its
contents.

## Usage

``` r
alphaProjectFromList(x, validate = TRUE)
```

## Arguments

- x:

  Canonical JSON-compatible project list.

- validate:

  Logical; whether to validate the restored project.

## Value

An `AlphaProject` instance.

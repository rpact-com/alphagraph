# Create an AlphaProject from JSON

Read a serialized project with jsonlite and restore its canonical model.

## Usage

``` r
alphaProjectFromJson(json, validate = TRUE)
```

## Arguments

- json:

  JSON text, connection, or file path accepted by
  [`jsonlite::fromJSON()`](https://jeroen.r-universe.dev/jsonlite/reference/fromJSON.html).

- validate:

  Logical; whether to validate the restored project.

## Value

An `AlphaProject` instance.

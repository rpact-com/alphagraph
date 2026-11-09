# Create a formatted hypothesis label

Generate an editable hypothesis label using alphagraph's underscore
syntax. The canonical value is converted to supported Unicode subscripts
only when rendered by visNetwork.

## Usage

``` r
getHypothesisLabel(number, x, i = NULL)
```

## Arguments

- number:

  Integer indicating hypothesis index (0/1/2 etc.).

- x:

  Character short code describing the hypothesis (e.g. "ni", "s").

- i:

  Optional additional index or annotation appended on a new line.

## Value

Character scalar such as `H1_(ni)`.

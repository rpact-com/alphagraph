# Parse and format AlphaGraph numeric values

Strictly parse decimal and fraction input while keeping canonical graph
values numeric, and format numeric values as fractions when the
configured complexity limit permits it.

## Usage

``` r
alphaGraphParseNumber(x, name = "value")

alphaGraphTryParseNumber(x)

alphaGraphFormatNumber(
  x,
  fractions = TRUE,
  maxDenominator = 16L,
  tolerance = 1e-08,
  digits = 8L
)

alphaGraphFormatMatrix(matrix, settings = list())

alphaGraphParseMatrix(x)
```

## Arguments

- x:

  A scalar number, fraction string, or matrix-like object.

- name:

  Name used in validation messages.

- fractions:

  Whether simple fractions should be displayed.

- maxDenominator:

  Largest denominator allowed in a displayed fraction.

- tolerance:

  Maximum relative approximation error for fraction display.

- digits:

  Number of significant digits used for decimal display.

- matrix:

  A numeric matrix.

- settings:

  Number-format settings from an `AlphaGraph`.

## Value

A numeric value or matrix when parsing, and character output when
formatting.

# Create a formatted hypothesis label

Generate a unicode-formatted label for hypotheses used in the graph
visualization. The function converts simple tokens (e.g. "n", "s",
digits) into appropriate unicode subscripts and returns a string
suitable for node labels.

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

Character scalar with the formatted hypothesis label.

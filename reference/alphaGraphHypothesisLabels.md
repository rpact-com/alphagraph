# Calculate the effective length of a hypothesis label

Underscores do not count. Characters following an underscore count at
half length, rounded up, because they are rendered as subscripts.

Converts the part after a single underscore to Unicode subscripts
supported by visNetwork. For example, `H1_ni` becomes `H1` followed by
subscript `ni`, and `H1_(ni)` also renders the parentheses as
subscripts.

## Usage

``` r
alphaGraphHypothesisLabelLength(label)

alphaGraphValidateHypothesisLabelLengths(
  labels,
  maxLength = .alphaGraphMaxHypothesisLabelLength()
)

alphaGraphRenderHypothesisLabel(label)

alphaGraphValidateHypothesisLabels(labels)
```

## Arguments

- label:

  Non-empty character scalar containing the editable label.

- labels:

  Character vector of canonical hypothesis labels.

- maxLength:

  Positive integer giving the maximum effective label length.

## Value

Numeric scalar with the effective label length.

`alphaGraphValidateHypothesisLabelLengths()` returns the calculated
effective lengths invisibly.

Character scalar suitable for display in visNetwork.

`alphaGraphValidateHypothesisLabels()` returns the rendered labels
invisibly after validating syntax and uniqueness.

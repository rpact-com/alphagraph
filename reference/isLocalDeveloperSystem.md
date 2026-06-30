# Check whether current system belongs to a local developer

Return TRUE when the current user account matches one of the known local
developer accounts. This is used to enable developer-only logging and
diagnostics. The check is resilient and returns FALSE on error.

## Usage

``` r
isLocalDeveloperSystem(developers = c("fried"))
```

## Arguments

- developers:

  Character vector of developer user names to accept.

## Value

Logical scalar: TRUE if running on a local developer system, FALSE
otherwise.

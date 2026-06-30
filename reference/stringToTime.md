# Coerce a string to POSIXlt time

Attempt to parse a variety of commonly used time/date string formats
into a POSIXlt object. On failure NA is returned and an error is logged.

## Usage

``` r
stringToTime(s)
```

## Arguments

- s:

  Character scalar with the time representation.

## Value

POSIXlt object on success or NA on failure.

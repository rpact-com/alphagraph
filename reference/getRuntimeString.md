# Format runtime between two times

Compute and format the elapsed time between start and end times. Inputs
can be POSIX objects or character strings parsable by stringToTime().

## Usage

``` r
getRuntimeString(
  startTime,
  ...,
  endTime = Sys.time(),
  runtimeUnits = c("secs", "auto"),
  addBrackets = FALSE
)
```

## Arguments

- startTime:

  Start time (POSIXct/POSIXlt or character).

- ...:

  Additional arguments (currently unused).

- endTime:

  End time (defaults to now).

- runtimeUnits:

  Character option: "secs" or "auto".

- addBrackets:

  Logical; if TRUE result is wrapped in brackets.

## Value

Formatted runtime string or NA_character\_.

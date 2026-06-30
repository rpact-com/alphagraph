# Format multiple runtimes as strings

Given vectors of start (and optionally end) times, return a character
vector with human-friendly runtime descriptions.

## Usage

``` r
getRuntimeStrings(
  startTimes,
  ...,
  endTimes = rep(Sys.time(), length(startTimes)),
  runtimeUnits = c("secs", "auto"),
  addBrackets = FALSE
)
```

## Arguments

- startTimes:

  Vector of start times (POSIXct/POSIXlt or character).

- ...:

  Additional arguments passed to getRuntimeString.

- endTimes:

  Vector of end times; recycled to match startTimes if needed.

- runtimeUnits:

  Character, either "secs" or "auto"; passed to formatter.

- addBrackets:

  Logical; if TRUE the returned strings are wrapped in brackets.

## Value

Character vector with formatted runtime strings.

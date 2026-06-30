# Log a timed message with elapsed time

Log a message and append the elapsed time computed between startTime and
endTime. The elapsed time is formatted and appended to the message.

## Usage

``` r
logTime(
  s,
  ...,
  startTime,
  endTime = Sys.time(),
  runtimeUnits = c("auto", "secs")
)
```

## Arguments

- s:

  Message text to log (character).

- ...:

  Additional values concatenated to the message.

- startTime:

  Start time for timing (POSIX or character).

- endTime:

  End time (defaults to Sys.time()).

- runtimeUnits:

  Units formatting preference passed to getRuntimeString.

## Value

Invisibly NULL.

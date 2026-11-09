# Check whether a log level is enabled

Determine whether messages with the specified log level should be
emitted based on the current application minimum log level stored in the
RPACT_CLOUD_LOG_LEVEL environment variable. The comparison is performed
case-insensitively. The special level "disabled" always returns FALSE
and the alias "time" is treated as "info".

## Usage

``` r
isLoggingEnabled(logLevel)
```

## Arguments

- logLevel:

  Character scalar indicating the log level to test. Supported values
  include "trace", "debug", "info", "warning", "error", "time" and
  "disabled".

## Value

Logical scalar: TRUE if messages at the given level should be logged,
FALSE otherwise.

## Examples

``` r
if (FALSE) { # \dontrun{
setAppLogLevel("warn")
isLoggingEnabled("info")
isLoggingEnabled("error")
} # }
```

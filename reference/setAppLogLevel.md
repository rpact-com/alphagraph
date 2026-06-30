# Set the application log level

Set the minimum log level for the application by modifying the
RPACT_CLOUD_LOG_LEVEL environment variable. Accepted levels are "trace",
"debug", "info", "warn", "error", and "disabled".

## Usage

``` r
setAppLogLevel(logLevel)
```

## Arguments

- logLevel:

  Character string specifying the minimum log level.

## Value

Invisibly NULL.

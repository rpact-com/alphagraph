# Produce lines describing the current session environment

Collect session information (R version, attached packages, loaded
namespaces, environment variables etc.) and return it as a character
vector suitable for saving or display.

## Usage

``` r
getSessionInfoLogLines(logInstalledPackages = FALSE)
```

## Arguments

- logInstalledPackages:

  Logical; if TRUE also list all installed packages.

## Value

Character vector with lines describing the session information.

# Detect whether the app is running as an installed package

Attempt to determine whether the app code is running from a package
installation or from a source (project) directory. This helps to adapt
behavior (e.g. resource lookup) depending on packaging mode.

## Usage

``` r
isAppRunningAsAPackage(loggingEnabled = TRUE)
```

## Arguments

- loggingEnabled:

  Logical; if TRUE, logging is enabled for error messages.

## Value

Logical scalar indicating whether the app is running as a package.

# Resolve a path to a system resource file

Return the correct file path for resources bundled with the package when
the app is installed, or the corresponding 'inst/' path when running
from the project source tree.

## Usage

``` r
getSystemFile(path)
```

## Arguments

- path:

  Relative path inside the package resources (e.g. "www/foo.css").

## Value

A string with the absolute file path to the requested resource.

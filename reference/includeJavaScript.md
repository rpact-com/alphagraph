# Include JavaScript in Shiny UI

Helper that creates a script tag for including a JavaScript file in the
Shiny UI. The path is passed through to the tag; when packaging the app
callers typically use getSystemFile() to resolve resource paths.

## Usage

``` r
includeJavaScript(path)
```

## Arguments

- path:

  Path or URL to the JavaScript file to include.

## Value

A Shiny tag containing a script include.

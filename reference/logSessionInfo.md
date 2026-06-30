# Print session information unless silenced

Wrapper that prints the output of getSessionInfoLogLines() unless the
'silent' argument is TRUE. By default silent is TRUE on developer
systems.

## Usage

``` r
logSessionInfo(silent = isLocalDeveloperSystem())
```

## Arguments

- silent:

  Logical; when TRUE nothing is printed.

## Value

Invisibly NULL.

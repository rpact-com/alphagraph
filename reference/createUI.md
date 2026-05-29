# Create the UI for the standalone Shiny app

This function defines the user interface for the standalone version of
the alphagraph Shiny app. It uses the bs4Dash package to create a
dashboard layout with a header, sidebar, and body. The sidebar contains
menu items for navigating between the "Graph Editor" and "Settings"
tabs. The body includes custom CSS for styling, as well as UI outputs
for a context menu and overlay that will be rendered dynamically when
the context menu is open. The "Graph Editor" tab contains a visNetwork
graph and additional panels for description and analysis, while the
"Settings" tab is currently a placeholder for future settings options.

## Usage

``` r
createUI(request)
```

## Arguments

- request:

  The Shiny request object (not used here but required for Shiny UI
  functions)

## Value

A Shiny UI definition for the alphagraph app

# Standalone Shiny server for the alphagraph app.

This function defines the server logic for the standalone version of the
alphagraph Shiny app. It initializes the server-side components for the
graph editor, transition matrix, and hypothesis panel modules.
Additionally, it renders a description of the graph and a placeholder
for analysis results. The description is loaded from a Markdown file,
while the analysis output is currently a static text that can be updated
with dynamic content in the future.

## Usage

``` r
createServer(input, output, session)
```

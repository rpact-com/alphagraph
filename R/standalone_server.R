##
## alphagraph
##
## This file is part of the R package alphagraph
##
## Author: Friedrich Pahlke, PhD
## Licensed under "GNU Lesser General Public License" version 3
## License text can be found here: https://www.r-project.org/Licenses/LGPL-3
##
## RPACT company website: https://www.rpact.com
##

#' 
#' @title 
#' Standalone Shiny server for the alphagraph app.
#' 
#' @param input The input object from the Shiny server function, containing all reactive inputs from the UI.
#' @param output The output object from the Shiny server function, used to render outputs back to the UI.
#' @param session The session object from the Shiny server function, providing information about the current 
#'        user session and allowing for session-specific operations.
#' 
#' @description
#' This function defines the server logic for the standalone version of the alphagraph Shiny app. 
#' It initializes the server-side components for the graph editor, transition matrix, and 
#' hypothesis panel modules. Additionally, it renders a description of the graph and a placeholder 
#' for analysis results. The description is loaded from a Markdown file, while the analysis output 
#' is currently a static text that can be updated with dynamic content in the future.
#' 
#' @export
#' 
createServer <- function(input, output, session) {
    graphStore <- createAlphaGraphStore(createDemoAlphaGraph())
    session$userData$alphaGraphStore <- graphStore

    moduleGraphServer("graphIO", graphStore = graphStore)
    moduleTransitionMatrixServer("transitionMatrix", graphStore = graphStore)
    moduleHypothesisServer("hypothesisPanel", graphStore = graphStore)
    moduleDescriptionServer("descriptionPanel")
    moduleAnalysisServer("analysisPanel")

    invisible(graphStore)
}

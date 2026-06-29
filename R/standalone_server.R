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

    moduleGraphServer("graphIO")
    moduleTransitionMatrixServer("transitionMatrix")
    moduleHypothesisServer("hypothesisPanel")
    moduleDescriptionServer("descriptionPanel")
    moduleAnalysisServer("analysisPanel")
    
}

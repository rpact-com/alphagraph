moduleAnalysisUI <- function(id) {
    ns <- shiny::NS(id)
    
    shiny::verbatimTextOutput(ns("analysis"))
}
moduleTransitionMatrixUI <- function(id) {
    ns <- shiny::NS(id)
    bs4Dash::box(
        width = 12,
        solidHeader = TRUE,
        title = "Transition Matrix",
        DT::DTOutput(ns("transitionMatrix"))
    )
}
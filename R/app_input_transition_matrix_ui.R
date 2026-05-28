moduleTransitionMatrixUI <- function(id) {
    ns <- NS(id)
    box(
        width = 12,
        solidHeader = TRUE,
        title = "Transition Matrix",
        DTOutput(ns("transitionMatrix"))
    )
}
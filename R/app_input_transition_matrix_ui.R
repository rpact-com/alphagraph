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

 #' @title Transition matrix UI module
 #'
 #' @description
 #' UI for displaying the transition matrix in a datatable. The module
 #' provides a boxed panel containing the rendered matrix output.
 #'
 #' @param id Module id (namespace) used for UI namespacing.
 #'
 #' @return UI elements for the transition matrix panel.
 #'
 #' @keywords internal
moduleTransitionMatrixUI <- function(id) {
    ns <- shiny::NS(id)
    bs4Dash::box(
        width = 12,
        solidHeader = TRUE,
        title = "Transition Matrix",
        shiny::tags$p(
            class = "text-muted small",
            "Cells are editable. Use numeric values or parameter expressions."
        ),
        DT::DTOutput(ns("transitionMatrix"))
    )
}

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

 #' @title Transition matrix server module
 #'
 #' @description
 #' Server-side logic for rendering the transition matrix datatable. The
 #' module obtains the example matrix from getTransitionMatrix() and renders
 #' it using DT.
 #'
 #' @param id Module id (namespace) for this server module.
 #' @param ... Additional parameters (reserved for future use).
 #' @param args Named list of optional parameters passed to the module.
 #'
 #' @return Invisibly NULL; registers server outputs and observers.
 #'
 #' @keywords internal
moduleTransitionMatrixServer <- function(id, ..., args = list()) {
    shiny::moduleServer(
        id,
        function(input, output, session) {
            ns <- session$ns

            logDebug("///--- moduleTransitionMatrixServer id = ", id, " (", session$ns("xxx"), ")")

            output$transitionMatrix <- DT::renderDT({
                mat <- getTransitionMatrix()
                DT::datatable(
                    mat,
                    options = list(dom = "t"),
                    class = "compact"
                )
            })
        }
    )
}

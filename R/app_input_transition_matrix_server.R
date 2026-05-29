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

moduleTransitionMatrixServer <- function(id, ..., args = list()) {
    moduleServer(
        id,
        function(input, output, session) {
            ns <- session$ns

            logDebug("///--- modulePlotServer id = ", id, " (", session$ns("xxx"), ")")

            output$transitionMatrix <- renderDT({
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

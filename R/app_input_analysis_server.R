moduleAnalysisServer <- function(id, ..., args = list()) {
    shiny::moduleServer(
        id,
        function(input, output, session) {
            ns <- session$ns

            logDebug("///--- moduleAnalysisServer id = ", id, " (", session$ns("xxx"), ")")

            output$analysis <- shiny::renderText({
                "Hypotheses, weights, and p-values go here."
            })
            
        }
    )
}

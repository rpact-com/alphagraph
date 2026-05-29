moduleHypothesisServer <- function(id, ..., args = list()) {
    moduleServer(
        id,
        function(input, output, session) {
            ns <- session$ns

            logDebug("///--- moduleHypothesisServer id = ", id, " (", session$ns("xxx"), ")")

            output$hypothesisPanel <- renderUI({
                hypotheses <- c("H1,NI", "H1,S", "H2,NI", "H2,S")
                weights <- c(1, 0, 0, 0)
                pvalues <- c(0.012, 0.001, 0.030, 0.162)

                tagList(
                    lapply(seq_along(hypotheses), function(i) {
                        fluidRow(
                            column(4, textInput(paste0("hyp", i), NULL, hypotheses[i])),
                            column(4, numericInput(paste0("w", i), NULL, weights[i], min = 0)),
                            column(4, numericInput(paste0("p", i), NULL, pvalues[i], min = 0, step = 0.001)),
                        )
                    })
                )
            })
        }
    )
}
moduleHypothesisServer <- function(id, ..., args = list()) {
    shiny::moduleServer(
        id,
        function(input, output, session) {
            ns <- session$ns

            logDebug("///--- moduleHypothesisServer id = ", id, " (", session$ns("xxx"), ")")

            output$hypothesisPanel <- shiny::renderUI({
                hypotheses <- c("H1,NI", "H1,S", "H2,NI", "H2,S")
                weights <- c(1, 0, 0, 0)
                pvalues <- c(0.012, 0.001, 0.030, 0.162)

                shiny::tagList(
                    lapply(seq_along(hypotheses), function(i) {
                        shiny::fluidRow(
                            bs4Dash::column(4, shiny::textInput(paste0("hyp", i), NULL, hypotheses[i])),
                            bs4Dash::column(4, shiny::numericInput(paste0("w", i), NULL, weights[i], min = 0)),
                            bs4Dash::column(4, shiny::numericInput(paste0("p", i), NULL, pvalues[i], min = 0, step = 0.001)),
                        )
                    })
                )
            })
        }
    )
}
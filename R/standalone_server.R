createServer <- function(input, output, session) {

    moduleGraphServer("graphIO")
    moduleTransitionMatrixServer("transitionMatrix")
    moduleHypothesisServer("hypothesisPanel")

    # Mock description + analysis texts
    output$desc <- renderUI({
        # "Graph representing the procedure from Hung and Wang (2010)."
        includeMarkdown(getGraphDescriptionFile())
    })
    output$analysis <- renderText({
        "Hypotheses, weights, and p-values go here."
    })

}

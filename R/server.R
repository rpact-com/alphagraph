createServer <- function(input, output, session) {
    # -------------------------------------------------------------
    # 1) Minimal visNetwork example (mockup for prototype)
    # -------------------------------------------------------------
    output$vis <- visNetwork::renderVisNetwork({
        getDemoGraph()
    })

    # -------------------------------------------------------------
    # 2) Transition Matrix (mockup)
    # -------------------------------------------------------------
    output$transitionMatrix <- renderDT({
        mat <- getTransitionMatrix()
        DT::datatable(
            mat,
            options = list(dom = "t"),
            class = "compact"
        )
    })

    # -------------------------------------------------------------
    # 3) Hypothesis Panel (mockup)
    # -------------------------------------------------------------
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

    # Mock description + analysis texts
    output$desc <- renderUI({
        # "Graph representing the procedure from Hung and Wang (2010)."
        includeMarkdown(getGraphDescriptionFile())
    })
    output$analysis <- renderText({
        "Hypotheses, weights, and p-values go here."
    })

    observeEvent(input$add_node, {
        visNetwork::visNetworkProxy("vis") |>
            visNetwork::visUpdateNodes(
                data.frame(
                    id = paste0("H_new_", sample(10000, 1)),
                    label = getHypothesisLabel(0, "n", 0),
                    x = 1,
                    y = 1,
                    color.background = "white",
                    color.border = "black",
                    shape = "circle",
                    borderWidth = 2,
                    value = 1, 
                    font = list(size = 20, align = "top"),
                    font.size = 20,
                    font.align = "top",
                    title = "New Node"
                )
            )
    })

    observeEvent(input$add_edge, {
#        visNetwork::visNetworkProxy("vis") |>
#            visNetwork::visConfigure(enabled = TRUE) |>
#            visNetwork::visOptions(manipulation = list(
#                enabled = TRUE,
#                addEdge = TRUE,
#                editEdge = FALSE,
#                addNode = FALSE,
#                deleteNode = FALSE
#            ))
    })

    observeEvent(input$edit_node, {
#        visNetwork::visNetworkProxy("vis") |>
#            visNetwork::visOptions(manipulation = list(
#                enabled = TRUE,
#                editNode = TRUE,
#                addNode = FALSE,
#                addEdge = FALSE
#            ))
    })

    observeEvent(input$delete, {
#        visNetwork::visNetworkProxy("vis") |>
#            visNetwork::visOptions(manipulation = list(
#                enabled = TRUE,
#                deleteNode = TRUE,
#                deleteEdge = TRUE
#            ))
    })

    observeEvent(input$reset_view, {
#        visNetwork::visNetworkProxy("vis") |> 
#            visNetwork::visFit()
    })

    observeEvent(input$zoom_in, {
        #visNetwork::visNetworkProxy("vis") |> visNetwork::visZoom(1.2)
    })

    observeEvent(input$zoom_out, {
        #visNetwork::visNetworkProxy("vis") |> visNetwork::visZoom(0.8)
    })

    observeEvent(input$lock_pos, {
#        visNetwork::visNetworkProxy("vis") |>
#            visNetwork::visPhysics(enabled = FALSE)
    })

    observeEvent(input$unlock_pos, {
#        visNetwork::visNetworkProxy("vis") |>
#            visNetwork::visPhysics(enabled = TRUE)
    })
}

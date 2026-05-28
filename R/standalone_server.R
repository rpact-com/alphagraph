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

    moduleTransitionMatrixServer("transitionMatrix")

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
                    x = "-40px",
                    y = "-5px",
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

#    observeEvent(input$node_right_click, {
#        node_id <- input$node_right_click$id
#        
#        showModal(modalDialog(
#                title = paste("Node properties:", node_id),
#                textInput("node_label", "Label", value = paste0("H", node_id)),
#                selectInput("node_color", "Color", choices = c("white","lightgreen","lightblue","orange")),
#                footer = tagList(
#                    modalButton("Close"),
#                    actionButton("save_node_props", "Save")
#                ),
#                easyClose = TRUE
#            ))
#    })


    rv <- reactiveValues(
        ctx_open = FALSE,
        ctx_node_id = NULL,
        ctx_x = 0,
        ctx_y = 0
    )

    # Open menu on right click
    observeEvent(input$node_right_click, {
        rv$ctx_open <- TRUE
        rv$ctx_node_id <- input$node_right_click$id
        rv$ctx_x <- input$node_right_click$x
        rv$ctx_y <- input$node_right_click$y
    })

    # Close menu (click away or empty-space right click)
    observeEvent(input$ctx_close, {
        rv$ctx_open <- FALSE
    })

    # Overlay to capture click-away
    output$ctx_overlay <- renderUI({
        req(rv$ctx_open)
        div(
            id = "ctx-overlay",
            onclick = "Shiny.setInputValue('ctx_close', Math.random(), {priority: 'event'});"
        )
    })

    # Context menu UI (Bootstrap dropdown menu rendered at cursor position)
    output$ctx_menu <- renderUI({
        shiny::req(rv$ctx_open)
        
        # Keep menu within viewport (simple clamp)
        x <- max(10, min(rv$ctx_x, session$clientData$output_vis_width %||% 1200))
        y <- max(10, min(rv$ctx_y, session$clientData$output_vis_height %||% 800))
        
        #message(sprintf("Opening context menu for node %s at (%s, %s)", rv$ctx_node_id, x, y))
        
        shiny::req(x, y)
        x <- round(x)
        y <- round(y)
        
        div(
            id = "ctx-menu",
            style = sprintf("left:%dpx; top:%dpx;", x, y),
            div(
                class = "dropdown-menu show",
                style = "position: static; display: block;",
                div(class = "dropdown-header", paste("Node", rv$ctx_node_id)),
                tags$a(class = "dropdown-item", href = "#", 
                    onclick = "Shiny.setInputValue('ctx_action', {action:'edit', nonce:Math.random()}, {priority:'event'});",
                    icon("edit"), "Edit properties"),
                tags$a(class = "dropdown-item", href = "#", 
                    onclick = "Shiny.setInputValue('ctx_action', {action:'green', nonce:Math.random()}, {priority:'event'});",
                    icon("paint-brush"), "Set color: green"),
                tags$a(class = "dropdown-item", href = "#", 
                    onclick = "Shiny.setInputValue('ctx_action', {action:'white', nonce:Math.random()}, {priority:'event'});",
                    icon("eraser"), "Set color: white"),
                div(class = "dropdown-divider"),
                tags$a(class = "dropdown-item text-danger", href = "#", 
                    onclick = "Shiny.setInputValue('ctx_action', {action:'delete', nonce:Math.random()}, {priority:'event'});",
                    icon("trash"), "Delete node")
            )
        )
    })

    # Handle menu actions
    observeEvent(input$ctx_action, {
        req(rv$ctx_open, rv$ctx_node_id)
        act <- input$ctx_action$action
        id <- rv$ctx_node_id
        
        if (act == "green") {
            visNetworkProxy("vis") %>%
                visUpdateNodes(data.frame(id = id, color.background = "#7CFF7C"))
        } else if (act == "white") {
            visNetworkProxy("vis") %>%
                visUpdateNodes(data.frame(id = id, color.background = "white"))
        } else if (act == "delete") {
            visNetworkProxy("vis") %>%
                visRemoveNodes(id = id)
        } else if (act == "edit") {
            # Example: show a bs4Dash/Shiny modal for editing
            showModal(modalDialog(
                title = paste("Edit Node", id),
                textInput("node_label", "Label", value = paste0("Node ", id)),
                footer = tagList(
                    modalButton("Cancel"),
                    actionButton("save_node_label", "Save")
                ),
                easyClose = TRUE
            ))
        }
        
        # Close menu after action
        rv$ctx_open <- FALSE
    })

    observeEvent(input$save_node_label, {
        req(rv$ctx_node_id)
        visNetworkProxy("vis") %>%
            visUpdateNodes(data.frame(id = rv$ctx_node_id, label = input$node_label))
        removeModal()
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

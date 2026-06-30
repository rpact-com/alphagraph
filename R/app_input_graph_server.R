 #' @title Graph editor server module
 #'
 #' @description
 #' Server-side logic for the graph editor module. Handles rendering the
 #' demonstration graph, managing node/edge actions and context menu events.
 #'
 #' @param id Module id (namespace) for this server module.
 #' @param ... Additional parameters (reserved for future use).
 #' @param args Named list of optional parameters passed to the module.
 #'
 #' @return Invisibly NULL; the function registers server observers and outputs.
 #'
 #' @noRd
moduleGraphServer <- function(id, ..., args = list()) {
    shiny::moduleServer(
        id,
        function(input, output, session) {
            ns <- session$ns

            logDebug("///--- moduleGraphServer id = ", id, " (", session$ns("xxx"), ")")

            output$graph <- visNetwork::renderVisNetwork({
                getDemoGraph(ns)
            })

            #    shiny::observeEvent(input$node_right_click, {
            #        node_id <- input$node_right_click$id
            #
            #        shiny::showModal(shiny::modalDialog(
            #                title = paste("Node properties:", node_id),
            #                shiny::textInput("node_label", "Label", value = paste0("H", node_id)),
            #                shiny::selectInput("node_color", "Color", choices = c("white","lightgreen","lightblue","orange")),
            #                footer = shiny::tagList(
            #                    shiny::modalButton("Close"),
            #                    shiny::actionButton("save_node_props", "Save")
            #                ),
            #                easyClose = TRUE
            #            ))
            #    })

            shiny::observeEvent(input$add_node, {
                visNetwork::visNetworkProxy(ns("graph")) |>
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

            shiny::observeEvent(input$add_edge, {
                #        visNetwork::visNetworkProxy(ns("graph")) |>
                #            visNetwork::visConfigure(enabled = TRUE) |>
                #            visNetwork::visOptions(manipulation = list(
                #                enabled = TRUE,
                #                addEdge = TRUE,
                #                editEdge = FALSE,
                #                addNode = FALSE,
                #                deleteNode = FALSE
                #            ))
            })

            shiny::observeEvent(input$edit_node, {
                #        visNetwork::visNetworkProxy(ns("graph")) |>
                #            visNetwork::visOptions(manipulation = list(
                #                enabled = TRUE,
                #                editNode = TRUE,
                #                addNode = FALSE,
                #                addEdge = FALSE
                #            ))
            })

            shiny::observeEvent(input$delete, {
                #        visNetwork::visNetworkProxy(ns("graph")) |>
                #            visNetwork::visOptions(manipulation = list(
                #                enabled = TRUE,
                #                deleteNode = TRUE,
                #                deleteEdge = TRUE
                #            ))
            })

            shiny::observeEvent(input$reset_view, {
                #        visNetwork::visNetworkProxy(ns("graph")) |>
                #            visNetwork::visFit()
            })

            shiny::observeEvent(input$zoom_in, {
                # visNetwork::visNetworkProxy(ns("graph")) |> visNetwork::visZoom(1.2)
            })

            shiny::observeEvent(input$zoom_out, {
                # visNetwork::visNetworkProxy(ns("graph")) |> visNetwork::visZoom(0.8)
            })

            shiny::observeEvent(input$lock_pos, {
                #        visNetwork::visNetworkProxy(ns("graph")) |>
                #            visNetwork::visPhysics(enabled = FALSE)
            })

            shiny::observeEvent(input$unlock_pos, {
                #        visNetwork::visNetworkProxy(ns("graph")) |>
                #            visNetwork::visPhysics(enabled = TRUE)
            })

            rv <- shiny::reactiveValues(
                ctx_open = FALSE,
                ctx_node_id = NULL,
                ctx_x = 0,
                ctx_y = 0
            )

            # Overlay to capture click-away
            output$ctx_overlay <- shiny::renderUI({
                shiny::req(rv$ctx_open)
                shiny::tags$div(
                    id = "ctx-overlay",
                    onclick = paste0("Shiny.setInputValue('", ns("ctx_close"), "', Math.random(), {priority: 'event'});")
                )
            })

            # Context menu UI (Bootstrap dropdown menu rendered at cursor position)
            output$ctx_menu <- shiny::renderUI({
                shiny::req(rv$ctx_open)

                # Keep menu within viewport (simple clamp)
                x <- max(10, min(rv$ctx_x, session$clientData$output_vis_width %||% 1200))
                y <- max(10, min(rv$ctx_y, session$clientData$output_vis_height %||% 800))

                # message(sprintf("Opening context menu for node %s at (%s, %s)", rv$ctx_node_id, x, y))

                shiny::req(x, y)
                x <- round(x)
                y <- round(y)

                shiny::tags$div(
                    id = "ctx-menu",
                    style = sprintf("left:%dpx; top:%dpx;", x, y),
                    shiny::tags$div(
                        class = "dropdown-menu show",
                        style = "position: static; display: block;",
                        shiny::tags$div(class = "dropdown-header", paste("Node", rv$ctx_node_id)),
                        shiny::tags$a(
                            class = "dropdown-item", href = "#",
                            onclick = paste0("Shiny.setInputValue('", ns("ctx_action"), "', {action:'edit', nonce:Math.random()}, {priority:'event'});"),
                            shiny::icon("edit"), "Edit properties"
                        ),
                        shiny::tags$a(
                            class = "dropdown-item", href = "#",
                            onclick = paste0("Shiny.setInputValue('", ns("ctx_action"), "', {action:'green', nonce:Math.random()}, {priority:'event'});"),
                            shiny::icon("paint-brush"), "Set color: green"
                        ),
                        shiny::tags$a(
                            class = "dropdown-item", href = "#",
                            onclick = paste0("Shiny.setInputValue('", ns("ctx_action"), "', {action:'white', nonce:Math.random()}, {priority:'event'});"),
                            shiny::icon("eraser"), "Set color: white"
                        ),
                        shiny::tags$div(class = "dropdown-divider"),
                        shiny::tags$a(
                            class = "dropdown-item text-danger", href = "#",
                            onclick = paste0("Shiny.setInputValue('", ns("ctx_action"), "', {action:'delete', nonce:Math.random()}, {priority:'event'});"),
                            shiny::icon("trash"), "Delete node"
                        )
                    )
                )
            })

            # Handle menu actions
            shiny::observeEvent(input$ctx_action, {
                shiny::req(rv$ctx_open, rv$ctx_node_id)
                act <- input$ctx_action$action
                id <- rv$ctx_node_id

                if (act == "green") {
                    visNetwork::visNetworkProxy(ns("graph")) |>
                        visNetwork::visUpdateNodes(data.frame(id = id, color.background = "#7CFF7C"))
                } else if (act == "white") {
                    visNetwork::visNetworkProxy(ns("graph")) |>
                        visNetwork::visUpdateNodes(data.frame(id = id, color.background = "white"))
                } else if (act == "delete") {
                    visNetwork::visNetworkProxy(ns("graph")) |>
                        visNetwork::visRemoveNodes(id = id)
                } else if (act == "edit") {
                    # Example: show a bs4Dash/Shiny modal for editing
                    shiny::showModal(shiny::modalDialog(
                        title = paste("Edit Node", id),
                        shiny::textInput("node_label", "Label", value = paste0("Node ", id)),
                        footer = shiny::tagList(
                            shiny::modalButton("Cancel"),
                            shiny::actionButton("save_node_label", "Save")
                        ),
                        easyClose = TRUE
                    ))
                }

                # Close menu after action
                rv$ctx_open <- FALSE
            })

            # Open menu on right click
            shiny::observeEvent(input$node_right_click, {
                rv$ctx_open <- TRUE
                rv$ctx_node_id <- input$node_right_click$id
                rv$ctx_x <- input$node_right_click$x
                rv$ctx_y <- input$node_right_click$y
            })

            # Close menu (click away or empty-space right click)
            shiny::observeEvent(input$ctx_close, {
                rv$ctx_open <- FALSE
            })

            shiny::observeEvent(input$save_node_label, {
                shiny::req(rv$ctx_node_id)
                visNetwork::visNetworkProxy(ns("graph")) |>
                    visNetwork::visUpdateNodes(data.frame(id = rv$ctx_node_id, label = input$node_label))
                shiny::removeModal()
            })
        }
    )
}

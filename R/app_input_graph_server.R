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

#' 
#' @title Graph editor server module
#'
#' @description
#' Server-side logic for the graph editor module. Handles rendering the
#' demonstration graph, managing node/edge actions and context menu events.
#'
#' @param id Module id (namespace) for this server module.
#' @param graphStore Shared `AlphaGraphStore`. A private store is created when
#'   omitted, which keeps the module usable in isolation.
#' @param ... Additional parameters (reserved for future use).
#' @param args Named list of optional parameters passed to the module.
#'
#' @return Invisibly NULL; the function registers server observers and outputs.
#'
#' @keywords internal
#' 
moduleGraphServer <- function(id, graphStore = NULL, ..., args = list()) {
    shiny::moduleServer(
        id,
        function(input, output, session) {
            ns <- session$ns

            if (is.null(graphStore)) {
                graphStore <- createAlphaGraphStore(createDemoAlphaGraph())
            }

            logDebug("///--- moduleGraphServer id = ", id, " (", session$ns("xxx"), ")")

            output$graph <- visNetwork::renderVisNetwork({
                graphStore$revision()
                getDemoGraph(ns, graphStore$graph())
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
                graph <- graphStore$snapshot()
                ids <- vapply(graph$toList()$nodes, function(node) node$id, character(1L))
                number <- 1L
                while (paste0("H_new_", number) %in% ids) {
                    number <- number + 1L
                }
                id <- paste0("H_new_", number)
                graphStore$transact("graph:add-node", function(candidate) {
                    candidate$addNode(
                        id = id,
                        label = id,
                        weight = 0,
                        x = 14,
                        y = -5,
                        vis = list(title = "New Node")
                    )
                })
            })

            shiny::observeEvent(input$add_edge, {
                nodes <- graphStore$snapshot()$toList()$nodes
                if (length(nodes) < 2L) {
                    shiny::showNotification(
                        "At least two nodes are required to add a transition.",
                        type = "warning"
                    )
                    return()
                }
                choices <- stats::setNames(
                    vapply(nodes, function(node) node$id, character(1L)),
                    vapply(nodes, function(node) node$label, character(1L))
                )
                shiny::showModal(shiny::modalDialog(
                    title = "Add transition",
                    shiny::selectInput(ns("edge_from"), "From", choices = choices),
                    shiny::selectInput(ns("edge_to"), "To", choices = choices,
                                       selected = choices[min(2L, length(choices))]),
                    shiny::textInput(ns("edge_transition"), "Weight for Edge", value = "1/2"),
                    footer = shiny::tagList(
                        shiny::modalButton("Cancel"),
                        shiny::actionButton(ns("save_edge"), "Add")
                    )
                ))
            })

            shiny::observeEvent(input$save_edge, {
                tryCatch({
                    shiny::req(input$edge_from, input$edge_to, input$edge_transition)
                    graphStore$transact("graph:add-edge", function(candidate) {
                        candidate$setTransitionWeight(
                            from = input$edge_from,
                            to = input$edge_to,
                            weight = input$edge_transition
                        )
                    }, requireNumeric = TRUE)
                    shiny::removeModal()
                }, error = function(error) {
                    shiny::showNotification(conditionMessage(error), type = "error")
                })
            })

            shiny::observeEvent(input$edit_node, {
                shiny::req(length(rv$selected_nodes) == 1L)
                rv$ctx_node_id <- rv$selected_nodes[[1L]]
                showNodeEditor(rv$ctx_node_id)
            })

            shiny::observeEvent(input$edit_edge, {
                shiny::req(length(rv$selected_edges) == 1L)
                edgeId <- rv$selected_edges[[1L]]
                edges <- graphStore$snapshot()$toList()$edges
                edge <- Filter(function(x) identical(x$id, edgeId), edges)
                shiny::req(length(edge) == 1L)
                rv$ctx_edge_id <- edgeId
                shiny::showModal(shiny::modalDialog(
                    title = paste("Edit edge weight", edge[[1L]]$from, "->", edge[[1L]]$to),
                    shiny::textInput(
                        ns("edited_edge_transition"),
                        "Weight for Edge",
                        value = edge[[1L]]$transition$display
                    ),
                    footer = shiny::tagList(
                        shiny::modalButton("Cancel"),
                        shiny::actionButton(ns("save_edge_transition"), "Save")
                    )
                ))
            })

            shiny::observeEvent(input$delete, {
                nodes <- rv$selected_nodes
                edges <- rv$selected_edges
                shiny::req(length(nodes) + length(edges) > 0L)
                graphStore$transact("graph:delete-selection", function(candidate) {
                    for (edge in edges) {
                        if (edge %in% vapply(candidate$toList()$edges,
                                            function(x) x$id, character(1L))) {
                            candidate$removeEdge(edge = edge)
                        }
                    }
                    for (node in nodes) {
                        if (node %in% vapply(candidate$toList()$nodes,
                                            function(x) x$id, character(1L))) {
                            candidate$removeNode(node)
                        }
                    }
                })
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
                ctx_edge_id = NULL,
                ctx_x = 0,
                ctx_y = 0,
                selected_nodes = character(),
                selected_edges = character()
            )

            showNodeEditor <- function(id) {
                nodes <- graphStore$snapshot()$toList()$nodes
                node <- Filter(function(x) identical(x$id, id), nodes)
                shiny::req(length(node) == 1L)
                shiny::showModal(shiny::modalDialog(
                    title = paste("Edit Node", id),
                    shiny::textInput(
                        ns("node_label"),
                        "Label",
                        value = node[[1L]]$label
                    ),
                    footer = shiny::tagList(
                        shiny::modalButton("Cancel"),
                        shiny::actionButton(ns("save_node_label"), "Save")
                    ),
                    easyClose = TRUE
                ))
            }

            shiny::observeEvent(input$selection, {
                rv$selected_nodes <- unlist(input$selection$nodes %||% list(),
                                            use.names = FALSE)
                rv$selected_edges <- unlist(input$selection$edges %||% list(),
                                            use.names = FALSE)
            }, ignoreInit = TRUE)

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
                    graphStore$transact("graph:node-color", function(candidate) {
                        candidate$setNodeColor(id, background = "#7CFF7C")
                    })
                } else if (act == "white") {
                    graphStore$transact("graph:node-color", function(candidate) {
                        candidate$setNodeColor(id, background = "white")
                    })
                } else if (act == "delete") {
                    graphStore$transact("graph:delete-node", function(candidate) {
                        candidate$removeNode(id)
                    })
                } else if (act == "edit") {
                    showNodeEditor(id)
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
                graphStore$transact("graph:node-label", function(candidate) {
                    candidate$setNodeLabel(rv$ctx_node_id, input$node_label)
                })
                shiny::removeModal()
            })

            shiny::observeEvent(input$save_edge_transition, {
                shiny::req(rv$ctx_edge_id, input$edited_edge_transition)
                tryCatch({
                    graphStore$transact("graph:edge-transition", function(candidate) {
                        edges <- candidate$toList()$edges
                        edge <- Filter(
                            function(item) identical(item$id, rv$ctx_edge_id),
                            edges
                        )[[1L]]
                        candidate$setTransitionWeight(
                            from = edge$from,
                            to = edge$to,
                            weight = input$edited_edge_transition
                        )
                    }, requireNumeric = TRUE)
                    shiny::removeModal()
                }, error = function(error) {
                    shiny::showNotification(conditionMessage(error), type = "error")
                })
            })

            nodePositions <- shiny::debounce(
                shiny::reactive(input$node_positions),
                millis = args$positionDebounceMillis %||% 150L
            )

            shiny::observeEvent(nodePositions(), {
                positions <- nodePositions()$nodes
                if (is.null(positions) || length(positions) == 0L) {
                    return()
                }
                if (is.data.frame(positions)) {
                    positions <- lapply(seq_len(nrow(positions)), function(i) {
                        as.list(positions[i, , drop = FALSE])
                    })
                }
                graphStore$transact("graph:node-position", function(candidate) {
                    for (position in positions) {
                        candidate$setNodeXY(
                            id = as.character(position$id),
                            x = as.numeric(position$x),
                            y = as.numeric(position$y)
                        )
                    }
                })
            }, ignoreInit = TRUE)

            invisible(graphStore)
        }
    )
}

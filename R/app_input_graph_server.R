##
## alphagraph
##
## This file is part of the R package alphagraph
##
## Author: Friedrich Pahlke, PhD
## Licensed under "GNU Affero General Public License" version 3.0
## License text can be found here: https://cran.r-project.org/web/licenses/AGPL-3
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
#' @param args Named list of optional parameters passed to the module. Supported
#'   entries include `positionDebounceMillis` and `zoomFactor`; the latter must
#'   be greater than one and defaults to `1.2`.
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

            zoomFactor <- as.numeric(args$zoomFactor %||% 1.2)
            if (length(zoomFactor) != 1L || !is.finite(zoomFactor) || zoomFactor <= 1) {
                stop("args$zoomFactor must be one finite number greater than 1.",
                    call. = FALSE
                )
            }

            logDebug("///--- moduleGraphServer id = ", id, " (", session$ns("xxx"), ")")

            viewLocked <- shiny::reactiveVal(TRUE)
            gridVisible <- shiny::reactiveVal(TRUE)

            output$graph <- visNetwork::renderVisNetwork({
                graphStore$revision()
                widget <- getDemoGraph(ns, graphStore$graph())

                # Read the current state without making a button click rebuild the
                # widget. Graph-data rebuilds still retain the selected view state.
                widget <- visNetwork::visInteraction(
                    widget,
                    dragView = !shiny::isolate(viewLocked())
                )
                htmlwidgets::onRender(widget, "
                    function(el, x, data) {
                        if (window.alphaGraphInitializeHeight) {
                            window.alphaGraphInitializeHeight(el);
                        }
                        if (window.alphaGraphInitializeEdgeDrawing) {
                            window.alphaGraphInitializeEdgeDrawing(el, data.inputPrefix);
                        }
                        window.alphaGraphSetGridVisibility({
                            id: el.id, visible: data.gridVisible, buttonId: data.gridButtonId
                        });
                    }
                ", data = list(inputPrefix = ns(""),
                              gridVisible = shiny::isolate(gridVisible()),
                              gridButtonId = ns("toggle_grid")))
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
                while (paste0("H_", number) %in% ids) {
                    number <- number + 1L
                }
                id <- paste0("H_", number)
                labels <- vapply(
                    graph$toList()$nodes,
                    function(node) node$label,
                    character(1L)
                )
                labelNumber <- length(labels) + 1L
                while (paste0("H", labelNumber) %in% labels) {
                    labelNumber <- labelNumber + 1L
                }
                graphStore$transact("graph:add-node", function(candidate) {
                    candidate$addNode(
                        id = id,
                        label = paste0("H", labelNumber),
                        weight = 0,
                        x = 14,
                        y = -5,
                        vis = list(title = "New Node")
                    )
                })
            })

            showEdgeEditor <- function(from = NULL, to = NULL) {
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
                    vapply(nodes, function(node) {
                        alphaGraphRenderHypothesisLabel(node$label)
                    }, character(1L))
                )
                shiny::showModal(shiny::modalDialog(
                    title = "Add transition",
                    shiny::selectInput(ns("edge_from"), "From", choices = choices,
                                       selected = from %||% choices[[1L]]),
                    shiny::selectInput(ns("edge_to"), "To", choices = choices,
                                       selected = to %||% choices[min(2L, length(choices))]),
                    shiny::textInput(ns("edge_transition"), "Weight for Edge", value = "1/2"),
                    footer = shiny::tagList(
                        shiny::modalButton("Cancel"),
                        shiny::actionButton(ns("save_edge"), "Add")
                    )
                ))
            }

            shiny::observeEvent(input$add_edge, {
                showEdgeEditor()
            })

            shiny::observeEvent(input$edge_drawn, {
                from <- input$edge_drawn$from
                to <- input$edge_drawn$to
                data <- graphStore$snapshot()$toList()
                ids <- vapply(data$nodes, function(node) node$id, character(1L))
                shiny::req(is.character(from), length(from) == 1L, from %in% ids,
                           is.character(to), length(to) == 1L, to %in% ids,
                           !identical(from, to))
                if (any(vapply(data$edges, function(edge) {
                    identical(edge$from, from) && identical(edge$to, to)
                }, logical(1L)))) {
                    shiny::showNotification(
                        "A transition from this node to the target already exists. Use Edit edge weight to change it.",
                        type = "warning"
                    )
                    return()
                }
                showEdgeEditor(from, to)
            })

            shiny::observeEvent(input$save_edge, {
                tryCatch({
                    shiny::req(input$edge_from, input$edge_to, input$edge_transition)
                    graphStore$transact("graph:add-edge", function(candidate) {
                        if (identical(input$edge_from, input$edge_to)) {
                            stop("A transition must connect two different nodes.", call. = FALSE)
                        }
                        if (any(vapply(candidate$toList()$edges, function(edge) {
                            identical(edge$from, input$edge_from) &&
                                identical(edge$to, input$edge_to)
                        }, logical(1L)))) {
                            stop("This transition already exists. Use Edit edge weight to change it.",
                                 call. = FALSE)
                        }
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
                visNetwork::visNetworkProxy(ns("graph"), session = session) |>
                    visNetwork::visFit(
                        animation = list(
                            duration = 250,
                            easingFunction = "easeInOutQuad"
                        )
                    )
            })

            shiny::observeEvent(input$zoom_in, {
                session$sendCustomMessage(
                    "alphaGraphZoom",
                    list(id = ns("graph"), factor = zoomFactor)
                )
            })

            shiny::observeEvent(input$zoom_out, {
                session$sendCustomMessage(
                    "alphaGraphZoom",
                    list(id = ns("graph"), factor = 1 / zoomFactor)
                )
            })

            shiny::observeEvent(input$toggle_grid, {
                visible <- !gridVisible()
                gridVisible(visible)
                session$sendCustomMessage("alphaGraphGridVisibility", list(
                    id = ns("graph"), visible = visible, buttonId = ns("toggle_grid")
                ))
            })

            shiny::observeEvent(input$lock_pos, {
                locked <- !viewLocked()
                viewLocked(locked)

                shiny::updateActionButton(
                    session,
                    "lock_pos",
                    icon = shiny::icon(if (locked) "lock" else "unlock")
                )

                # dragView controls panning the canvas. dragNodes deliberately
                # remains enabled so users can still reposition individual nodes.
                visNetwork::visNetworkProxy(ns("graph"), session = session) |>
                    visNetwork::visInteraction(dragView = !locked)
            })

            rv <- shiny::reactiveValues(
                ctx_open = FALSE,
                ctx_node_id = NULL,
                edited_node_id = NULL,
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
                rv$edited_node_id <- id
                shiny::showModal(shiny::modalDialog(
                    title = paste("Edit Node", id),
                    shiny::fluidRow(
                        shiny::column(6,
                            shiny::textInput(
                                ns("node_label"),
                                "Label",
                                value = node[[1L]]$label,
                                width = "100%"
                            )
                        ),
                        shiny::column(6,
                            shiny::tags$div(
                                class = "form-group",
                                shiny::tags$label("Rendered label"),
                                shiny::uiOutput(
                                    ns("node_label_preview"),
                                    container = function(...) shiny::tags$div(
                                        ...,
                                        class = "form-control-static",
                                        style = "white-space: pre-wrap; overflow-wrap: anywhere;",
                                        `aria-live` = "polite"
                                    )
                                )
                            )
                        )
                    ),
                    shiny::helpText(
                        "Use H1_ni or H1_(ni) to render a Unicode subscript. Only supported subscript characters are accepted."
                    ),
                    shinyWidgets::colorPickr(
                        inputId = ns("node_color"),
                        label = "Color",
                        selected = node[[1L]]$vis$color$background %||% "#FFFFFF",
                        theme = "nano",
                        inline = TRUE,
                        swatches = c("#FFFFFF", "#7CFF7C"),
                        opacity = FALSE,
                        update = "change",
                        interaction = list(clear = FALSE, save = FALSE, rgba = FALSE),
                        width = "260px"
                    ),
                    footer = shiny::tagList(
                        shiny::tags$button(
                            type = "button",
                            class = "btn btn-danger",
                            style = "float: left; margin-right: auto;",
                            onclick = paste0(
                                "if (window.confirm('Delete this node and all connected edges?')) {",
                                "Shiny.setInputValue('", ns("delete_edited_node"),
                                "', Math.random(), {priority: 'event'});}"
                            ),
                            shiny::icon("trash"), "Delete"
                        ),
                        shiny::modalButton("Cancel"),
                        shiny::actionButton(ns("save_node_label"), "Save")
                    ),
                    easyClose = TRUE
                ))
            }

            shiny::observeEvent(input$delete_edited_node, {
                shiny::req(rv$edited_node_id)
                id <- rv$edited_node_id
                tryCatch({
                    graphStore$transact("graph:delete-node", function(candidate) {
                        candidate$removeNode(id)
                    })
                    rv$edited_node_id <- NULL
                    rv$ctx_node_id <- NULL
                    shiny::removeModal()
                }, error = function(error) {
                    shiny::showNotification(conditionMessage(error), type = "error")
                })
            })

            output$node_label_preview <- shiny::renderUI({
                shiny::req(!is.null(input$node_label))
                # renderText() uses cat(), which replaces Unicode characters
                # with <U+....> placeholders in non-UTF-8 locales on Windows.
                label <- tryCatch(
                    alphaGraphRenderHypothesisLabel(trimws(input$node_label)),
                    error = function(error) {
                        shiny::validate(shiny::need(FALSE, conditionMessage(error)))
                    }
                )
                shiny::tags$span(label)
            })

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
                shiny::req(rv$ctx_node_id, input$node_color)
                tryCatch({
                    label <- trimws(as.character(input$node_label))
                    color <- as.character(input$node_color)
                    if (length(color) != 1L || is.na(color) ||
                            !grepl("^#[0-9A-Fa-f]{6}$", color)) {
                        stop("Please select a valid node color.", call. = FALSE)
                    }
                    graphStore$transact("graph:node-label", function(candidate) {
                        nodes <- candidate$toList()$nodes
                        ids <- vapply(nodes, function(node) node$id, character(1L))
                        labels <- vapply(nodes, function(node) node$label, character(1L))
                        labels[match(rv$ctx_node_id, ids)] <- label
                        alphaGraphValidateHypothesisLabels(labels)
                        candidate$setNodeLabel(rv$ctx_node_id, label)
                        candidate$setNodeColor(rv$ctx_node_id, background = color)
                    })
                    shiny::removeModal()
                }, error = function(error) {
                    shiny::showNotification(conditionMessage(error), type = "error")
                })
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

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

 #' @title Hypotheses server module
 #'
 #' @description
 #' Server-side logic for the hypotheses editor. Hypothesis labels and weights
 #' are read from and written to the shared canonical graph store. P-values
 #' remain local analysis inputs.
 #'
 #' @param id Module id (namespace) for this server module.
 #' @param graphStore Shared `AlphaGraphStore`. A private store is created when
 #'   omitted.
 #' @param ... Additional parameters (reserved for future use).
 #' @param args Named list of optional parameters passed to the module.
 #'
 #' @return Invisibly NULL; registers server outputs and observers.
 #'
 #' @keywords internal
moduleHypothesisServer <- function(id, graphStore = NULL, ..., args = list()) {
    shiny::moduleServer(
        id,
        function(input, output, session) {
            ns <- session$ns

            if (is.null(graphStore)) {
                graphStore <- createAlphaGraphStore(createDemoAlphaGraph())
            }

            logDebug("///--- moduleHypothesisServer id = ", id, " (", session$ns("xxx"), ")")

            pValues <- shiny::reactiveValues(
                values = c(
                    H1_NI = 0.012,
                    H1_S = 0.001,
                    H2_NI = 0.030,
                    H2_S = 0.162
                )
            )

            editEvent <- function(nodeId, field, valueExpression = "this.value") {
                inputId <- jsonlite::toJSON(ns("hypothesis_edit"), auto_unbox = TRUE)
                nodeId <- jsonlite::toJSON(nodeId, auto_unbox = TRUE)
                field <- jsonlite::toJSON(field, auto_unbox = TRUE)
                paste0(
                    "Shiny.setInputValue(", inputId, ", {",
                    "id:", nodeId, ",field:", field, ",value:", valueExpression, ",",
                    "nonce:Math.random()},{priority:'event'});"
                )
            }

            pValueEvent <- function(nodeId) {
                inputId <- jsonlite::toJSON(ns("pvalue_edit"), auto_unbox = TRUE)
                nodeId <- jsonlite::toJSON(nodeId, auto_unbox = TRUE)
                paste0(
                    "Shiny.setInputValue(", inputId, ", {",
                    "id:", nodeId, ",value:this.value,nonce:Math.random()",
                    "},{priority:'event'});"
                )
            }

            output$hypothesisPanel <- shiny::renderUI({
                graphStore$revision()
                nodes <- graphStore$graph()$toList()$nodes

                shiny::tagList(
                    lapply(seq_along(nodes), function(i) {
                        node <- nodes[[i]]
                        nodeId <- node$id
                        weight <- node$weight$value
                        if (is.null(weight)) {
                            weight <- suppressWarnings(as.numeric(node$weight$expression))
                        }
                        pValue <- if (nodeId %in% names(pValues$values)) {
                            pValues$values[[nodeId]]
                        } else {
                            0
                        }
                        shiny::fluidRow(
                            bs4Dash::column(
                                4,
                                shiny::tags$input(
                                    id = ns(paste0("hypothesis_", i)),
                                    type = "text",
                                    class = "form-control",
                                    value = node$label,
                                    `aria-label` = paste("Hypothesis label", nodeId),
                                    onchange = editEvent(nodeId, "label")
                                )
                            ),
                            bs4Dash::column(
                                4,
                                shiny::tags$input(
                                    id = ns(paste0("weight_", i)),
                                    type = "number",
                                    class = "form-control",
                                    value = weight,
                                    min = 0,
                                    max = 1,
                                    step = "any",
                                    `aria-label` = paste("Hypothesis weight", nodeId),
                                    onchange = editEvent(nodeId, "weight")
                                )
                            ),
                            bs4Dash::column(
                                4,
                                shiny::tags$input(
                                    id = ns(paste0("pvalue_", i)),
                                    type = "number",
                                    class = "form-control",
                                    value = pValue,
                                    min = 0,
                                    max = 1,
                                    step = 0.001,
                                    `aria-label` = paste("P-value", nodeId),
                                    onchange = pValueEvent(nodeId)
                                )
                            )
                        )
                    })
                )
            })

            shiny::observeEvent(input$hypothesis_edit, {
                edit <- input$hypothesis_edit
                tryCatch({
                    shiny::req(edit$id, edit$field)
                    if (identical(edit$field, "label")) {
                        label <- trimws(as.character(edit$value))
                        if (!nzchar(label)) {
                            stop("Hypothesis labels must not be empty.", call. = FALSE)
                        }
                        graphStore$transact("hypotheses:label", function(graph) {
                            labels <- vapply(
                                graph$toList()$nodes,
                                function(node) node$label,
                                character(1L)
                            )
                            current <- match(edit$id, vapply(
                                graph$toList()$nodes,
                                function(node) node$id,
                                character(1L)
                            ))
                            if (is.na(current)) {
                                stop("Unknown hypothesis '", edit$id, "'.", call. = FALSE)
                            }
                            if (label %in% labels[-current]) {
                                stop("Hypothesis labels must be unique.", call. = FALSE)
                            }
                            graph$setNodeLabel(edit$id, label, updateVisLabel = TRUE)
                        })
                    } else if (identical(edit$field, "weight")) {
                        weight <- suppressWarnings(as.numeric(edit$value))
                        if (length(weight) != 1L || is.na(weight) ||
                            weight < 0 || weight > 1) {
                            stop("Hypothesis weights must be between 0 and 1.",
                                 call. = FALSE)
                        }
                        graphStore$transact("hypotheses:weight", function(graph) {
                            graph$setNodeWeight(
                                edit$id,
                                weight = weight,
                                updateVisLabel = TRUE
                            )
                            weights <- graph$toHypotheses(type = "value")
                            if (sum(weights) > 1 + sqrt(.Machine$double.eps)) {
                                stop("Hypothesis weights must sum to at most 1.",
                                     call. = FALSE)
                            }
                        })
                    } else {
                        stop("Unknown hypothesis field.", call. = FALSE)
                    }
                }, error = function(error) {
                    shiny::showNotification(conditionMessage(error), type = "error")
                })
            }, ignoreInit = TRUE)

            shiny::observeEvent(input$pvalue_edit, {
                edit <- input$pvalue_edit
                value <- suppressWarnings(as.numeric(edit$value))
                if (length(value) == 1L && !is.na(value) &&
                    value >= 0 && value <= 1) {
                    values <- shiny::isolate(pValues$values)
                    values[[as.character(edit$id)]] <- value
                    pValues$values <- values
                } else {
                    shiny::showNotification(
                        "P-values must be between 0 and 1.",
                        type = "error"
                    )
                }
            }, ignoreInit = TRUE)

            invisible(graphStore)
        }
    )
}

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

#' @title Hypotheses server module
#'
#' @description
#' Server-side logic for the hypotheses table. Editable labels and weights are
#' synchronized with the shared graph store. Rendered labels are read-only
#' previews, while p-values remain local analysis inputs.
#'
#' @param id Module id (namespace) for this server module.
#' @param graphStore Shared `AlphaGraphStore`. A private store is created when
#'   omitted.
#' @param projectStore Optional shared `AlphaProjectStore`. When supplied,
#'   p-values are read from and written to its selected dataset instead of
#'   being kept as module-local state.
#' @param datasetId Project dataset used for p-values.
#' @param ... Additional parameters (reserved for future use).
#' @param args Named list of optional parameters passed to the module. The
#'   `maxHypothesisLabelLength` entry overrides the application-wide option
#'   `alphagraph.maxHypothesisLabelLength` for this module.
#'
#' @return Invisibly NULL; registers server outputs and observers.
#'
#' @keywords internal
moduleHypothesisServer <- function(
        id,
        graphStore = NULL,
        projectStore = NULL,
        datasetId = "primary",
        ...,
        args = list()) {
    shiny::moduleServer(
        id,
        function(input, output, session) {
            if (is.null(graphStore)) {
                graphStore <- if (inherits(projectStore, "AlphaProjectStore")) {
                    projectStore$graphStore
                } else {
                    createAlphaGraphStore(createDemoAlphaGraph())
                }
            }
            if (!is.null(projectStore) && !inherits(projectStore, "AlphaProjectStore")) {
                stop("projectStore must be an AlphaProjectStore object.", call. = FALSE)
            }

            maxHypothesisLabelLength <- args$maxHypothesisLabelLength %||%
                .alphaGraphMaxHypothesisLabelLength()

            logDebug("///--- moduleHypothesisServer id = ", id, " (", session$ns("xxx"), ")")

            pValues <- shiny::reactiveValues(
                values = c(
                    H1_NI = 0.012,
                    H1_S = 0.001,
                    H2_NI = 0.030,
                    H2_S = 0.162
                )
            )
            tableRefresh <- shiny::reactiveVal(0L)

            output$hypothesisTable <- rhandsontable::renderRHandsontable({
                graphStore$revision()
                tableRefresh()
                graph <- graphStore$graph()
                currentPValues <- if (is.null(projectStore)) {
                    pValues$values
                } else {
                    projectStore$pValues(datasetId)
                }
                table <- .hypothesisTableData(graph, currentPValues)
                widget <- rhandsontable::rhandsontable(
                    table,
                    rowHeaders = NULL,
                    useTypes = TRUE,
                    stretchH = "all"
                )
                widget <- htmlwidgets::onRender(widget, "
                    function(el) {
                        if (window.alphaGraphTableRendered) {
                            window.alphaGraphTableRendered(el);
                        }
                    }
                ")
                widget <- rhandsontable::hot_col(
                    widget,
                    col = "ID",
                    readOnly = TRUE
                )
                rhandsontable::hot_col(
                    widget,
                    col = "Rendered label",
                    readOnly = TRUE
                )
            })

            shiny::observe({
                tryCatch({
                    edited <- rhandsontable::hot_to_r(input$hypothesisTable)
                    updatedPValues <- .updateGraphFromHypothesisTable(
                        graphStore = graphStore,
                        table = edited,
                        pValues = shiny::isolate(pValues$values),
                        maxHypothesisLabelLength = maxHypothesisLabelLength
                    )
                    if (is.null(projectStore)) {
                        pValues$values <- updatedPValues
                    } else {
                        projectStore$setPValues(
                            updatedPValues,
                            datasetId = datasetId,
                            source = "hypotheses:p-values"
                        )
                    }
                }, error = function(error) {
                    shiny::showNotification(conditionMessage(error), type = "error")
                    tableRefresh(shiny::isolate(tableRefresh()) + 1L)
                })
            }, label = "hypotheses-apply-table-edit") |>
                shiny::bindEvent(
                    input$hypothesisTable,
                    ignoreInit = TRUE,
                    ignoreNULL = TRUE,
                    label = "hypotheses-apply-table-edit"
                )

            invisible(graphStore)
        }
    )
}

.hypothesisTableData <- function(graph, pValues = numeric()) {
    nodes <- graph$toList()$nodes
    ids <- vapply(nodes, function(node) node$id, character(1L))
    labels <- vapply(nodes, function(node) node$label, character(1L))
    renderedLabels <- vapply(
        labels,
        alphaGraphRenderHypothesisLabel,
        character(1L)
    )
    weights <- vapply(nodes, function(node) {
        node$weight$display %||% graph$formatNumber(node$weight$value)
    }, character(1L))
    currentPValues <- pValues[ids]
    currentPValues[is.na(currentPValues)] <- 0

    data.frame(
        ID = ids,
        Hypothesis = labels,
        `Rendered label` = renderedLabels,
        Weight = weights,
        `p-Value` = as.character(currentPValues),
        check.names = FALSE,
        stringsAsFactors = FALSE
    )
}

.updateGraphFromHypothesisTable <- function(
        graphStore,
        table,
        pValues,
        maxHypothesisLabelLength = .alphaGraphMaxHypothesisLabelLength(),
        source = "hypotheses:table-edit") {
    required <- c("ID", "Hypothesis", "Rendered label", "Weight", "p-Value")
    if (!is.data.frame(table) || !all(required %in% names(table))) {
        stop("The hypothesis table has an unexpected structure.", call. = FALSE)
    }

    graph <- graphStore$snapshot()
    nodes <- graph$toList()$nodes
    currentIds <- vapply(nodes, function(node) node$id, character(1L))
    ids <- trimws(as.character(table$ID))
    if (!identical(ids, currentIds)) {
        stop("Hypothesis IDs and row order must not be changed.", call. = FALSE)
    }

    labels <- trimws(as.character(table$Hypothesis))
    alphaGraphValidateHypothesisLabels(labels)
    alphaGraphValidateHypothesisLabelLengths(
        labels,
        maxLength = maxHypothesisLabelLength
    )
    weights <- vapply(table$Weight, function(value) {
        graph$parseNumber(value, "Hypothesis weight")
    }, numeric(1L))
    editedPValues <- vapply(table[["p-Value"]], function(value) {
        graph$parseNumber(value, "p-value")
    }, numeric(1L))
    if (any(editedPValues < 0 | editedPValues > 1)) {
        stop("P-values must be between 0 and 1.", call. = FALSE)
    }

    currentLabels <- vapply(nodes, function(node) node$label, character(1L))
    currentWeights <- graph$toHypotheses(type = "value")
    graphChanged <- !identical(labels, currentLabels) ||
        !isTRUE(all.equal(unname(weights), unname(currentWeights),
                         tolerance = 1e-12))

    if (graphChanged) {
        graphStore$transact(source, function(candidate) {
            for (i in seq_along(ids)) {
                candidate$setNodeLabel(ids[[i]], labels[[i]])
                candidate$setNodeWeight(ids[[i]], weight = weights[[i]])
            }
        }, requireNumeric = TRUE)
    }

    updatedPValues <- pValues
    missingIds <- setdiff(ids, names(updatedPValues))
    if (length(missingIds)) {
        updatedPValues[missingIds] <- 0
    }
    updatedPValues[ids] <- editedPValues
    updatedPValues
}

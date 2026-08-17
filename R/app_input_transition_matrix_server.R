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

 #' @title Transition matrix server module
 #'
 #' @description
 #' Server-side logic for an editable transition matrix backed by the shared
 #' canonical graph store.
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
moduleTransitionMatrixServer <- function(id, graphStore = NULL, ..., args = list()) {
    shiny::moduleServer(
        id,
        function(input, output, session) {
            ns <- session$ns

            if (is.null(graphStore)) {
                graphStore <- createAlphaGraphStore(createDemoAlphaGraph())
            }

            logDebug("///--- moduleTransitionMatrixServer id = ", id, " (", session$ns("xxx"), ")")

            tableRefresh <- shiny::reactiveVal(0L)

            output$transitionMatrix <- rhandsontable::renderRHandsontable({
                graphStore$revision()
                tableRefresh()
                graph <- graphStore$graph()
                mat <- graph$toTransitionMatrix(
                    type = "value",
                    asDataFrame = FALSE,
                    rowNames = "id"
                )
                display <- alphaGraphFormatMatrix(mat, graph$numberFormat())
                nodeLabels <- vapply(
                    graph$toList()$nodes,
                    function(node) alphaGraphRenderHypothesisLabel(node$label),
                    character(1L)
                )
                widget <- rhandsontable::rhandsontable(
                    as.data.frame(display, check.names = FALSE,
                                  stringsAsFactors = FALSE),
                    rowHeaders = nodeLabels,
                    colHeaders = nodeLabels,
                    useTypes = FALSE,
                    stretchH = "all"
                )
                for (i in seq_len(nrow(display))) {
                    widget <- rhandsontable::hot_cell(
                        widget,
                        row = i,
                        col = i,
                        readOnly = TRUE
                    )
                }
                widget
            })

            shiny::observeEvent(input$transitionMatrix, {
                tryCatch({
                    edited <- rhandsontable::hot_to_r(input$transitionMatrix)
                    .updateGraphFromTransitionTable(graphStore, edited)
                }, error = function(error) {
                    shiny::showNotification(conditionMessage(error), type = "error")
                    tableRefresh(shiny::isolate(tableRefresh()) + 1L)
                })
            }, ignoreInit = TRUE)

            invisible(graphStore)
        }
    )
}

.updateGraphFromTransitionTable <- function(
        graphStore,
        table,
        source = "matrix:table-edit") {
    graph <- graphStore$snapshot()
    edited <- alphaGraphParseMatrix(table)
    current <- graph$toTransitionMatrix(
        type = "value",
        asDataFrame = FALSE,
        rowNames = "id"
    )
    if (!identical(dim(edited), dim(current))) {
        stop("Transition matrix dimensions changed unexpectedly.", call. = FALSE)
    }
    dimnames(edited) <- dimnames(current)
    if (isTRUE(all.equal(edited, current, tolerance = 1e-12,
                         check.attributes = TRUE))) {
        return(invisible(FALSE))
    }
    graphStore$transact(source, function(candidate) {
        candidate$updateFromTransitionMatrix(
            edited,
            type = "value",
            nodeKeys = "id",
            validate = TRUE
        )
    }, requireNumeric = TRUE)
}

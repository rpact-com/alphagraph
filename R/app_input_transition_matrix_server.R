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

            output$transitionMatrix <- DT::renderDT({
                graphStore$revision()
                graph <- graphStore$graph()
                mat <- graph$toTransitionMatrix(
                    type = "expression",
                    asDataFrame = TRUE,
                    rowNames = "id"
                )
                nodeLabels <- vapply(
                    graph$toList()$nodes,
                    function(node) node$label,
                    character(1L)
                )
                DT::datatable(
                    mat,
                    editable = list(
                        target = "cell",
                        disable = list(columns = 0L)
                    ),
                    options = list(
                        dom = "t",
                        ordering = FALSE,
                        paging = FALSE
                    ),
                    class = "compact cell-border",
                    rownames = nodeLabels,
                    colnames = nodeLabels
                )
            })

            shiny::observeEvent(input$transitionMatrix_cell_edit, {
                info <- input$transitionMatrix_cell_edit
                tryCatch({
                    graph <- graphStore$snapshot()
                    matrixData <- graph$toTransitionMatrix(
                        type = "expression",
                        asDataFrame = TRUE,
                        rowNames = "id"
                    )
                    edited <- DT::editData(
                        matrixData,
                        info,
                        rownames = TRUE
                    )
                    graphStore$transact("matrix:cell-edit", function(candidate) {
                        candidate$updateFromTransitionMatrix(
                            edited,
                            type = "expression",
                            nodeKeys = "id",
                            validate = TRUE
                        )
                    })
                }, error = function(error) {
                    shiny::showNotification(conditionMessage(error), type = "error")
                    graphStore$revision()
                    proxy <- DT::dataTableProxy("transitionMatrix", session = session)
                    DT::replaceData(
                        proxy,
                        graphStore$snapshot()$toTransitionMatrix(
                            type = "expression",
                            asDataFrame = TRUE,
                            rowNames = "id"
                        ),
                        rownames = TRUE,
                        resetPaging = FALSE
                    )
                })
            }, ignoreInit = TRUE)

            invisible(graphStore)
        }
    )
}

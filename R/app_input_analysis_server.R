##
## alphagraph
##
## Shortcut analysis module backed by the shared project model.
##

#' @title Analysis panel server module
#'
#' @description
#' Runs the first end-to-end graphicalMCP calculation. The graph comes from the
#' project graph, raw p-values from a project dataset, and the derived result is
#' stored separately with an input fingerprint and engine versions.
#'
#' @param id Module id.
#' @param projectStore Shared `AlphaProjectStore`.
#' @param analysisId Shortcut analysis specification to update and run.
#' @param datasetId Dataset containing raw p-values.
#' @param ... Additional parameters reserved for future use.
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

#' @param args Optional module parameters.
#'
#' @return Invisibly, the project store.
#' @keywords internal
moduleAnalysisServer <- function(
        id,
        projectStore = NULL,
        analysisId = "shortcut-primary",
        datasetId = "primary",
        ...,
        args = list()) {
    shiny::moduleServer(id, function(input, output, session) {
        if (is.null(projectStore)) {
            projectStore <- createAlphaProjectStore(createDemoAlphaProject())
        }
        if (!inherits(projectStore, "AlphaProjectStore")) {
            stop("projectStore must be an AlphaProjectStore object.", call. = FALSE)
        }

        logDebug("///--- moduleAnalysisServer id = ", id, " (", session$ns("xxx"), ")")

        output$analysisStatus <- shiny::renderUI({
            result <- projectStore$result(analysisId)
            
            statusText <- if (is.null(result)) {
                "No current result. Run the shortcut analysis after entering one p-value per hypothesis."
            } else {
                paste0(
                    "Current result computed ", result$createdAt,
                    " with graphicalMCP ", result$engine$graphicalMCPVersion %||% "unknown", "."
                )
            }
            shiny::tags$p(
                class = "text-muted small",
                statusText
            )
        })

        output$analysisTable <- rhandsontable::renderRHandsontable({
            table <- .analysisTableData(
                projectStore$project(validate = FALSE), analysisId
            )
            widget <- rhandsontable::rhandsontable(
                table,
                rowHeaders = NULL,
                readOnly = TRUE,
                useTypes = TRUE,
                contextMenu = FALSE,
                stretchH = "all",
                colWidths = c(60, 100, 75, 95, 120, 85)
            )
            htmlwidgets::onRender(widget, "
                function(el) {
                    if (window.alphaGraphTableRendered) {
                        window.alphaGraphTableRendered(el);
                    }
                }
            ")
        })

        shiny::observeEvent(input$runShortcut, {
            tryCatch({
                projectStore$upsertAnalysis(
                    id = analysisId,
                    datasetId = datasetId,
                    method = "shortcut",
                    alpha = input$alpha,
                    verbose = FALSE,
                    testValues = FALSE,
                    label = "Shortcut analysis",
                    source = "analysis:configuration"
                )
                projectStore$runShortcut(
                    analysisId,
                    source = "analysis:shortcut"
                )
                shiny::showNotification(
                    "Shortcut analysis completed.",
                    type = "message"
                )
            }, error = function(error) {
                shiny::showNotification(conditionMessage(error), type = "error")
            })
        }, ignoreInit = TRUE)

        invisible(projectStore)
    })
}

.analysisTableData <- function(project, analysisId = "shortcut-primary") {
    if (!inherits(project, "AlphaProject")) {
        stop("project must be an AlphaProject object.", call. = FALSE)
    }
    graph <- project$graph()
    nodes <- graph$toList()$nodes
    ids <- vapply(nodes, function(node) node$id, character(1L))
    labels <- vapply(nodes, function(node) node$label, character(1L))
    weights <- graph$toHypotheses(type = "value")

    analysis <- tryCatch(project$analysis(analysisId), error = function(error) NULL)
    rawP <- stats::setNames(rep(NA_real_, length(ids)), ids)
    if (!is.null(analysis)) {
        rawP[ids] <- project$pValues(analysis$datasetId)[ids]
    }

    adjustedP <- stats::setNames(rep(NA_real_, length(ids)), ids)
    rejected <- stats::setNames(rep(NA, length(ids)), ids)
    result <- if (is.null(analysis)) NULL else project$result(analysisId)
    if (!is.null(result) && isTRUE(project$resultIsCurrent(analysisId))) {
        adjusted <- .projectResultValues(result$adjustedP)
        decisions <- .projectResultValues(result$rejected)
        adjustedP[ids] <- adjusted[ids]
        rejected[ids] <- decisions[ids]
    }

    data.frame(
        ID = ids,
        Hypothesis = labels,
        Weight = unname(weights[ids]),
        `Raw p-value` = unname(rawP[ids]),
        `Adjusted p-value` = unname(adjustedP[ids]),
        Rejected = unname(rejected[ids]),
        check.names = FALSE,
        stringsAsFactors = FALSE
    )
}

.projectResultValues <- function(records) {
    values <- lapply(records, function(record) record$value)
    ids <- vapply(records, function(record) record$hypothesisId, character(1L))
    values <- unlist(values, use.names = FALSE)
    names(values) <- ids
    values
}

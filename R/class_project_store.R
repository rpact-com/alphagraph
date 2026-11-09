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

#' @title Reactive store for an AlphaProject
#'
#' @description
#' Owns project datasets, analysis specifications, and results while exposing
#' the embedded graph through the existing `AlphaGraphStore`. A project
#' snapshot always embeds the current graph-store snapshot.
#'
#' @keywords internal
AlphaProjectStore <- R6::R6Class(
    "AlphaProjectStore",
    public = list(
        graphStore = NULL,

        initialize = function(project = createDemoAlphaProject()) {
            if (!inherits(project, "AlphaProject")) {
                stop("project must be an AlphaProject object.", call. = FALSE)
            }
            project$validate()
            self$graphStore <- createAlphaGraphStore(project$graph())
            private$state <- shiny::reactiveValues(
                project = alphaProjectFromList(project$toList()),
                revision = 0L,
                source = "initial",
                error = NULL,
                updating = FALSE
            )
            invisible(self)
        },

        revision = function() {
            graphRevision <- self$graphStore$revision()
            projectRevision <- private$state$revision
            projectRevision + graphRevision
        },

        source = function() private$state$source,
        error = function() private$state$error,

        snapshot = function(validate = FALSE) {
            projectList <- shiny::isolate(private$state$project$toList())
            projectList$graph <- self$graphStore$snapshot()$toList()
            alphaProjectFromList(projectList, validate = validate)
        },

        project = function(validate = FALSE) {
            self$revision()
            projectList <- private$state$project$toList()
            projectList$graph <- self$graphStore$graph()$toList()
            alphaProjectFromList(projectList, validate = validate)
        },

        pValues = function(datasetId = "primary") {
            self$revision()
            private$state$project$pValues(datasetId)
        },

        setPValues = function(
                pValues,
                datasetId = "primary",
                label = datasetId,
                source = "project:p-values") {
            self$transact(source, function(project) {
                existing <- tryCatch(project$dataset(datasetId), error = function(error) NULL)
                project$upsertDataset(
                    id = datasetId,
                    pValues = pValues,
                    label = existing$label %||% label,
                    description = existing$description %||% NULL,
                    metadata = existing$metadata %||% list()
                )
            })
        },

        upsertAnalysis = function(..., source = "project:analysis") {
            arguments <- list(...)
            self$transact(source, function(project) {
                do.call(project$upsertAnalysis, arguments)
            })
        },

        result = function(analysisId) {
            self$revision()
            project <- self$project(validate = FALSE)
            result <- project$result(analysisId)
            if (is.null(result) || !isTRUE(project$resultIsCurrent(analysisId))) {
                return(NULL)
            }
            result
        },

        runShortcut = function(analysisId, source = "project:shortcut") {
            result <- NULL
            self$transact(source, function(project) {
                result <<- project$runShortcut(analysisId)
            }, validate = TRUE)
            invisible(result)
        },

        replace = function(project, source = "external") {
            if (!inherits(project, "AlphaProject")) {
                stop("project must be an AlphaProject object.", call. = FALSE)
            }
            project$validate()
            self$graphStore$replace(project$graph(), source = source)
            private$state$project <- alphaProjectFromList(project$toList())
            private$state$source <- source
            private$state$error <- NULL
            private$state$revision <- shiny::isolate(private$state$revision) + 1L
            invisible(TRUE)
        },

        transact = function(source, mutate, validate = FALSE) {
            if (!is.function(mutate)) {
                stop("mutate must be a function.", call. = FALSE)
            }
            if (isTRUE(shiny::isolate(private$state$updating))) {
                return(invisible(FALSE))
            }
            private$state$updating <- TRUE
            on.exit(private$state$updating <- FALSE, add = TRUE)

            candidate <- self$snapshot(validate = FALSE)
            outcome <- tryCatch({
                mutate(candidate)
                if (isTRUE(validate)) {
                    candidate$validate()
                }
                candidate
            }, error = function(error) error)
            if (inherits(outcome, "error")) {
                private$state$error <- conditionMessage(outcome)
                stop(outcome)
            }

            private$state$project <- outcome
            private$state$source <- as.character(source)
            private$state$error <- NULL
            private$state$revision <- shiny::isolate(private$state$revision) + 1L
            invisible(TRUE)
        }
    ),
    private = list(
        state = NULL
    )
)

#' @title Create a reactive AlphaProject store
#' @keywords internal
createAlphaProjectStore <- function(project = createDemoAlphaProject()) {
    AlphaProjectStore$new(project)
}

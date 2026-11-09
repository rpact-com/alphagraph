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
#' @title
#' Reactive store for an AlphaProject
#'
#' @description
#' Owns project datasets, analysis specifications, and results while exposing
#' the embedded graph through the existing `AlphaGraphStore`. A project
#' snapshot always embeds the current graph-store snapshot.
#'
#' @details
#' Snapshots embed the current graph-store snapshot. Dataset and analysis
#' updates invalidate affected results; graph edits are detected by input hashes.
#' `$result()` returns only results for current inputs. Use reactive accessors in
#' a reactive context or inside `shiny::isolate()`. Transactions commit project
#' data; graph changes use the shared `graphStore`.
#'
#' @return An R6 class generator.
#'
#' @examples
#' store <- createAlphaProjectStore()
#' store$snapshot()$pValues("primary")
#' shiny::isolate(store$revision())
#'
#' @keywords internal
#'
AlphaProjectStore <- R6::R6Class(
    "AlphaProjectStore",
    public = list(
        #'
        #' @field graphStore AlphaGraphStore holding the current project graph;
        #'   initialized from the supplied project and shared with graph views.
        #'
        graphStore = NULL,

        #'
        #' @description
        #' Initialize the session store from a validated independent copy.
        #'
        #' @param project An AlphaProject instance; the demo project is used when omitted at
        #'   initialization.
        #'
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

        #'
        #' @description
        #' Return the reactive revision counter used by dependent views.
        #'
        revision = function() {
            graphRevision <- self$graphStore$revision()
            projectRevision <- private$state$revision
            projectRevision + graphRevision
        },

        #'
        #' @description
        #' Return the reactive source label of the last committed store change.
        #'
        source = function() private$state$source,
        #'
        #' @description
        #' Return the reactive message from the last failed transaction, or NULL.
        #'
        error = function() private$state$error,

        #'
        #' @description
        #' Return an independent snapshot without registering reactive dependencies.
        #'
        #' @param validate Logical; whether to validate the imported, returned, or modified
        #'   object.
        #'
        snapshot = function(validate = FALSE) {
            projectList <- shiny::isolate(private$state$project$toList())
            projectList$graph <- self$graphStore$snapshot()$toList()
            alphaProjectFromList(projectList, validate = validate)
        },

        #'
        #' @description
        #' Return an independent AlphaProject with the current graph and register revision
        #'   dependencies.
        #'
        #' @param validate Logical; whether to validate the imported, returned, or modified
        #'   object.
        #'
        project = function(validate = FALSE) {
            self$revision()
            projectList <- private$state$project$toList()
            projectList$graph <- self$graphStore$graph()$toList()
            alphaProjectFromList(projectList, validate = validate)
        },

        #'
        #' @description
        #' Return dataset p-values and register a reactive revision dependency.
        #'
        #' @param datasetId Character identifier of a p-value dataset.
        #'
        pValues = function(datasetId = "primary") {
            self$revision()
            private$state$project$pValues(datasetId)
        },

        #'
        #' @description
        #' Insert or replace dataset p-values in a transaction, preserving existing dataset
        #'   metadata.
        #'
        #' @param pValues Named numeric vector or list of hypothesisId/value records, keyed by
        #'   stable hypothesis IDs.
        #' @param datasetId Character identifier of a p-value dataset.
        #' @param label Human-readable character label.
        #' @param source Character scalar identifying the origin of a committed change.
        #'
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

        #'
        #' @description
        #' Forward ... to AlphaProject$upsertAnalysis() in a transaction.
        #'
        #' @param ... Named arguments passed to the operation described above.
        #' @param source Character scalar identifying the origin of a committed change.
        #'
        upsertAnalysis = function(..., source = "project:analysis") {
            arguments <- list(...)
            self$transact(source, function(project) {
                do.call(project$upsertAnalysis, arguments)
            })
        },

        #'
        #' @description
        #' Return a stored analysis result only if its input fingerprint is current; otherwise
        #'   return NULL.
        #'
        #' @param analysisId Character identifier of an analysis specification.
        #'
        result = function(analysisId) {
            self$revision()
            project <- self$project(validate = FALSE)
            result <- project$result(analysisId)
            if (is.null(result) || !isTRUE(project$resultIsCurrent(analysisId))) {
                return(NULL)
            }
            result
        },

        #'
        #' @description
        #' Run shortcut testing in a validated transaction and invisibly return the result.
        #'
        #' @param analysisId Character identifier of an analysis specification.
        #' @param source Character scalar identifying the origin of a committed change.
        #'
        runShortcut = function(analysisId, source = "project:shortcut") {
            result <- NULL
            self$transact(source, function(project) {
                result <<- project$runShortcut(analysisId)
            }, validate = TRUE)
            invisible(result)
        },

        #'
        #' @description
        #' Replace the stored object and update its revision and source label.
        #'
        #' @param project An AlphaProject instance; the demo project is used when omitted at
        #'   initialization.
        #' @param source Character scalar identifying the origin of a committed change.
        #'
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

        #'
        #' @description
        #' Apply mutate to a private candidate and commit after optional validation. Failures
        #'   preserve the previous state and are rethrown. Return invisible TRUE on commit or
        #'   FALSE for a nested transaction.
        #'
        #' @param source Character scalar identifying the origin of a committed change.
        #' @param mutate Function that modifies a private candidate object before it is
        #'   committed.
        #' @param validate Logical; whether to validate the imported, returned, or modified
        #'   object.
        #'
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

#'
#' @title
#' Create a reactive AlphaProject store
#'
#' @description
#' Create a project store and its shared graph store from the supplied project.
#'
#' @param project Initial `AlphaProject`; defaults to the demo project.
#' @return An `AlphaProjectStore` instance.
#'
#' @keywords internal
#'
createAlphaProjectStore <- function(project = createDemoAlphaProject()) {
    AlphaProjectStore$new(project)
}

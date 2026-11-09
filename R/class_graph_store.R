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
#' Reactive store for a shared AlphaGraph
#'
#' @description
#' Holds one canonical `AlphaGraph` per Shiny session. All mutations are
#' applied to a private copy and committed atomically by increasing a revision
#' counter. Views depend on the revision; view rendering never writes back to
#' the store, preventing graph/matrix feedback loops.
#'
#' @details
#' Use `$transact(source, mutate)` for changes. Nested transactions are ignored
#' and failed transactions preserve the previous graph. Reactive accessors must
#' be used inside a reactive context, or inside `shiny::isolate()`.
#' `$snapshot()` returns an independent graph without reactive dependencies.
#'
#' @return An R6 class generator.
#'
#' @examples
#' store <- createAlphaGraphStore()
#' store$snapshot()$toHypotheses()
#' shiny::isolate(store$revision())
#'
#' @keywords internal
#'
AlphaGraphStore <- R6::R6Class(
    "AlphaGraphStore",
    public = list(
        #'
        #' @description
        #' Initialize the session store from a validated independent copy.
        #'
        #' @param graph An AlphaGraph instance; the demo graph is used when omitted at
        #'   initialization.
        #'
        initialize = function(graph = createDemoAlphaGraph()) {
            if (!inherits(graph, "AlphaGraph")) {
                stop("graph must be an AlphaGraph object.", call. = FALSE)
            }
            graph$validate(requireNumeric = FALSE)
            private$state <- shiny::reactiveValues(
                graph = alphaGraphFromList(graph$toList()),
                revision = 0L,
                source = "initial",
                updating = FALSE,
                error = NULL
            )
            invisible(self)
        },

        #'
        #' @description
        #' Return the current canonical AlphaGraph and register a reactive revision dependency.
        #'
        graph = function() {
            private$state$revision
            private$state$graph
        },

        #'
        #' @description
        #' Return the reactive revision counter used by dependent views.
        #'
        revision = function() {
            private$state$revision
        },

        #'
        #' @description
        #' Return the reactive source label of the last committed store change.
        #'
        source = function() {
            private$state$source
        },

        #'
        #' @description
        #' Return the reactive message from the last failed transaction, or NULL.
        #'
        error = function() {
            private$state$error
        },

        #'
        #' @description
        #' Return an independent snapshot without registering reactive dependencies.
        #'
        snapshot = function() {
            shiny::isolate(alphaGraphFromList(private$state$graph$toList()))
        },

        #'
        #' @description
        #' Replace the stored object and update its revision and source label.
        #'
        #' @param graph An AlphaGraph instance; the demo graph is used when omitted at
        #'   initialization.
        #' @param source Character scalar identifying the origin of a committed change.
        #' @param validate Logical; whether to validate the imported, returned, or modified
        #'   object.
        #'
        replace = function(graph, source = "external", validate = TRUE) {
            if (!inherits(graph, "AlphaGraph")) {
                stop("graph must be an AlphaGraph object.", call. = FALSE)
            }
            self$transact(
                source = source,
                validate = validate,
                mutate = function(candidate) candidate$loadList(graph$toList())
            )
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
        #' @param requireNumeric Logical; whether weights and transitions must be numerically
        #'   computable.
        #'
        transact = function(source, mutate, validate = TRUE,
                            requireNumeric = FALSE) {
            if (!is.function(mutate)) {
                stop("mutate must be a function.", call. = FALSE)
            }
            if (isTRUE(shiny::isolate(private$state$updating))) {
                return(invisible(FALSE))
            }

            private$state$updating <- TRUE
            on.exit(private$state$updating <- FALSE, add = TRUE)
            candidate <- self$snapshot()
            result <- tryCatch(
                {
                    mutate(candidate)
                    if (isTRUE(validate)) {
                        candidate$validate(requireNumeric = requireNumeric)
                    }
                    candidate
                },
                error = function(error) error
            )
            if (inherits(result, "error")) {
                private$state$error <- conditionMessage(result)
                stop(result)
            }

            private$state$graph <- candidate
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
#' Create a reactive AlphaGraph store
#'
#' @description
#' Create a session store containing an independent copy of the supplied graph.
#'
#' @param graph Initial canonical graph.
#'
#' @return An `AlphaGraphStore` object.
#'
#' @keywords internal
#'
createAlphaGraphStore <- function(graph = createDemoAlphaGraph()) {
    AlphaGraphStore$new(graph)
}

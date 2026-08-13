##
## alphagraph
##
## Shared reactive state for synchronized graph editors.
##

#' @title Reactive store for a shared AlphaGraph
#'
#' @description
#' Holds one canonical `AlphaGraph` per Shiny session. All mutations are
#' applied to a private copy and committed atomically by increasing a revision
#' counter. Views depend on the revision; view rendering never writes back to
#' the store, preventing graph/matrix feedback loops.
#'
#' @keywords internal
AlphaGraphStore <- R6::R6Class(
    "AlphaGraphStore",
    public = list(
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

        graph = function() {
            private$state$revision
            private$state$graph
        },

        revision = function() {
            private$state$revision
        },

        source = function() {
            private$state$source
        },

        error = function() {
            private$state$error
        },

        snapshot = function() {
            shiny::isolate(alphaGraphFromList(private$state$graph$toList()))
        },

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

#' @title Create a reactive AlphaGraph store
#'
#' @param graph Initial canonical graph.
#'
#' @return An `AlphaGraphStore` object.
#'
#' @keywords internal
createAlphaGraphStore <- function(graph = createDemoAlphaGraph()) {
    AlphaGraphStore$new(graph)
}

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

`%||%` <- function(x, y) {
    if (is.null(x)) {
        return(y)
    }
    return(x)
}

#' @title JSON-friendly alphagraph graph model
#'
#' @description
#' R6 container for a canonical alphagraph graph definition. The internal data
#' model is a plain R list that can be serialized with jsonlite and converted to
#' graphicalMCP inputs, editable transition matrices, and visNetwork data.
#'
#' @keywords internal
AlphaGraph <- R6::R6Class(
    "AlphaGraph",
    public = list(
        initialize = function(id = "graph", label = id, description = NULL,
                              metadata = list(), settings = list()) {
            private$data <- list(
                format = "alphagraph.graph",
                version = 1L,
                id = as.character(id),
                label = as.character(label %||% id),
                description = description,
                parameters = list(),
                nodes = list(),
                edges = list(),
                settings = private$mergeLists(private$defaultSettings(), settings),
                metadata = metadata %||% list()
            )
            invisible(self)
        },

        addParameter = function(id, value = NULL, label = id, description = NULL,
                                replace = FALSE) {
            private$assertScalarCharacter(id, "id")
            index <- private$parameterIndex(id)
            if (!is.na(index) && !isTRUE(replace)) {
                stop("Parameter '", id, "' already exists.", call. = FALSE)
            }

            parameter <- list(
                id = as.character(id),
                label = as.character(label %||% id),
                value = private$asNullableNumber(value),
                description = description
            )

            if (is.na(index)) {
                private$data$parameters[[length(private$data$parameters) + 1L]] <- parameter
            } else {
                private$data$parameters[[index]] <- private$mergeLists(
                    private$data$parameters[[index]],
                    parameter
                )
            }

            invisible(self)
        },

        setParameter = function(id, value = NULL, label = NULL, description = NULL) {
            index <- private$requireParameterIndex(id)
            if (!is.null(value)) {
                private$data$parameters[[index]]$value <- private$asNullableNumber(value)
            }
            if (!is.null(label)) {
                private$data$parameters[[index]]$label <- as.character(label)
            }
            if (!is.null(description)) {
                private$data$parameters[[index]]$description <- description
            }
            invisible(self)
        },

        addNode = function(id, label = id, weight = 0, weightLabel = NULL,
                           weightExpression = NULL, weightValue = NULL,
                           x = NULL, y = NULL, visLabel = NULL,
                           shape = "circle", colorBackground = "white",
                           colorBorder = "black", borderWidth = 2,
                           fontSize = 22, vis = list(), metadata = list(),
                           replace = FALSE) {
            private$assertScalarCharacter(id, "id")
            index <- private$nodeIndex(id)
            if (!is.na(index) && !isTRUE(replace)) {
                stop("Node '", id, "' already exists.", call. = FALSE)
            }

            weightSpec <- private$valueSpec(
                input = weight,
                display = weightLabel,
                expression = weightExpression,
                value = weightValue,
                defaultDisplay = "0"
            )
            nodeLabel <- as.character(label %||% id)
            node <- list(
                id = as.character(id),
                label = nodeLabel,
                weight = weightSpec,
                vis = private$mergeLists(
                    list(
                        label = visLabel %||% private$nodeVisLabel(nodeLabel, weightSpec),
                        x = private$asNullableNumber(x),
                        y = private$asNullableNumber(y),
                        shape = shape,
                        color = list(
                            background = colorBackground,
                            border = colorBorder
                        ),
                        borderWidth = private$asNullableNumber(borderWidth),
                        font = list(size = private$asNullableNumber(fontSize))
                    ),
                    vis %||% list()
                ),
                metadata = metadata %||% list()
            )

            if (is.na(index)) {
                private$data$nodes[[length(private$data$nodes) + 1L]] <- node
            } else {
                private$data$nodes[[index]] <- private$mergeLists(
                    private$data$nodes[[index]],
                    node
                )
            }

            invisible(self)
        },

        removeNode = function(id, removeEdges = TRUE) {
            index <- private$requireNodeIndex(id)
            private$data$nodes <- private$data$nodes[-index]
            if (isTRUE(removeEdges)) {
                keep <- vapply(
                    private$data$edges,
                    function(edge) edge$from != id && edge$to != id,
                    logical(1L)
                )
                private$data$edges <- private$data$edges[keep]
            }
            invisible(self)
        },

        addEdge = function(from, to, transition = NULL, label = NULL,
                           expression = NULL, value = NULL, id = NULL,
                           arrows = "to", smooth = FALSE, fontSize = 20,
                           fontAlign = "top", color = NULL, vis = list(),
                           metadata = list(), replace = FALSE) {
            private$requireNodeIndex(from)
            private$requireNodeIndex(to)
            if (identical(from, to)) {
                stop("Self transitions are not allowed because the transition matrix diagonal must be 0.",
                     call. = FALSE)
            }

            edgeId <- as.character(id %||% private$edgeId(from, to))
            index <- private$edgeIndex(edge = edgeId)
            pairIndex <- private$edgeIndex(from = from, to = to)
            if (!is.na(pairIndex) && is.na(index)) {
                index <- pairIndex
            }
            if (!is.na(index) && !isTRUE(replace)) {
                stop("Edge from '", from, "' to '", to, "' already exists.", call. = FALSE)
            }

            transitionSpec <- private$valueSpec(
                input = transition,
                display = label,
                expression = expression,
                value = value,
                defaultDisplay = "0"
            )
            edge <- list(
                id = edgeId,
                from = as.character(from),
                to = as.character(to),
                transition = transitionSpec,
                vis = private$mergeLists(
                    list(
                        label = transitionSpec$display,
                        arrows = arrows,
                        smooth = smooth,
                        color = color,
                        font = list(
                            size = private$asNullableNumber(fontSize),
                            align = fontAlign
                        )
                    ),
                    vis %||% list()
                ),
                metadata = metadata %||% list()
            )

            if (is.na(index)) {
                private$data$edges[[length(private$data$edges) + 1L]] <- edge
            } else {
                private$data$edges[[index]] <- private$mergeLists(
                    private$data$edges[[index]],
                    edge
                )
            }

            invisible(self)
        },

        removeEdge = function(edge = NULL, from = NULL, to = NULL) {
            index <- private$requireEdgeIndex(edge = edge, from = from, to = to)
            private$data$edges <- private$data$edges[-index]
            invisible(self)
        },

        setNodeLabel = function(id, label, updateVisLabel = TRUE) {
            index <- private$requireNodeIndex(id)
            private$data$nodes[[index]]$label <- as.character(label)
            if (isTRUE(updateVisLabel)) {
                private$data$nodes[[index]]$vis$label <- private$nodeVisLabel(
                    label,
                    private$data$nodes[[index]]$weight
                )
            }
            invisible(self)
        },

        setNodeWeight = function(id, weight = NULL, label = NULL,
                                 expression = NULL, value = NULL,
                                 updateVisLabel = TRUE) {
            index <- private$requireNodeIndex(id)
            private$data$nodes[[index]]$weight <- private$valueSpec(
                input = weight,
                display = label,
                expression = expression,
                value = value,
                current = private$data$nodes[[index]]$weight,
                defaultDisplay = "0"
            )
            if (isTRUE(updateVisLabel)) {
                private$data$nodes[[index]]$vis$label <- private$nodeVisLabel(
                    private$data$nodes[[index]]$label,
                    private$data$nodes[[index]]$weight
                )
            }
            invisible(self)
        },

        setNodeVisLabel = function(id, label) {
            index <- private$requireNodeIndex(id)
            private$data$nodes[[index]]$vis$label <- as.character(label)
            invisible(self)
        },

        setNodeXY = function(id, x, y) {
            index <- private$requireNodeIndex(id)
            private$data$nodes[[index]]$vis$x <- private$asNullableNumber(x)
            private$data$nodes[[index]]$vis$y <- private$asNullableNumber(y)
            invisible(self)
        },

        setNodeColor = function(id, background = NULL, border = NULL,
                                highlight = NULL, hover = NULL) {
            index <- private$requireNodeIndex(id)
            color <- private$data$nodes[[index]]$vis$color %||% list()
            if (!is.null(background)) {
                color$background <- background
            }
            if (!is.null(border)) {
                color$border <- border
            }
            if (!is.null(highlight)) {
                color$highlight <- highlight
            }
            if (!is.null(hover)) {
                color$hover <- hover
            }
            private$data$nodes[[index]]$vis$color <- color
            invisible(self)
        },

        setNodeFontSize = function(id, size) {
            index <- private$requireNodeIndex(id)
            font <- private$data$nodes[[index]]$vis$font %||% list()
            font$size <- private$asNullableNumber(size)
            private$data$nodes[[index]]$vis$font <- font
            invisible(self)
        },

        setNodeVis = function(id, ...) {
            index <- private$requireNodeIndex(id)
            private$data$nodes[[index]]$vis <- private$mergeLists(
                private$data$nodes[[index]]$vis %||% list(),
                list(...)
            )
            invisible(self)
        },

        setEdgeLabel = function(edge = NULL, label, from = NULL, to = NULL,
                                expression = NULL, value = NULL) {
            index <- private$requireEdgeIndex(edge = edge, from = from, to = to)
            private$data$edges[[index]]$transition <- private$valueSpec(
                input = NULL,
                display = label,
                expression = expression,
                value = value,
                current = private$data$edges[[index]]$transition,
                defaultDisplay = "0"
            )
            private$data$edges[[index]]$vis$label <- as.character(label)
            invisible(self)
        },

        setEdgeTransition = function(edge = NULL, transition = NULL,
                                     label = NULL, expression = NULL,
                                     value = NULL, from = NULL, to = NULL) {
            index <- private$requireEdgeIndex(edge = edge, from = from, to = to)
            private$data$edges[[index]]$transition <- private$valueSpec(
                input = transition,
                display = label,
                expression = expression,
                value = value,
                current = private$data$edges[[index]]$transition,
                defaultDisplay = "0"
            )
            private$data$edges[[index]]$vis$label <- private$data$edges[[index]]$transition$display
            invisible(self)
        },

        setEdgeColor = function(edge = NULL, color = NULL, from = NULL, to = NULL) {
            index <- private$requireEdgeIndex(edge = edge, from = from, to = to)
            private$data$edges[[index]]$vis$color <- color
            invisible(self)
        },

        setEdgeFontSize = function(edge = NULL, size, from = NULL, to = NULL) {
            index <- private$requireEdgeIndex(edge = edge, from = from, to = to)
            font <- private$data$edges[[index]]$vis$font %||% list()
            font$size <- private$asNullableNumber(size)
            private$data$edges[[index]]$vis$font <- font
            invisible(self)
        },

        setEdgeSmooth = function(edge = NULL, smooth, from = NULL, to = NULL) {
            index <- private$requireEdgeIndex(edge = edge, from = from, to = to)
            private$data$edges[[index]]$vis$smooth <- smooth
            invisible(self)
        },

        setEdgeVis = function(edge = NULL, from = NULL, to = NULL, ...) {
            index <- private$requireEdgeIndex(edge = edge, from = from, to = to)
            private$data$edges[[index]]$vis <- private$mergeLists(
                private$data$edges[[index]]$vis %||% list(),
                list(...)
            )
            invisible(self)
        },

        setVisOptions = function(...) {
            private$data$settings$vis <- private$mergeLists(
                private$data$settings$vis %||% list(),
                list(...)
            )
            invisible(self)
        },

        loadList = function(x, validate = TRUE) {
            if (!is.list(x)) {
                stop("x must be a list.", call. = FALSE)
            }
            private$data <- private$mergeLists(private$emptyData(), x)
            if (isTRUE(validate)) {
                self$validate(requireNumeric = FALSE)
            }
            invisible(self)
        },

        fromJson = function(json, validate = TRUE) {
            if (!requireNamespace("jsonlite", quietly = TRUE)) {
                stop("Package 'jsonlite' is required to read graph JSON.", call. = FALSE)
            }
            self$loadList(jsonlite::fromJSON(json, simplifyVector = FALSE), validate = validate)
            invisible(self)
        },

        toList = function() {
            private$data
        },

        toJson = function(pretty = TRUE, auto_unbox = TRUE, null = "null", ...) {
            if (!requireNamespace("jsonlite", quietly = TRUE)) {
                stop("Package 'jsonlite' is required to serialize graph JSON.", call. = FALSE)
            }
            jsonlite::toJSON(
                private$data,
                pretty = pretty,
                auto_unbox = auto_unbox,
                null = null,
                ...
            )
        },

        toJSON = function(...) {
            self$toJson(...)
        },

        toJason = function(...) {
            self$toJson(...)
        },

        toTransitionMatrix = function(type = c("display", "value", "expression"),
                                      parameters = list(), asDataFrame = TRUE,
                                      rowNames = c("label", "id"),
                                      zero = NULL, validate = FALSE) {
            type <- match.arg(type)
            rowNames <- match.arg(rowNames)
            if (isTRUE(validate)) {
                self$validate(requireNumeric = identical(type, "value"), parameters = parameters)
            }

            nodes <- private$data$nodes
            nodeIds <- vapply(nodes, function(node) node$id, character(1L))
            labels <- vapply(nodes, function(node) node[[rowNames]], character(1L))
            n <- length(nodes)
            fill <- zero %||% if (identical(type, "value")) 0 else "0"
            mat <- matrix(fill, nrow = n, ncol = n)
            rownames(mat) <- labels
            colnames(mat) <- labels

            if (identical(type, "value")) {
                storage.mode(mat) <- "double"
            }

            for (edge in private$data$edges) {
                i <- match(edge$from, nodeIds)
                j <- match(edge$to, nodeIds)
                if (is.na(i) || is.na(j)) {
                    next
                }
                mat[i, j] <- private$valueFromSpec(edge$transition, type, parameters)
            }

            if (isTRUE(asDataFrame)) {
                return(as.data.frame(mat, check.names = FALSE, stringsAsFactors = FALSE))
            }

            mat
        },

        toHypotheses = function(type = c("value", "display", "expression"),
                                parameters = list(), validate = FALSE) {
            type <- match.arg(type)
            if (isTRUE(validate)) {
                self$validate(requireNumeric = identical(type, "value"), parameters = parameters)
            }
            values <- vapply(
                private$data$nodes,
                function(node) private$valueFromSpec(node$weight, type, parameters),
                if (identical(type, "value")) numeric(1L) else character(1L)
            )
            names(values) <- vapply(private$data$nodes, function(node) node$id, character(1L))
            values
        },

        toGraphicalMCPInput = function(parameters = list(), validate = TRUE,
                                       hypNames = c("label", "id")) {
            hypNames <- match.arg(hypNames)
            hypotheses <- self$toHypotheses(
                type = "value",
                parameters = parameters,
                validate = FALSE
            )
            transitions <- self$toTransitionMatrix(
                type = "value",
                parameters = parameters,
                asDataFrame = FALSE,
                validate = FALSE
            )
            names <- vapply(private$data$nodes, function(node) node[[hypNames]], character(1L))
            names(hypotheses) <- names
            rownames(transitions) <- names
            colnames(transitions) <- names

            out <- list(
                hypotheses = hypotheses,
                transitions = transitions,
                hyp_names = names
            )
            if (isTRUE(validate)) {
                self$validate(requireNumeric = TRUE, parameters = parameters)
            }
            out
        },

        toGraphicalMCP = function(parameters = list(), validate = TRUE,
                                  hypNames = c("label", "id")) {
            if (!requireNamespace("graphicalMCP", quietly = TRUE)) {
                stop("Package 'graphicalMCP' is required to create a graphicalMCP graph.",
                     call. = FALSE)
            }
            input <- self$toGraphicalMCPInput(
                parameters = parameters,
                validate = validate,
                hypNames = hypNames
            )
            graphicalMCP::graph_create(
                hypotheses = input$hypotheses,
                transitions = input$transitions,
                hyp_names = input$hyp_names
            )
        },

        toVisNetworkData = function() {
            list(
                nodes = private$recordsToDataFrame(
                    lapply(private$data$nodes, private$nodeToVisRecord)
                ),
                edges = private$recordsToDataFrame(
                    lapply(private$data$edges, private$edgeToVisRecord)
                )
            )
        },

        toVisNetwork = function(width = NULL, height = NULL,
                                applyDefaults = TRUE) {
            if (!requireNamespace("visNetwork", quietly = TRUE)) {
                stop("Package 'visNetwork' is required to render the graph.",
                     call. = FALSE)
            }
            graphData <- self$toVisNetworkData()
            vis <- private$data$settings$vis %||% list()
            widget <- visNetwork::visNetwork(
                graphData$nodes,
                graphData$edges,
                width = width %||% vis$width %||% "100%",
                height = height %||% vis$height %||% "400px"
            )
            if (isTRUE(applyDefaults)) {
                if (!is.null(vis$edges)) {
                    widget <- do.call(visNetwork::visEdges, c(list(graph = widget), vis$edges))
                }
                if (!is.null(vis$nodes)) {
                    widget <- do.call(visNetwork::visNodes, c(list(graph = widget), vis$nodes))
                }
                if (!is.null(vis$interaction)) {
                    widget <- do.call(visNetwork::visInteraction, c(list(graph = widget), vis$interaction))
                }
                if (!is.null(vis$physics)) {
                    widget <- do.call(visNetwork::visPhysics, c(list(graph = widget), vis$physics))
                }
            }
            widget
        },

        validate = function(requireNumeric = FALSE, parameters = list(),
                            tolerance = sqrt(.Machine$double.eps)) {
            nodeIds <- vapply(private$data$nodes, function(node) node$id, character(1L))
            if (any(!nzchar(nodeIds))) {
                stop("All nodes must have non-empty ids.", call. = FALSE)
            }
            if (anyDuplicated(nodeIds)) {
                stop("Node ids must be unique.", call. = FALSE)
            }

            edgeIds <- vapply(private$data$edges, function(edge) edge$id, character(1L))
            if (length(edgeIds) > 0L && anyDuplicated(edgeIds)) {
                stop("Edge ids must be unique.", call. = FALSE)
            }

            pairs <- vapply(
                private$data$edges,
                function(edge) paste(edge$from, edge$to, sep = "\r"),
                character(1L)
            )
            if (length(pairs) > 0L && anyDuplicated(pairs)) {
                stop("Only one transition per ordered node pair is supported.", call. = FALSE)
            }

            for (edge in private$data$edges) {
                if (!edge$from %in% nodeIds) {
                    stop("Edge '", edge$id, "' references unknown source node '",
                         edge$from, "'.", call. = FALSE)
                }
                if (!edge$to %in% nodeIds) {
                    stop("Edge '", edge$id, "' references unknown target node '",
                         edge$to, "'.", call. = FALSE)
                }
                if (identical(edge$from, edge$to)) {
                    stop("Edge '", edge$id, "' is a self transition.", call. = FALSE)
                }
            }

            if (isTRUE(requireNumeric)) {
                hypotheses <- self$toHypotheses(
                    type = "value",
                    parameters = parameters,
                    validate = FALSE
                )
                transitions <- self$toTransitionMatrix(
                    type = "value",
                    parameters = parameters,
                    asDataFrame = FALSE,
                    validate = FALSE
                )
                private$assertProbabilities(hypotheses, "Hypothesis weights", tolerance)
                private$assertProbabilities(as.vector(transitions), "Transition values", tolerance)
                if (sum(hypotheses) > 1 + tolerance) {
                    stop("Hypothesis weights must sum to at most 1.", call. = FALSE)
                }
                rowSums <- rowSums(transitions)
                if (any(rowSums > 1 + tolerance)) {
                    stop("Each transition matrix row must sum to at most 1.", call. = FALSE)
                }
                if (any(abs(diag(transitions)) > tolerance)) {
                    stop("The transition matrix diagonal must be 0.", call. = FALSE)
                }
            }

            TRUE
        }
    ),
    private = list(
        data = NULL,

        emptyData = function() {
            list(
                format = "alphagraph.graph",
                version = 1L,
                id = "graph",
                label = "graph",
                description = NULL,
                parameters = list(),
                nodes = list(),
                edges = list(),
                settings = private$defaultSettings(),
                metadata = list()
            )
        },

        defaultSettings = function() {
            list(
                vis = list(
                    width = "100%",
                    height = "400px",
                    edges = list(
                        arrows = "to",
                        smooth = list(type = "curvedCW", roundness = 0.3)
                    ),
                    nodes = list(
                        shape = "circle",
                        borderWidth = 2,
                        color = list(background = "white", border = "black"),
                        scaling = list(
                            label = list(enabled = TRUE, min = 10, max = 16)
                        )
                    ),
                    interaction = list(
                        dragNodes = TRUE,
                        dragView = TRUE,
                        zoomView = FALSE
                    ),
                    physics = list(enabled = FALSE)
                ),
                grid = list(size = 40, origin = list(x = 14, y = -5))
            )
        },

        parameterIndex = function(id) {
            ids <- vapply(private$data$parameters, function(parameter) parameter$id, character(1L))
            index <- match(id, ids)
            if (is.na(index)) {
                return(NA_integer_)
            }
            index
        },

        requireParameterIndex = function(id) {
            index <- private$parameterIndex(id)
            if (is.na(index)) {
                stop("Unknown parameter '", id, "'.", call. = FALSE)
            }
            index
        },

        nodeIndex = function(id) {
            ids <- vapply(private$data$nodes, function(node) node$id, character(1L))
            index <- match(id, ids)
            if (is.na(index)) {
                return(NA_integer_)
            }
            index
        },

        requireNodeIndex = function(id) {
            index <- private$nodeIndex(id)
            if (is.na(index)) {
                stop("Unknown node '", id, "'.", call. = FALSE)
            }
            index
        },

        edgeIndex = function(edge = NULL, from = NULL, to = NULL) {
            if (!is.null(edge)) {
                ids <- vapply(private$data$edges, function(x) x$id, character(1L))
                index <- match(edge, ids)
                if (!is.na(index)) {
                    return(index)
                }
            }
            if (!is.null(from) && !is.null(to)) {
                matches <- vapply(
                    private$data$edges,
                    function(x) identical(x$from, from) && identical(x$to, to),
                    logical(1L)
                )
                index <- match(TRUE, matches)
                if (!is.na(index)) {
                    return(index)
                }
            }
            NA_integer_
        },

        requireEdgeIndex = function(edge = NULL, from = NULL, to = NULL) {
            index <- private$edgeIndex(edge = edge, from = from, to = to)
            if (is.na(index)) {
                if (!is.null(edge)) {
                    stop("Unknown edge '", edge, "'.", call. = FALSE)
                }
                stop("Unknown edge from '", from, "' to '", to, "'.", call. = FALSE)
            }
            index
        },

        edgeId = function(from, to) {
            paste0(from, "__", to)
        },

        valueSpec = function(input = NULL, display = NULL, expression = NULL,
                             value = NULL, current = NULL,
                             defaultDisplay = NULL) {
            spec <- current %||% list(display = NULL, expression = NULL, value = NULL)

            if (!is.null(input)) {
                if (is.numeric(input) || is.logical(input)) {
                    value <- input
                    if (is.null(display)) {
                        display <- private$formatNumber(input)
                    }
                    if (is.null(expression)) {
                        expression <- private$formatNumber(input)
                    }
                } else {
                    if (is.null(display)) {
                        display <- as.character(input)
                    }
                    if (is.null(expression)) {
                        expression <- as.character(input)
                    }
                }
            }

            if (!is.null(display)) {
                spec$display <- as.character(display)
            } else if (is.null(spec$display)) {
                spec$display <- defaultDisplay
            }

            if (!is.null(expression)) {
                spec$expression <- as.character(expression)
            } else if (is.null(spec$expression)) {
                spec$expression <- spec$display
            }

            if (!is.null(value)) {
                spec$value <- private$asNullableNumber(value)
            } else if (is.null(spec$value)) {
                spec$value <- private$tryParseNumber(spec$expression)
            }

            spec
        },

        valueFromSpec = function(spec, type, parameters = list()) {
            if (identical(type, "display")) {
                return(as.character(spec$display %||% "0"))
            }
            if (identical(type, "expression")) {
                return(as.character(spec$expression %||% spec$display %||% "0"))
            }
            if (!is.null(spec$value)) {
                return(private$asNullableNumber(spec$value))
            }
            private$evaluateExpression(spec$expression %||% spec$display, parameters)
        },

        evaluateExpression = function(expression, parameters = list()) {
            if (is.null(expression) || !nzchar(expression)) {
                return(0)
            }

            env <- new.env(parent = baseenv())
            for (parameter in private$data$parameters) {
                if (!is.null(parameter$value)) {
                    assign(parameter$id, parameter$value, envir = env)
                }
                if (!is.null(parameter$label) && !identical(parameter$label, parameter$id) &&
                    !is.null(parameter$value)) {
                    assign(parameter$label, parameter$value, envir = env)
                }
            }
            if (length(parameters) > 0L) {
                for (name in names(parameters)) {
                    assign(name, parameters[[name]], envir = env)
                }
            }

            value <- tryCatch(
                eval(parse(text = expression), envir = env),
                error = function(e) NA_real_
            )
            if (!is.numeric(value) || length(value) != 1L || is.na(value)) {
                stop("Expression '", expression, "' could not be evaluated to a numeric scalar.",
                     call. = FALSE)
            }
            as.numeric(value)
        },

        nodeVisLabel = function(label, weightSpec) {
            weightLabel <- weightSpec$display %||% private$formatNumber(weightSpec$value) %||% "0"
            paste0(label, "\n", weightLabel)
        },

        nodeToVisRecord = function(node) {
            private$flattenVisRecord(c(list(id = node$id), node$vis %||% list()))
        },

        edgeToVisRecord = function(edge) {
            private$flattenVisRecord(c(
                list(id = edge$id, from = edge$from, to = edge$to),
                edge$vis %||% list()
            ))
        },

        flattenVisRecord = function(record) {
            out <- list()
            for (name in names(record)) {
                value <- record[[name]]
                if (is.null(value)) {
                    next
                }
                if (is.list(value) && name %in% c("color", "font")) {
                    for (childName in names(value)) {
                        out[[paste0(name, ".", childName)]] <- value[[childName]]
                    }
                } else {
                    out[[name]] <- value
                }
            }
            out
        },

        recordsToDataFrame = function(records) {
            if (length(records) == 0L) {
                return(data.frame())
            }
            columns <- unique(unlist(lapply(records, names), use.names = FALSE))
            data <- lapply(columns, function(column) {
                values <- lapply(records, function(record) record[[column]] %||% NA)
                isScalarAtomic <- vapply(
                    values,
                    function(value) !is.list(value) && length(value) == 1L,
                    logical(1L)
                )
                if (all(isScalarAtomic)) {
                    return(unlist(values, recursive = FALSE, use.names = FALSE))
                }
                I(values)
            })
            names(data) <- columns
            as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE)
        },

        mergeLists = function(x, y) {
            if (is.null(y) || length(y) == 0L) {
                return(x)
            }
            if (is.null(x) || length(x) == 0L) {
                return(y)
            }
            for (name in names(y)) {
                if (!is.null(x[[name]]) && is.list(x[[name]]) && is.list(y[[name]]) &&
                    !is.data.frame(x[[name]]) && !is.data.frame(y[[name]])) {
                    x[[name]] <- private$mergeLists(x[[name]], y[[name]])
                } else {
                    x[[name]] <- y[[name]]
                }
            }
            x
        },

        assertScalarCharacter = function(x, name) {
            if (!is.character(x) || length(x) != 1L || !nzchar(x)) {
                stop(name, " must be a non-empty character scalar.", call. = FALSE)
            }
        },

        assertProbabilities = function(x, name, tolerance) {
            if (any(is.na(x))) {
                stop(name, " must be fully numeric before validation.", call. = FALSE)
            }
            if (any(x < -tolerance | x > 1 + tolerance)) {
                stop(name, " must be between 0 and 1.", call. = FALSE)
            }
        },

        asNullableNumber = function(x) {
            if (is.null(x)) {
                return(NULL)
            }
            if (length(x) != 1L || is.na(x)) {
                return(NULL)
            }
            as.numeric(x)
        },

        tryParseNumber = function(x) {
            if (is.null(x)) {
                return(NULL)
            }
            value <- suppressWarnings(as.numeric(x))
            if (length(value) == 1L && !is.na(value)) {
                return(value)
            }
            NULL
        },

        formatNumber = function(x) {
            if (is.null(x) || length(x) != 1L || is.na(x)) {
                return(NULL)
            }
            format(x, trim = TRUE, scientific = FALSE)
        }
    )
)

#' @title Create the current Hung and Wang demo graph as an AlphaGraph
#'
#' @description
#' Convenience constructor mirroring the current static example used in the
#' graph and transition matrix modules.
#'
#' @return An `AlphaGraph` object.
#'
#' @keywords internal
createDemoAlphaGraph <- function() {
    graph <- AlphaGraph$new(
        id = "hung_wang_2010",
        label = "Hung and Wang (2010)"
    )

    graph$addParameter("nu", label = "\u03BD")
    graph$addParameter("omega", label = "\u03C9")
    graph$addParameter("t", label = "t")
    graph$addNode("H1_NI", label = getHypothesisLabel(1, "ni"), weight = 1,
                  x = 0, y = 0, colorBackground = "#7CFF7C")
    graph$addNode("H1_S", label = getHypothesisLabel(1, "s"), weight = 0,
                  x = 300, y = 0)
    graph$addNode("H2_NI", label = getHypothesisLabel(2, "ni"), weight = 0,
                  x = 0, y = 200)
    graph$addNode("H2_S", label = getHypothesisLabel(2, "s"), weight = 0,
                  x = 300, y = 200)
    graph$addEdge("H1_NI", "H1_S", label = "\u03BD", expression = "nu")
    graph$addEdge("H1_NI", "H2_NI", label = "1-\u03BD", expression = "1 - nu")
    graph$addEdge("H2_NI", "H1_S", label = "\u03C9", expression = "omega",
                  smooth = TRUE)
    graph$addEdge("H1_S", "H2_NI", label = "t", expression = "t",
                  smooth = TRUE)
    graph$addEdge("H2_NI", "H2_S", label = "1-\u03C9", expression = "1 - omega")
    graph$addEdge("H1_S", "H2_S", label = "1-t", expression = "1 - t")

    graph
}

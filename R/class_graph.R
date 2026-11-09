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

`%||%` <- function(x, y) {
    if (is.null(x)) {
        return(y)
    }
    return(x)
}

#'
#' @title
#' JSON-friendly alphagraph graph model
#'
#' @description
#' R6 container for a canonical alphagraph graph definition. The internal data
#' model is a plain R list that can be serialized with jsonlite and converted to
#' graphicalMCP inputs, editable transition matrices, and visNetwork data.
#'
#' @details
#' Create an instance with `AlphaGraph$new()`. Nodes represent hypotheses,
#' edges represent directed transitions, and nested `vis` fields hold presentation
#' properties. Symbolic drafts remain editable; numeric validation is required
#' before passing inputs to graphicalMCP. Mutating methods return the instance
#' invisibly unless their description states otherwise.
#'
#' @return An R6 class generator.
#'
#' @examples
#' graph <- createDemoAlphaGraph()
#' graph$validate(requireNumeric = TRUE)
#' graph$toTransitionMatrix(type = "value", rowNames = "id")
#'
#' @keywords internal
#' @importFrom graphicalMCP graph_create
#'
AlphaGraph <- R6::R6Class(
    "AlphaGraph",
    public = list(
        #'
        #' @description
        #' Initialize an empty canonical graph with default presentation settings.
        #'
        #' @param id Character scalar identifying the object within its collection.
        #' @param label Human-readable character label.
        #' @param description Optional character description; NULL omits it.
        #' @param metadata Named list of JSON-compatible metadata; empty by default.
        #' @param settings Named list of graph settings merged with the defaults.
        #'
        initialize = function(
                id = "graph",
                label = id,
                description = NULL,
                metadata = list(),
                settings = list()) {
            private$data <- list(
                format = "alphagraph.graph",
                version = 1L,
                id = as.character(id),
                label = as.character(label %||% id),
                description = description,
                parameters = list(),
                nodes = list(),
                edges = list(),
                settings = private$asObjectList(
                    private$mergeLists(private$defaultSettings(), settings),
                    "settings"
                ),
                metadata = private$asObjectList(metadata %||% list(), "metadata")
            )
            invisible(self)
        },
        #'
        #' @description
        #' Add a symbolic parameter, optionally replacing an existing parameter.
        #'
        #' @param id Character scalar identifying the object within its collection.
        #' @param value Optional numeric scalar supplying the stored numeric value.
        #' @param label Human-readable character label.
        #' @param description Optional character description; NULL omits it.
        #' @param replace Logical; whether an existing identifier may be replaced.
        #'
        addParameter = function(
                id,
                value = NULL,
                label = id,
                description = NULL,
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
        #'
        #' @description
        #' Update non-NULL properties of an existing symbolic parameter.
        #'
        #' @param id Character scalar identifying the object within its collection.
        #' @param value Optional numeric scalar supplying the stored numeric value.
        #' @param label Human-readable character label.
        #' @param description Optional character description; NULL omits it.
        #'
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
        #'
        #' @description
        #' Add a hypothesis with a stable identifier, weight, and presentation properties.
        #'
        #' @param id Character scalar identifying the object within its collection.
        #' @param label Human-readable character label.
        #' @param weight Numeric weight, fraction string, or symbolic expression.
        #' @param weightLabel Optional character display label for the hypothesis weight.
        #' @param weightExpression Optional character expression for the hypothesis weight.
        #' @param weightValue Optional numeric value for the hypothesis weight.
        #' @param x Numeric scalar or fraction string to parse or format.
        #' @param y Vertical numeric coordinate, or NULL for automatic placement.
        #' @param visLabel Optional character network label; derived from label and weight by
        #'   default.
        #' @param shape Character visNetwork node shape; defaults to "circle".
        #' @param colorBackground Node background color; defaults to "white".
        #' @param colorBorder Node border color; defaults to "black".
        #' @param borderWidth Numeric node border width; defaults to 2.
        #' @param fontSize Numeric label font size in pixels.
        #' @param vis Named list of additional visNetwork presentation properties.
        #' @param metadata Named list of JSON-compatible metadata; empty by default.
        #' @param replace Logical; whether an existing identifier may be replaced.
        #'
        addNode = function(
                id,
                label = id,
                weight = 0,
                weightLabel = NULL,
                weightExpression = NULL,
                weightValue = NULL,
                x = NULL,
                y = NULL,
                visLabel = NULL,
                shape = "circle",
                colorBackground = "white",
                colorBorder = "black",
                borderWidth = 2,
                # Change this default to adjust the font size of newly created nodes.
                fontSize = 14.0,
                vis = list(),
                metadata = list(),
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
            defaultVisLabel <- private$nodeVisLabel(nodeLabel, weightSpec)
            node <- list(
                id = as.character(id),
                label = nodeLabel,
                weight = weightSpec,
                vis = private$asObjectList(private$mergeLists(
                    list(
                        label = visLabel %||% defaultVisLabel,
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
                ), "node vis"),
                metadata = private$asObjectList(metadata %||% list(), "node metadata")
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
        #'
        #' @description
        #' Remove a hypothesis and, by default, its incident edges.
        #'
        #' @param id Character scalar identifying the object within its collection.
        #' @param removeEdges Logical; whether incident edges are also removed. Defaults to
        #'   TRUE.
        #'
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
        #'
        #' @description
        #' Add a directed transition between distinct existing hypotheses.
        #'
        #' @param from Character identifier of the source hypothesis.
        #' @param to Character identifier of the target hypothesis.
        #' @param transition Optional numeric transition, fraction string, or symbolic
        #'   expression.
        #' @param label Human-readable character label.
        #' @param expression Optional character expression for the stored weight or transition.
        #' @param value Optional numeric scalar supplying the stored numeric value.
        #' @param id Character scalar identifying the object within its collection.
        #' @param arrows visNetwork arrow specification; defaults to "to".
        #' @param smooth Logical or list specifying visNetwork edge smoothing.
        #' @param fontSize Numeric label font size in pixels.
        #' @param fontAlign Character edge-label alignment; defaults to "top".
        #' @param color Optional visNetwork color specification.
        #' @param vis Named list of additional visNetwork presentation properties.
        #' @param metadata Named list of JSON-compatible metadata; empty by default.
        #' @param replace Logical; whether an existing identifier may be replaced.
        #'
        addEdge = function(
                from,
                to,
                transition = NULL,
                label = NULL,
                expression = NULL,
                value = NULL,
                id = NULL,
                arrows = "to",
                smooth = FALSE,
                # Change this default to adjust the font size of newly created edge labels.
                fontSize = 15,
                fontAlign = "top",
                color = NULL,
                vis = list(),
                metadata = list(),
                replace = FALSE) {
            private$requireNodeIndex(from)
            private$requireNodeIndex(to)
            if (identical(from, to)) {
                stop("Self transitions are not allowed because the transition matrix diagonal must be 0.",
                    call. = FALSE
                )
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
                vis = private$asObjectList(private$mergeLists(
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
                ), "edge vis"),
                metadata = private$asObjectList(metadata %||% list(), "edge metadata")
            )

            if (is.na(index)) {
                private$data$edges[[length(private$data$edges) + 1L]] <- edge
            } else {
                private$data$edges[[index]] <- private$mergeLists(
                    private$data$edges[[index]],
                    edge
                )
            }
            private$refreshReciprocalEdgeSmooth()

            invisible(self)
        },
        #'
        #' @description
        #' Remove a directed transition and refresh reciprocal-edge smoothing.
        #'
        #' @param edge Optional character edge identifier; otherwise use from and to.
        #' @param from Character identifier of the source hypothesis.
        #' @param to Character identifier of the target hypothesis.
        #'
        removeEdge = function(edge = NULL, from = NULL, to = NULL) {
            index <- private$requireEdgeIndex(edge = edge, from = from, to = to)
            private$data$edges <- private$data$edges[-index]
            private$refreshReciprocalEdgeSmooth()
            invisible(self)
        },
        #'
        #' @description
        #' Set a canonical hypothesis label and optionally refresh its network label.
        #'
        #' @param id Character scalar identifying the object within its collection.
        #' @param label Human-readable character label.
        #' @param updateVisLabel Logical; whether to regenerate the network label. Defaults to
        #'   TRUE.
        #'
        setNodeLabel = function(id, label, updateVisLabel = TRUE) {
            index <- private$requireNodeIndex(id)
            label <- as.character(label)
            renderedLabel <- private$nodeVisLabel(
                label,
                private$data$nodes[[index]]$weight
            )
            private$data$nodes[[index]]$label <- label
            if (isTRUE(updateVisLabel)) {
                private$data$nodes[[index]]$vis$label <- renderedLabel
            }
            invisible(self)
        },
        #'
        #' @description
        #' Update the weight specification and optionally refresh the network label.
        #'
        #' @param id Character scalar identifying the object within its collection.
        #' @param weight Numeric weight, fraction string, or symbolic expression.
        #' @param label Human-readable character label.
        #' @param expression Optional character expression for the stored weight or transition.
        #' @param value Optional numeric scalar supplying the stored numeric value.
        #' @param updateVisLabel Logical; whether to regenerate the network label. Defaults to
        #'   TRUE.
        #'
        setNodeWeight = function(
                id,
                weight = NULL,
                label = NULL,
                expression = NULL,
                value = NULL,
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
        #'
        #' @description
        #' Set the presentation label without changing the canonical hypothesis label.
        #'
        #' @param id Character scalar identifying the object within its collection.
        #' @param label Human-readable character label.
        #'
        setNodeVisLabel = function(id, label) {
            index <- private$requireNodeIndex(id)
            private$data$nodes[[index]]$vis$label <- as.character(label)
            invisible(self)
        },
        #'
        #' @description
        #' Set the stored network coordinates of a hypothesis.
        #'
        #' @param id Character scalar identifying the object within its collection.
        #' @param x Horizontal numeric coordinate, or NULL for automatic placement.
        #' @param y Vertical numeric coordinate, or NULL for automatic placement.
        #'
        setNodeXY = function(id, x, y) {
            index <- private$requireNodeIndex(id)
            private$data$nodes[[index]]$vis$x <- private$asNullableNumber(x)
            private$data$nodes[[index]]$vis$y <- private$asNullableNumber(y)
            invisible(self)
        },
        #'
        #' @description
        #' Update non-NULL node color properties.
        #'
        #' @param id Character scalar identifying the object within its collection.
        #' @param background Optional node background color.
        #' @param border Optional node border color.
        #' @param highlight Optional visNetwork highlight color or color list.
        #' @param hover Optional visNetwork hover color or color list.
        #'
        setNodeColor = function(
                id,
                background = NULL,
                border = NULL,
                highlight = NULL,
                hover = NULL) {
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
        #'
        #' @description
        #' Set the node label font size.
        #'
        #' @param id Character scalar identifying the object within its collection.
        #' @param size Numeric font size in pixels, or NULL to clear the stored size.
        #'
        setNodeFontSize = function(id, size) {
            index <- private$requireNodeIndex(id)
            font <- private$data$nodes[[index]]$vis$font %||% list()
            font$size <- private$asNullableNumber(size)
            private$data$nodes[[index]]$vis$font <- font
            invisible(self)
        },
        #'
        #' @description
        #' Merge named presentation properties from ... into the node vis list.
        #'
        #' @param id Character scalar identifying the object within its collection.
        #' @param ... Named arguments passed to the operation described above.
        #'
        setNodeVis = function(id, ...) {
            index <- private$requireNodeIndex(id)
            private$data$nodes[[index]]$vis <- private$mergeLists(
                private$data$nodes[[index]]$vis %||% list(),
                list(...)
            )
            invisible(self)
        },
        #'
        #' @description
        #' Update the transition display label and optional expression or numeric value.
        #'
        #' @param edge Optional character edge identifier; otherwise use from and to.
        #' @param label Human-readable character label.
        #' @param from Character identifier of the source hypothesis.
        #' @param to Character identifier of the target hypothesis.
        #' @param expression Optional character expression for the stored weight or transition.
        #' @param value Optional numeric scalar supplying the stored numeric value.
        #'
        setEdgeLabel = function(
                edge = NULL,
                label,
                from = NULL,
                to = NULL,
                expression = NULL,
                value = NULL) {
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
        #'
        #' @description
        #' Update the transition specification and refresh its network label.
        #'
        #' @param edge Optional character edge identifier; otherwise use from and to.
        #' @param transition Optional numeric transition, fraction string, or symbolic
        #'   expression.
        #' @param label Human-readable character label.
        #' @param expression Optional character expression for the stored weight or transition.
        #' @param value Optional numeric scalar supplying the stored numeric value.
        #' @param from Character identifier of the source hypothesis.
        #' @param to Character identifier of the target hypothesis.
        #'
        setEdgeTransition = function(
                edge = NULL,
                transition = NULL,
                label = NULL,
                expression = NULL,
                value = NULL,
                from = NULL,
                to = NULL) {
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
        #'
        #' @description
        #' Set a numeric transition between 0 and 1, removing an edge when its weight is zero.
        #'
        #' @param from Character identifier of the source hypothesis.
        #' @param to Character identifier of the target hypothesis.
        #' @param weight Numeric weight, fraction string, or symbolic expression.
        #'
        setTransitionWeight = function(from, to, weight) {
            private$requireNodeIndex(from)
            private$requireNodeIndex(to)
            value <- self$parseNumber(weight, "Edge weight")
            if (value < 0 || value > 1) {
                stop("Edge weight must be between 0 and 1.", call. = FALSE)
            }
            if (identical(from, to)) {
                if (value != 0) {
                    stop("The transition matrix diagonal must be 0.", call. = FALSE)
                }
                return(invisible(self))
            }

            index <- private$edgeIndex(from = from, to = to)
            if (value == 0) {
                if (!is.na(index)) {
                    private$data$edges <- private$data$edges[-index]
                    private$refreshReciprocalEdgeSmooth()
                }
            } else if (is.na(index)) {
                self$addEdge(from = from, to = to, transition = value)
            } else {
                self$setEdgeTransition(edge = private$data$edges[[index]]$id,
                                       transition = value)
            }
            invisible(self)
        },
        #'
        #' @description
        #' Set the presentation color of an existing edge.
        #'
        #' @param edge Optional character edge identifier; otherwise use from and to.
        #' @param color Optional visNetwork color specification.
        #' @param from Character identifier of the source hypothesis.
        #' @param to Character identifier of the target hypothesis.
        #'
        setEdgeColor = function(edge = NULL, color = NULL, from = NULL, to = NULL) {
            index <- private$requireEdgeIndex(edge = edge, from = from, to = to)
            private$data$edges[[index]]$vis$color <- color
            invisible(self)
        },
        #'
        #' @description
        #' Set the edge label font size.
        #'
        #' @param edge Optional character edge identifier; otherwise use from and to.
        #' @param size Numeric font size in pixels, or NULL to clear the stored size.
        #' @param from Character identifier of the source hypothesis.
        #' @param to Character identifier of the target hypothesis.
        #'
        setEdgeFontSize = function(edge = NULL, size, from = NULL, to = NULL) {
            index <- private$requireEdgeIndex(edge = edge, from = from, to = to)
            font <- private$data$edges[[index]]$vis$font %||% list()
            font$size <- private$asNullableNumber(size)
            private$data$edges[[index]]$vis$font <- font
            invisible(self)
        },
        #'
        #' @description
        #' Set the edge smoothing specification.
        #'
        #' @param edge Optional character edge identifier; otherwise use from and to.
        #' @param smooth Logical or list specifying visNetwork edge smoothing.
        #' @param from Character identifier of the source hypothesis.
        #' @param to Character identifier of the target hypothesis.
        #'
        setEdgeSmooth = function(edge = NULL, smooth, from = NULL, to = NULL) {
            index <- private$requireEdgeIndex(edge = edge, from = from, to = to)
            private$data$edges[[index]]$vis$smooth <- smooth
            invisible(self)
        },
        #'
        #' @description
        #' Merge named presentation properties from ... into the edge vis list.
        #'
        #' @param edge Optional character edge identifier; otherwise use from and to.
        #' @param from Character identifier of the source hypothesis.
        #' @param to Character identifier of the target hypothesis.
        #' @param ... Named arguments passed to the operation described above.
        #'
        setEdgeVis = function(edge = NULL, from = NULL, to = NULL, ...) {
            index <- private$requireEdgeIndex(edge = edge, from = from, to = to)
            private$data$edges[[index]]$vis <- private$mergeLists(
                private$data$edges[[index]]$vis %||% list(),
                list(...)
            )
            invisible(self)
        },
        #'
        #' @description
        #' Merge named options from ... into the graph network settings.
        #'
        #' @param ... Named arguments passed to the operation described above.
        #'
        setVisOptions = function(...) {
            private$data$settings$vis <- private$mergeLists(
                private$data$settings$vis %||% list(),
                list(...)
            )
            invisible(self)
        },
        #'
        #' @description
        #' Return the current number-format settings as a named list.
        #'
        numberFormat = function() {
            private$data$settings$numbers %||% private$defaultSettings()$numbers
        },
        #'
        #' @description
        #' Update non-NULL number-format settings and regenerate numeric display labels.
        #'
        #' @param fractions Optional logical enabling fraction display; NULL keeps the current
        #'   setting.
        #' @param maxDenominator Optional positive integer limiting displayed fraction
        #'   denominators.
        #' @param tolerance Numeric tolerance for the checks or approximation performed by this
        #'   method.
        #' @param digits Optional positive integer giving significant digits for decimal
        #'   display.
        #'
        setNumberFormat = function(
                fractions = NULL,
                maxDenominator = NULL,
                tolerance = NULL,
                digits = NULL) {
            settings <- self$numberFormat()
            if (!is.null(fractions)) {
                settings$enabled <- isTRUE(fractions)
            }
            if (!is.null(maxDenominator)) {
                settings$maxDenominator <- max(1L, as.integer(maxDenominator))
            }
            if (!is.null(tolerance)) {
                settings$tolerance <- max(0, as.numeric(tolerance))
            }
            if (!is.null(digits)) {
                settings$digits <- max(1L, as.integer(digits))
            }
            private$data$settings$numbers <- settings
            private$refreshNumericLabels()
            invisible(self)
        },
        #'
        #' @description
        #' Parse one finite decimal number or fraction string.
        #'
        #' @param x Numeric scalar or fraction string to parse or format.
        #' @param name Character name used in validation errors; defaults to "value".
        #'
        parseNumber = function(x, name = "value") {
            alphaGraphParseNumber(x, name = name)
        },
        #'
        #' @description
        #' Format a number using the graph number-format settings.
        #'
        #' @param x Numeric scalar or fraction string to parse or format.
        #'
        formatNumber = function(x) {
            private$renderNumber(x)
        },
        #'
        #' @description
        #' Replace the graph from a canonical list and normalize stored settings.
        #'
        #' @param x Canonical JSON-compatible list.
        #' @param validate Logical; whether to validate the imported, returned, or modified
        #'   object.
        #'
        loadList = function(x, validate = TRUE) {
            if (!is.list(x)) {
                stop("x must be a list.", call. = FALSE)
            }
            private$data <- private$normalizeJsonNumbers(
                private$mergeLists(private$emptyData(), x)
            )
            private$data$version <- as.integer(private$data$version)
            private$data$settings <- private$asObjectList(
                private$data$settings,
                "settings"
            )
            numberSettings <- private$mergeLists(
                private$defaultSettings()$numbers,
                private$data$settings$numbers %||% list()
            )
            numberSettings$maxDenominator <- as.integer(numberSettings$maxDenominator)
            numberSettings$digits <- as.integer(numberSettings$digits)
            numberSettings$tolerance <- as.numeric(numberSettings$tolerance)
            numberSettings$enabled <- isTRUE(numberSettings$enabled)
            private$data$settings$numbers <- numberSettings
            private$data$metadata <- private$asObjectList(
                private$data$metadata,
                "metadata"
            )
            private$data$nodes <- lapply(private$data$nodes, function(node) {
                node$vis <- private$asObjectList(node$vis %||% list(), "node vis")
                node$metadata <- private$asObjectList(
                    node$metadata %||% list(),
                    "node metadata"
                )
                node
            })
            private$data$edges <- lapply(private$data$edges, function(edge) {
                edge$vis <- private$asObjectList(edge$vis %||% list(), "edge vis")
                edge$metadata <- private$asObjectList(
                    edge$metadata %||% list(),
                    "edge metadata"
                )
                edge
            })
            if (isTRUE(validate)) {
                self$validate(requireNumeric = FALSE)
            }
            invisible(self)
        },
        #'
        #' @description
        #' Update transitions while preserving canonical hypothesis order.
        #'
        #' @param transitions Square transition matrix or data frame in hypothesis order.
        #' @param type Character choice of "value", "expression", or "display" representation.
        #' @param nodeKeys Character choice of "id" or "label" used to match matrix dimnames.
        #' @param removeZero Logical; whether zero cells remove existing edges. Defaults to
        #'   TRUE.
        #' @param validate Logical; whether to validate the imported, returned, or modified
        #'   object.
        #'
        updateFromTransitionMatrix = function(
                transitions,
                type = c("expression", "display", "value"),
                nodeKeys = c("id", "label"),
                removeZero = TRUE,
                validate = TRUE) {
            type <- match.arg(type)
            nodeKeys <- match.arg(nodeKeys)
            mat <- private$normalizeTransitionMatrix(transitions, nodeKeys)
            nodeIds <- vapply(private$data$nodes, function(node) node$id, character(1L))

            for (i in seq_len(nrow(mat))) {
                for (j in seq_len(ncol(mat))) {
                    value <- mat[i, j]
                    isZero <- private$isZeroTransition(value)
                    if (i == j) {
                        if (!isZero) {
                            stop("The transition matrix diagonal must be 0.", call. = FALSE)
                        }
                        next
                    }

                    from <- nodeIds[i]
                    to <- nodeIds[j]
                    index <- private$edgeIndex(from = from, to = to)
                    if (isZero) {
                        if (isTRUE(removeZero) && !is.na(index)) {
                            private$data$edges <- private$data$edges[-index]
                        }
                        next
                    }

                    if (is.na(index)) {
                        args <- private$transitionArguments(value, type)
                        do.call(
                            self$addEdge,
                            c(list(from = from, to = to), args)
                        )
                    } else if (identical(type, "display")) {
                        self$setEdgeLabel(
                            edge = private$data$edges[[index]]$id,
                            label = as.character(value)
                        )
                    } else {
                        args <- private$transitionArguments(value, type)
                        do.call(
                            self$setEdgeTransition,
                            c(list(edge = private$data$edges[[index]]$id), args)
                        )
                    }
                }
            }
            private$refreshReciprocalEdgeSmooth()

            if (isTRUE(validate)) {
                self$validate(requireNumeric = identical(type, "value"))
            }
            invisible(self)
        },
        #'
        #' @description
        #' Merge network data into the canonical graph using stable identifiers.
        #'
        #' @param nodes visNetwork node data frame containing stable node identifiers.
        #' @param edges visNetwork edge data frame containing from and to identifiers.
        #' @param removeMissing Logical; whether nodes and edges absent from the input are
        #'   removed.
        #' @param updateCanonicalLabels Logical; whether network labels also replace canonical
        #'   labels. Defaults to FALSE.
        #' @param validate Logical; whether to validate the imported, returned, or modified
        #'   object.
        #'
        updateFromVisNetworkData = function(
                nodes,
                edges,
                removeMissing = TRUE,
                updateCanonicalLabels = FALSE,
                validate = TRUE) {
            nodeRecords <- private$dataFrameToRecords(nodes, "nodes")
            edgeRecords <- private$dataFrameToRecords(edges, "edges")
            nodeIds <- vapply(nodeRecords, function(node) as.character(node$id), character(1L))
            if (anyDuplicated(nodeIds)) {
                stop("visNetwork node ids must be unique.", call. = FALSE)
            }

            if (isTRUE(removeMissing)) {
                currentIds <- vapply(private$data$nodes, function(node) node$id, character(1L))
                for (id in setdiff(currentIds, nodeIds)) {
                    self$removeNode(id)
                }
            }

            for (record in nodeRecords) {
                id <- as.character(record$id)
                vis <- private$unflattenVisRecord(record, reserved = "id")
                index <- private$nodeIndex(id)
                if (is.na(index)) {
                    self$addNode(
                        id = id,
                        label = as.character(record$label %||% id),
                        weight = 0,
                        vis = vis
                    )
                } else {
                    private$data$nodes[[index]]$vis <- private$mergeLists(
                        private$data$nodes[[index]]$vis %||% list(),
                        vis
                    )
                    if (isTRUE(updateCanonicalLabels) && !is.null(record$label)) {
                        private$data$nodes[[index]]$label <- as.character(record$label)
                    }
                }
            }

            edgePairs <- vapply(
                edgeRecords,
                function(edge) paste(edge$from, edge$to, sep = "\r"),
                character(1L)
            )
            if (anyDuplicated(edgePairs)) {
                stop("Only one visNetwork edge per ordered node pair is supported.",
                    call. = FALSE
                )
            }
            if (isTRUE(removeMissing)) {
                currentPairs <- vapply(
                    private$data$edges,
                    function(edge) paste(edge$from, edge$to, sep = "\r"),
                    character(1L)
                )
                remove <- which(!currentPairs %in% edgePairs)
                if (length(remove) > 0L) {
                    private$data$edges <- private$data$edges[-remove]
                }
            }

            for (record in edgeRecords) {
                from <- as.character(record$from)
                to <- as.character(record$to)
                private$requireNodeIndex(from)
                private$requireNodeIndex(to)
                vis <- private$unflattenVisRecord(
                    record,
                    reserved = c("id", "from", "to")
                )
                index <- private$edgeIndex(from = from, to = to)
                if (is.na(index)) {
                    transition <- record$label %||% 0
                    self$addEdge(
                        from = from,
                        to = to,
                        transition = transition,
                        id = as.character(record$id %||% private$edgeId(from, to)),
                        vis = vis
                    )
                } else {
                    private$data$edges[[index]]$vis <- private$mergeLists(
                        private$data$edges[[index]]$vis %||% list(),
                        vis
                    )
                }
            }

            if (isTRUE(validate)) {
                self$validate(requireNumeric = FALSE)
            }
            invisible(self)
        },
        #'
        #' @description
        #' Load a canonical graph using jsonlite::fromJSON().
        #'
        #' @param json JSON text, connection, or file path accepted by jsonlite::fromJSON().
        #' @param validate Logical; whether to validate the imported, returned, or modified
        #'   object.
        #'
        fromJson = function(json, validate = TRUE) {
            if (!requireNamespace("jsonlite", quietly = TRUE)) {
                stop("Package 'jsonlite' is required to read graph JSON.", call. = FALSE)
            }
            self$loadList(jsonlite::fromJSON(json, simplifyVector = FALSE), validate = validate)
            invisible(self)
        },
        #'
        #' @description
        #' Return the JSON-compatible canonical graph list.
        #'
        toList = function() {
            private$data
        },
        #'
        #' @description
        #' Serialize the canonical graph with jsonlite::toJSON(); ... supplies additional JSON
        #'   options.
        #'
        #' @param pretty Logical; whether to indent JSON output. Defaults to TRUE.
        #' @param auto_unbox Logical; whether to unbox JSON scalars. Defaults to TRUE.
        #' @param null Character NULL encoding passed to jsonlite::toJSON(); defaults to
        #'   "null".
        #' @param ... Named arguments passed to the operation described above.
        #'
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
        #'
        #' @description
        #' Compatibility alias forwarding ... to toJson().
        #'
        #' @param ... Named arguments passed to the operation described above.
        #'
        toJSON = function(...) {
            self$toJson(...)
        },
        #'
        #' @description
        #' Compatibility alias forwarding ... to toJson().
        #'
        #' @param ... Named arguments passed to the operation described above.
        #'
        toJason = function(...) {
            self$toJson(...)
        },
        #'
        #' @description
        #' Return an n by n transition matrix or data frame in canonical node order.
        #'
        #' @param type Character choice of "value", "expression", or "display" representation.
        #' @param parameters Named list of numeric parameter overrides for expression
        #'   evaluation.
        #' @param asDataFrame Logical; return a data frame when TRUE, a matrix otherwise.
        #' @param rowNames Character choice of "label" or "id" for row and column names.
        #' @param zero Optional fill value for missing transitions; defaults to numeric or
        #'   character zero.
        #' @param validate Logical; whether to validate the imported, returned, or modified
        #'   object.
        #'
        toTransitionMatrix = function(
                type = c("display", "value", "expression"),
                parameters = list(),
                asDataFrame = TRUE,
                rowNames = c("label", "id"),
                zero = NULL,
                validate = FALSE) {
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
        #'
        #' @description
        #' Return a weight vector in canonical node order, named by stable hypothesis IDs.
        #'
        #' @param type Character choice of "value", "expression", or "display" representation.
        #' @param parameters Named list of numeric parameter overrides for expression
        #'   evaluation.
        #' @param validate Logical; whether to validate the imported, returned, or modified
        #'   object.
        #'
        toHypotheses = function(
                type = c("value", "display", "expression"),
                parameters = list(),
                validate = FALSE) {
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
        #'
        #' @description
        #' Return a list of hypotheses, transitions, and hyp_names with matching names and
        #'   dimensions.
        #'
        #' @param parameters Named list of numeric parameter overrides for expression
        #'   evaluation.
        #' @param validate Logical; whether to validate the imported, returned, or modified
        #'   object.
        #' @param hypNames Character choice of "id" or "label" for unique engine hypothesis
        #'   names.
        #'
        toGraphicalMCPInput = function(
                parameters = list(),
                validate = TRUE,
                hypNames = c("id", "label")) {
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
            if (anyDuplicated(names)) {
                stop("Hypothesis names passed to graphicalMCP must be unique.",
                    call. = FALSE
                )
            }
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
        #'
        #' @description
        #' Create a graphicalMCP initial_graph from numeric canonical inputs.
        #'
        #' @param parameters Named list of numeric parameter overrides for expression
        #'   evaluation.
        #' @param validate Logical; whether to validate the imported, returned, or modified
        #'   object.
        #' @param hypNames Character choice of "id" or "label" for unique engine hypothesis
        #'   names.
        #'
        toGraphicalMCP = function(
                parameters = list(),
                validate = TRUE,
                hypNames = c("id", "label")) {
            if (!requireNamespace("graphicalMCP", quietly = TRUE)) {
                stop("Package 'graphicalMCP' is required to create a graphicalMCP graph.",
                    call. = FALSE
                )
            }
            input <- self$toGraphicalMCPInput(
                parameters = parameters,
                validate = validate,
                hypNames = hypNames
            )
            graphicalMCP::graph_create(
                hypotheses = unname(input$hypotheses),
                transitions = unname(input$transitions),
                hyp_names = input$hyp_names
            )
        },
        #'
        #' @description
        #' Return a list containing nodes and edges data frames for visNetwork.
        #'
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
        #'
        #' @description
        #' Return a visNetwork htmlwidget using the stored graph presentation.
        #'
        #' @param width Optional widget width; falls back to graph settings and then "100%".
        #' @param height Optional widget height; falls back to graph settings and then "400px".
        #' @param applyDefaults Logical; whether to apply saved network settings. Defaults to
        #'   TRUE.
        #'
        toVisNetwork = function(
                width = NULL,
                height = NULL,
                applyDefaults = TRUE) {
            if (!requireNamespace("visNetwork", quietly = TRUE)) {
                stop("Package 'visNetwork' is required to render the graph.",
                    call. = FALSE
                )
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
        #'
        #' @description
        #' Validate graph structure and optionally numeric weights and transitions; return TRUE
        #'   on success.
        #'
        #' @param requireNumeric Logical; whether weights and transitions must be numerically
        #'   computable.
        #' @param parameters Named list of numeric parameter overrides for expression
        #'   evaluation.
        #' @param tolerance Numeric validation tolerance; defaults to
        #'   sqrt(.Machine$double.eps).
        #'
        validate = function(
                requireNumeric = FALSE,
                parameters = list(),
                tolerance = sqrt(.Machine$double.eps)) {
            if (!is.list(private$data)) {
                stop("Graph data must be an object.", call. = FALSE)
            }
            if (!identical(private$data$format, "alphagraph.graph")) {
                stop("Unsupported graph format.", call. = FALSE)
            }
            if (!identical(as.integer(private$data$version), 1L)) {
                stop("Unsupported alphagraph graph version.", call. = FALSE)
            }
            private$assertScalarCharacter(private$data$id, "Graph id")
            if (!is.character(private$data$label) || length(private$data$label) != 1L) {
                stop("Graph label must be a character scalar.", call. = FALSE)
            }
            if (!is.null(private$data$description) &&
                    (!is.character(private$data$description) ||
                        length(private$data$description) != 1L)) {
                stop("Graph description must be null or a character scalar.", call. = FALSE)
            }
            if (!is.list(private$data$parameters) || !is.list(private$data$nodes) ||
                    !is.list(private$data$edges) || !is.list(private$data$settings) ||
                    !is.list(private$data$metadata)) {
                stop("Graph collections, settings, and metadata must be objects or arrays.",
                    call. = FALSE
                )
            }

            for (parameter in private$data$parameters) {
                private$assertScalarCharacter(parameter$id, "Parameter id")
                if (!is.character(parameter$label) || length(parameter$label) != 1L) {
                    stop("Parameter labels must be character scalars.", call. = FALSE)
                }
                if (!is.null(parameter$value) &&
                        (length(parameter$value) != 1L || !is.numeric(parameter$value) ||
                            !is.finite(parameter$value))) {
                    stop("Parameter values must be null or finite numeric scalars.",
                        call. = FALSE
                    )
                }
            }

            parameterIds <- vapply(
                private$data$parameters,
                function(parameter) parameter$id,
                character(1L)
            )
            if (any(!nzchar(parameterIds)) || anyDuplicated(parameterIds)) {
                stop("Parameter ids must be unique and non-empty.", call. = FALSE)
            }
            nodeIds <- vapply(private$data$nodes, function(node) node$id, character(1L))
            if (any(!nzchar(nodeIds))) {
                stop("All nodes must have non-empty ids.", call. = FALSE)
            }
            if (anyDuplicated(nodeIds)) {
                stop("Node ids must be unique.", call. = FALSE)
            }
            nodeLabels <- vapply(
                private$data$nodes,
                function(node) node$label,
                character(1L)
            )
            alphaGraphValidateHypothesisLabels(nodeLabels)

            for (node in private$data$nodes) {
                private$validateValueSpec(node$weight, "Node weight")
                if (!is.list(node$vis) || !is.list(node$metadata)) {
                    stop("Node vis and metadata fields must be objects.", call. = FALSE)
                }
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
                private$validateValueSpec(edge$transition, "Edge transition")
                if (!is.list(edge$vis) || !is.list(edge$metadata)) {
                    stop("Edge vis and metadata fields must be objects.", call. = FALSE)
                }
                if (!edge$from %in% nodeIds) {
                    stop("Edge '", edge$id, "' references unknown source node '",
                        edge$from, "'.",
                        call. = FALSE
                    )
                }
                if (!edge$to %in% nodeIds) {
                    stop("Edge '", edge$id, "' references unknown target node '",
                        edge$to, "'.",
                        call. = FALSE
                    )
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
                metadata = setNames(list(), character())
            )
        },
        defaultSettings = function() {
            list(
                numbers = list(
                    enabled = TRUE,
                    maxDenominator = 16L,
                    tolerance = 1e-8,
                    digits = 8L
                ),
                vis = list(
                    width = "100%",
                    height = "400px",
                    edges = list(
                        arrows = "to",
                        smooth = list(type = "curvedCW", roundness = 0.3)
                    ),
                    nodes = list(
                        shape = "circle",
                        borderWidth = 1,
                        color = list(background = "white", border = "black"),
                        # Change this value to adjust the inner space between
                        # node labels and their border (in pixels).
                        margin = 6,
                        font = list(align = "center"),
                        scaling = list(
                            label = list(enabled = TRUE, min = 10, max = 16)
                        )
                    ),
                    interaction = list(
                        dragNodes = TRUE,
                        dragView = FALSE,
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
        valueSpec = function(
                input = NULL,
                display = NULL,
                expression = NULL,
                value = NULL,
                current = NULL,
                defaultDisplay = NULL) {
            spec <- current %||% list(display = NULL, expression = NULL, value = NULL)

            oldExpression <- spec$expression

            if (!is.null(input)) {
                if (is.numeric(input) || is.logical(input)) {
                    value <- input
                    if (is.null(display)) {
                        display <- private$renderNumber(input)
                    }
                    if (is.null(expression)) {
                        expression <- private$renderNumber(input)
                    }
                } else {
                    parsed <- alphaGraphTryParseNumber(input)
                    if (!is.null(parsed)) {
                        value <- parsed
                        if (is.null(display)) {
                            display <- private$renderNumber(parsed)
                        }
                        if (is.null(expression)) {
                            expression <- private$renderNumber(parsed)
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
                spec["value"] <- list(private$asNullableNumber(value))
            } else if (!identical(oldExpression, spec$expression)) {
                spec["value"] <- list(private$tryParseNumber(spec$expression))
            } else if (is.null(spec$value)) {
                spec["value"] <- list(private$tryParseNumber(spec$expression))
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

            parsed <- tryCatch(
                parse(text = expression),
                error = function(e) NA_real_
            )
            if (!is.expression(parsed) || length(parsed) != 1L) {
                stop("Expression '", expression, "' is not a single valid expression.",
                    call. = FALSE
                )
            }
            allowedNames <- ls(env, all.names = TRUE)
            private$assertSafeExpression(parsed[[1L]], allowedNames)
            value <- tryCatch(
                eval(parsed[[1L]], envir = env),
                error = function(e) NA_real_
            )
            if (!is.numeric(value) || length(value) != 1L || is.na(value)) {
                stop("Expression '", expression, "' could not be evaluated to a numeric scalar.",
                    call. = FALSE
                )
            }
            as.numeric(value)
        },
        assertSafeExpression = function(node, allowedNames) {
            if (is.numeric(node)) {
                return(invisible(TRUE))
            }
            if (is.name(node)) {
                name <- as.character(node)
                if (!name %in% allowedNames) {
                    stop("Unknown parameter '", name, "' in graph expression.",
                        call. = FALSE
                    )
                }
                return(invisible(TRUE))
            }
            if (!is.call(node)) {
                stop("Graph expressions may contain only numbers, parameters, and arithmetic.",
                    call. = FALSE
                )
            }
            operator <- as.character(node[[1L]])
            if (!operator %in% c("+", "-", "*", "/", "^", "(")) {
                stop("Operator or function '", operator,
                    "' is not allowed in graph expressions.",
                    call. = FALSE
                )
            }
            arguments <- as.list(node)[-1L]
            if (operator == "(" && length(arguments) != 1L) {
                stop("Invalid parentheses in graph expression.", call. = FALSE)
            }
            if (operator %in% c("*", "/", "^") && length(arguments) != 2L) {
                stop("Operator '", operator, "' requires two operands.", call. = FALSE)
            }
            if (operator %in% c("+", "-") && !length(arguments) %in% c(1L, 2L)) {
                stop("Operator '", operator, "' requires one or two operands.",
                    call. = FALSE
                )
            }
            for (argument in arguments) {
                private$assertSafeExpression(argument, allowedNames)
            }
            invisible(TRUE)
        },
        nodeVisLabel = function(label, weightSpec) {
            weightLabel <- weightSpec$display %||% private$renderNumber(weightSpec$value) %||% "0"
            paste0(alphaGraphRenderHypothesisLabel(label), "\n", weightLabel)
        },
        refreshNumericLabels = function() {
            for (i in seq_along(private$data$nodes)) {
                spec <- private$data$nodes[[i]]$weight
                if (!is.null(spec$value)) {
                    formatted <- private$renderNumber(spec$value)
                    spec$display <- formatted
                    spec$expression <- formatted
                    private$data$nodes[[i]]$weight <- spec
                    private$data$nodes[[i]]$vis$label <- private$nodeVisLabel(
                        private$data$nodes[[i]]$label,
                        spec
                    )
                }
            }
            for (i in seq_along(private$data$edges)) {
                spec <- private$data$edges[[i]]$transition
                if (!is.null(spec$value)) {
                    formatted <- private$renderNumber(spec$value)
                    spec$display <- formatted
                    spec$expression <- formatted
                    private$data$edges[[i]]$transition <- spec
                    private$data$edges[[i]]$vis$label <- formatted
                }
            }
            invisible(NULL)
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
        flattenVisRecord = function(record, prefix = NULL) {
            out <- list()
            for (name in names(record)) {
                value <- record[[name]]
                if (is.null(value)) {
                    next
                }
                key <- if (is.null(prefix)) name else paste(prefix, name, sep = ".")
                if (is.list(value) && !is.data.frame(value) && !is.null(names(value))) {
                    out <- c(out, private$flattenVisRecord(value, prefix = key))
                } else {
                    out[[key]] <- value
                }
            }
            out
        },
        unflattenVisRecord = function(record, reserved = character()) {
            out <- list()
            for (name in setdiff(names(record), reserved)) {
                value <- record[[name]]
                if (length(value) == 0L || (length(value) == 1L && is.na(value))) {
                    next
                }
                path <- strsplit(name, ".", fixed = TRUE)[[1L]]
                out <- private$setNestedValue(out, path, value)
            }
            out
        },
        setNestedValue = function(x, path, value) {
            name <- path[[1L]]
            if (length(path) == 1L) {
                x[[name]] <- value
                return(x)
            }
            child <- x[[name]] %||% list()
            x[[name]] <- private$setNestedValue(child, path[-1L], value)
            x
        },
        dataFrameToRecords = function(x, name) {
            if (!is.data.frame(x)) {
                stop(name, " must be a data.frame.", call. = FALSE)
            }
            required <- if (identical(name, "nodes")) "id" else c("from", "to")
            missing <- setdiff(required, names(x))
            if (length(missing) > 0L) {
                stop(name, " must contain columns: ", paste(required, collapse = ", "),
                    ".",
                    call. = FALSE
                )
            }
            lapply(seq_len(nrow(x)), function(i) {
                lapply(x, function(column) {
                    value <- column[[i]]
                    if (is.factor(value)) as.character(value) else value
                })
            })
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
            if (is.null(names(y))) {
                return(y)
            }
            for (name in names(y)) {
                if (!is.null(x[[name]]) && is.list(x[[name]]) && is.list(y[[name]]) &&
                        !is.data.frame(x[[name]]) && !is.data.frame(y[[name]])) {
                    x[[name]] <- private$mergeLists(x[[name]], y[[name]])
                } else {
                    x[name] <- list(y[[name]])
                }
            }
            x
        },
        normalizeJsonNumbers = function(x) {
            if (is.integer(x)) {
                return(as.numeric(x))
            }
            if (is.list(x)) {
                return(lapply(x, private$normalizeJsonNumbers))
            }
            x
        },
        asObjectList = function(x, name) {
            if (!is.list(x)) {
                stop(name, " must be a list.", call. = FALSE)
            }
            if (length(x) == 0L) {
                return(setNames(list(), character()))
            }
            if (is.null(names(x)) || any(!nzchar(names(x)))) {
                stop(name, " must be a named list.", call. = FALSE)
            }
            x
        },
        normalizeTransitionMatrix = function(transitions, nodeKeys) {
            if (is.data.frame(transitions)) {
                transitions <- as.matrix(transitions)
            }
            if (!is.matrix(transitions) || nrow(transitions) != ncol(transitions)) {
                stop("transitions must be a square matrix or data.frame.", call. = FALSE)
            }
            keys <- vapply(
                private$data$nodes,
                function(node) as.character(node[[nodeKeys]]),
                character(1L)
            )
            if (nrow(transitions) != length(keys)) {
                stop("Transition matrix dimensions must match the number of nodes.",
                    call. = FALSE
                )
            }
            rn <- rownames(transitions)
            cn <- colnames(transitions)
            if (!is.null(rn) || !is.null(cn)) {
                if (is.null(rn) || is.null(cn) || !setequal(rn, keys) || !setequal(cn, keys)) {
                    stop("Transition matrix row and column names must match node ",
                        nodeKeys, "s.",
                        call. = FALSE
                    )
                }
                transitions <- transitions[keys, keys, drop = FALSE]
            }
            transitions
        },
        isZeroTransition = function(x) {
            if (length(x) == 0L || is.na(x)) {
                return(TRUE)
            }
            if (is.numeric(x) || is.logical(x)) {
                return(as.numeric(x) == 0)
            }
            text <- trimws(as.character(x))
            if (!nzchar(text)) {
                return(TRUE)
            }
            number <- suppressWarnings(as.numeric(text))
            !is.na(number) && number == 0
        },
        transitionArguments = function(value, type) {
            if (identical(type, "value")) {
                return(list(transition = as.numeric(value)))
            }
            if (identical(type, "expression")) {
                text <- as.character(value)
                return(list(transition = text, label = text, expression = text))
            }
            list(transition = as.character(value), label = as.character(value))
        },
        refreshReciprocalEdgeSmooth = function() {
            # A single directed edge is straight; both directions are curved
            # so reciprocal transitions remain visually distinguishable.
            for (i in seq_along(private$data$edges)) {
                edge <- private$data$edges[[i]]
                hasReverse <- any(vapply(
                    private$data$edges,
                    function(candidate) {
                        identical(candidate$from, edge$to) &&
                            identical(candidate$to, edge$from)
                    },
                    logical(1L)
                ))
                private$data$edges[[i]]$vis$smooth <- hasReverse
            }
            invisible(NULL)
        },
        assertScalarCharacter = function(x, name) {
            if (!is.character(x) || length(x) != 1L || !nzchar(x)) {
                stop(name, " must be a non-empty character scalar.", call. = FALSE)
            }
        },
        assertProbabilities = function(x, name, tolerance) {
            if (any(is.na(x)) || any(!is.finite(x))) {
                stop(name, " must be fully numeric before validation.", call. = FALSE)
            }
            if (any(x < -tolerance | x > 1 + tolerance)) {
                stop(name, " must be between 0 and 1.", call. = FALSE)
            }
        },
        validateValueSpec = function(spec, name) {
            if (!is.list(spec) ||
                    !all(c("display", "expression", "value") %in% names(spec))) {
                stop(name, " must contain display, expression, and value.",
                    call. = FALSE
                )
            }
            for (field in c("display", "expression")) {
                value <- spec[[field]]
                if (!is.null(value) && (!is.character(value) || length(value) != 1L)) {
                    stop(name, " ", field, " must be null or a character scalar.",
                        call. = FALSE
                    )
                }
            }
            if (!is.null(spec$value) &&
                    (length(spec$value) != 1L || !is.numeric(spec$value) ||
                        !is.finite(spec$value))) {
                stop(name, " value must be null or a finite numeric scalar.",
                    call. = FALSE
                )
            }
            invisible(TRUE)
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
            alphaGraphTryParseNumber(x)
        },
        renderNumber = function(x) {
            settings <- private$data$settings$numbers %||%
                private$defaultSettings()$numbers
            alphaGraphFormatNumber(
                x,
                fractions = settings$enabled,
                maxDenominator = settings$maxDenominator,
                tolerance = settings$tolerance,
                digits = settings$digits
            )
        }
    )
)

#'
#' @title
#' Create an AlphaGraph from external representations
#'
#' @description
#' Construct a canonical graph from a list, JSON, a transition matrix, a
#' graphicalMCP initial graph, or visNetwork node and edge data frames.
#'
#' @param x A JSON-compatible alphagraph list.
#' @param validate Whether to validate the imported model.
#'
#' @return An `AlphaGraph` object.
#'
#' @name alphaGraphConverters
#' @rdname alphaGraphConverters
#' @keywords internal
#'
alphaGraphFromList <- function(x, validate = TRUE) {
    graph <- AlphaGraph$new()
    graph$loadList(x, validate = validate)
    graph
}

#'
#' @param json JSON text, a connection, or a file path accepted by
#'   `jsonlite::fromJSON()`.
#' @param validate Whether to validate the imported model.
#'
#' @rdname alphaGraphConverters
#' @keywords internal
#'
alphaGraphFromJson <- function(json, validate = TRUE) {
    graph <- AlphaGraph$new()
    graph$fromJson(json, validate = validate)
    graph
}

#'
#' @param transitions Square transition matrix or data frame.
#' @param hypotheses Optional hypothesis weights, in matrix order.
#' @param type Interpretation of non-zero transition cells.
#' @param nodeIds Stable node identifiers. Defaults to matrix row names.
#' @param nodeLabels Display labels. Defaults to `nodeIds`.
#' @param id,label,description,metadata,settings Graph-level properties.
#' @param validate Whether to validate the resulting graph.
#'
#' @rdname alphaGraphConverters
#' @keywords internal
#'
alphaGraphFromTransitionMatrix <- function(
        transitions,
        hypotheses = NULL,
        type = c("value", "expression", "display"),
        nodeIds = NULL,
        nodeLabels = NULL,
        id = "graph",
        label = id,
        description = NULL,
        metadata = list(),
        settings = list(),
        validate = TRUE) {
    type <- match.arg(type)
    mat <- if (is.data.frame(transitions)) as.matrix(transitions) else transitions
    if (!is.matrix(mat) || nrow(mat) != ncol(mat)) {
        stop("transitions must be a square matrix or data.frame.", call. = FALSE)
    }
    n <- nrow(mat)
    if (is.null(nodeIds)) {
        nodeIds <- rownames(mat) %||% paste0("H", seq_len(n))
    }
    nodeIds <- as.character(nodeIds)
    if (length(nodeIds) != n || any(!nzchar(nodeIds)) || anyDuplicated(nodeIds)) {
        stop("nodeIds must contain one unique, non-empty id per matrix row.",
            call. = FALSE
        )
    }
    if (!is.null(rownames(mat)) && !is.null(colnames(mat))) {
        if (!setequal(rownames(mat), colnames(mat))) {
            stop("Transition matrix row and column names must contain the same nodes.",
                call. = FALSE
            )
        }
        mat <- mat[nodeIds, nodeIds, drop = FALSE]
    }
    nodeLabels <- as.character(nodeLabels %||% nodeIds)
    if (length(nodeLabels) != n) {
        stop("nodeLabels must contain one label per matrix row.", call. = FALSE)
    }
    hypotheses <- hypotheses %||% rep(0, n)
    if (!is.null(names(hypotheses))) {
        if (!setequal(names(hypotheses), nodeIds)) {
            stop("Named hypotheses must match nodeIds.", call. = FALSE)
        }
        hypotheses <- hypotheses[nodeIds]
    }
    if (length(hypotheses) != n) {
        stop("hypotheses must contain one weight per matrix row.", call. = FALSE)
    }

    graph <- AlphaGraph$new(
        id = id,
        label = label,
        description = description,
        metadata = metadata,
        settings = settings
    )
    for (i in seq_len(n)) {
        graph$addNode(id = nodeIds[i], label = nodeLabels[i], weight = hypotheses[i])
    }
    dimnames(mat) <- list(nodeIds, nodeIds)
    graph$updateFromTransitionMatrix(
        mat,
        type = type,
        nodeKeys = "id",
        validate = validate
    )
    graph
}

#'
#' @param graph An `initial_graph` object returned by
#'   `graphicalMCP::graph_create()` or an equivalent list with `hypotheses` and
#'   `transitions`.
#' @param id,label,description,metadata,settings Graph-level properties.
#' @param validate Whether to validate the resulting graph.
#'
#' @rdname alphaGraphConverters
#' @keywords internal
#'
alphaGraphFromGraphicalMCP <- function(
        graph,
        id = "graph",
        label = id,
        description = NULL,
        metadata = list(),
        settings = list(),
        validate = TRUE) {
    if (!is.list(graph) || is.null(graph$hypotheses) || is.null(graph$transitions)) {
        stop("graph must contain hypotheses and transitions.", call. = FALSE)
    }
    hypotheses <- graph$hypotheses
    transitions <- graph$transitions
    names <- names(hypotheses) %||% rownames(transitions)
    alphaGraphFromTransitionMatrix(
        transitions = transitions,
        hypotheses = hypotheses,
        type = "value",
        nodeIds = names,
        nodeLabels = names,
        id = id,
        label = label,
        description = description,
        metadata = metadata,
        settings = settings,
        validate = validate
    )
}

#'
#' @param nodes,edges Data frames accepted by `visNetwork::visNetwork()`.
#' @param id,label,description,metadata,settings Graph-level properties.
#' @param validate Whether to validate the resulting graph.
#'
#' @rdname alphaGraphConverters
#' @keywords internal
#'
alphaGraphFromVisNetworkData <- function(
        nodes,
        edges,
        id = "graph",
        label = id,
        description = NULL,
        metadata = list(),
        settings = list(),
        validate = TRUE) {
    graph <- AlphaGraph$new(
        id = id,
        label = label,
        description = description,
        metadata = metadata,
        settings = settings
    )
    graph$updateFromVisNetworkData(
        nodes = nodes,
        edges = edges,
        removeMissing = TRUE,
        updateCanonicalLabels = TRUE,
        validate = validate
    )
    graph
}

#'
#' @title
#' Create the current Hung and Wang demo graph as an AlphaGraph
#'
#' @description
#' Convenience constructor mirroring the current static example used in the
#' graph and transition matrix modules.
#'
#' @return An `AlphaGraph` object.
#'
#' @keywords internal
#'
createDemoAlphaGraph <- function() {
    graph <- AlphaGraph$new(
        id = "hung_wang_2010",
        label = "Hung and Wang (2010)"
    )

    graph$addNode(
        id = "H1_NI",
        label = getHypothesisLabel(1, "ni"), 
        weight = 1,
        x = 14, y = -5, 
        colorBackground = "#7CFF7C"
    )
    graph$addNode(
        id = "H1_S",
        label = getHypothesisLabel(1, "s"), 
        weight = 0,
        x = 294, y = -5
    )
    graph$addNode(
        id = "H2_NI",
        label = getHypothesisLabel(2, "ni"), 
        weight = 0,
        x = 14, y = 195
    )
    graph$addNode(
        id = "H2_S",
        label = getHypothesisLabel(2, "s"), 
        weight = 0,
        x = 294, y = 195
    )
    graph$addEdge("H1_NI", "H1_S", transition = 0.5)
    graph$addEdge("H1_NI", "H2_NI", transition = 0.5)
    graph$addEdge("H2_NI", "H1_S",
        transition = 0.5,
        smooth = TRUE
    )
    graph$addEdge("H1_S", "H2_NI",
        transition = 0.5,
        smooth = TRUE
    )
    graph$addEdge("H2_NI", "H2_S", transition = 0.5)
    graph$addEdge("H1_S", "H2_S", transition = 0.5)

    graph
}

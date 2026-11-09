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
#' @title Create a formatted hypothesis label
#'
#' @description
#' Generate an editable hypothesis label using alphagraph's underscore syntax.
#' The canonical value is converted to supported Unicode subscripts only when
#' rendered by visNetwork.
#'
#' @param number Integer indicating hypothesis index (0/1/2 etc.).
#' @param x Character short code describing the hypothesis (e.g. "ni", "s").
#' @param i Optional additional index or annotation appended on a new line.
#'
#' @return Character scalar such as `H1_(ni)`.
#'
#' @keywords internal
#' 
getHypothesisLabel <- function(number, x, i = NULL) {
    label <- paste0("H", as.character(number), "_(", as.character(x), ")")
    alphaGraphRenderHypothesisLabel(label)
    if (!is.null(i)) {
        label <- paste0(label, "\n", i)
    }
    label
}
#' 
#' @title Create a demo visNetwork graph for the UI
#'
#' @description
#' Build and return a visNetwork object illustrating a small example
#' transition graph. The function is intended for use in the UI module
#' as a demonstration / initial view.
#'
#' @param ns Shiny namespace function (usually obtained via session$ns).
#' @param graph Canonical `AlphaGraph` to render.
#'
#' @return A visNetwork object ready to be rendered.
#'
#' @keywords internal
#' 
getDemoGraph <- function(ns, graph = createDemoAlphaGraph()) {
    grid <- graph$toList()$settings$grid %||% list()
    grid <- list(
        size = grid$size %||% 40,
        origin = list(x = grid$origin$x %||% 14, y = grid$origin$y %||% -5)
    )
    validCoordinate <- function(value) {
        is.numeric(value) && length(value) == 1L && is.finite(value)
    }
    if (!validCoordinate(grid$size) || grid$size <= 0 ||
            !validCoordinate(grid$origin$x) || !validCoordinate(grid$origin$y)) {
        stop("Grid size must be positive and grid origins must be finite numbers.", call. = FALSE)
    }
    gridJson <- as.character(jsonlite::toJSON(grid, auto_unbox = TRUE, digits = NA))
    positionInputJson <- as.character(jsonlite::toJSON(ns("node_positions"), auto_unbox = TRUE))
    widget <- graph$toVisNetwork(height = "400px")
    widget$dependencies <- c(widget$dependencies, list(htmltools::htmlDependency(
        name = "alphagraph-grid", version = "1.0.0",
        src = c(file = getSystemFile("www/js")), script = "graph-grid.js"
    )))
    widget |>
        visNetwork::visEvents(
            initRedraw = sprintf("function() {
                window.alphaGraphGrid.beforeRedraw(this, %s, %s);
            }", gridJson, positionInputJson),
            dragging = sprintf("function() {
                window.alphaGraphGrid.beforeRedraw(this, %s, %s);
            }", gridJson, positionInputJson)
        ) |>
        visNetwork::visEvents(
            beforeDrawing = paste0("
                function(ctx) {
                    window.alphaGraphGrid.draw(ctx, this, ", gridJson, ");
                    // vis-network positions labels in circle nodes relative to
                    // the circle's left edge. When a short multiline label makes
                    // the circle height-driven, that places the label too far
                    // left. Keep the public circle sizing behavior, but draw its
                    // label at the actual node center.
                    var nodes = this.body.nodes;
                    Object.keys(nodes).forEach(function(id) {
                        var node = nodes[id];
                        var labelModule = node.shape && node.shape.labelModule;
                        if (!labelModule || node.options.shape !== 'circle' ||
                                labelModule.__alphaGraphCentered) {
                            return;
                        }

                        var originalDraw = labelModule.draw;
                        labelModule.draw = function(
                                context, x, y, selected, hover, baseline) {
                            return originalDraw.call(
                                labelModule,
                                context,
                                node.x,
                                y,
                                selected,
                                hover,
                                baseline
                            );
                        };
                        labelModule.__alphaGraphCentered = true;
                    });
                }
            ")
        ) |>
        visNetwork::visEvents(
            dragEnd = sprintf("function(params) {
                window.alphaGraphGrid.dragEnd(this, %s, %s, params.nodes);
            }", gridJson, positionInputJson)
        ) |>
        visNetwork::visEvents(
            selectNode = paste0("
                function(params) {
                    Shiny.setInputValue('", ns("selection"), "', {
                        nodes: params.nodes || [], edges: [], nonce: Math.random()
                    }, {priority: 'event'});
                }
            "),
            selectEdge = paste0("
                function(params) {
                    Shiny.setInputValue('", ns("selection"), "', {
                        nodes: params.nodes || [], edges: params.edges || [], nonce: Math.random()
                    }, {priority: 'event'});
                }
            "),
            deselectNode = paste0("
                function(params) {
                    Shiny.setInputValue('", ns("selection"), "', {
                        nodes: params.nodes || [], edges: params.edges || [], nonce: Math.random()
                    }, {priority: 'event'});
                }
            "),
            deselectEdge = paste0("
                function(params) {
                    Shiny.setInputValue('", ns("selection"), "', {
                        nodes: params.nodes || [], edges: params.edges || [], nonce: Math.random()
                    }, {priority: 'event'});
                }
            ")
        ) |>
        visNetwork::visEvents(
            # Right click / context menu
            oncontext = paste0("
                function(params) {
                params.event.preventDefault(); // disable browser context menu
                
                var nodeId = this.getNodeAt(params.pointer.DOM);
                if (nodeId) {
                Shiny.setInputValue('", ns("node_right_click"), "', {
                id: nodeId,
                x: params.event.clientX,
                y: params.event.clientY,
                nonce: Math.random()
                }, {priority: 'event'});
                } else {
                // clicked on empty space -> close any open menu
                Shiny.setInputValue('", ns("ctx_close"), "', Math.random(), {priority: 'event'});
                }
                }
                ")
        )
#        visNetwork::visEvents(
#            # Right-click handler (vis.js event: "oncontext")
#            oncontext = "
#                function(params) {
#                    params.event.preventDefault(); // suppress browser context menu
#                    
#                    var nodeId = this.getNodeAt(params.pointer.DOM);
#                    if (nodeId) {
#                    	Shiny.setInputValue('node_right_click',
#                    		{ id: nodeId, x: params.pointer.DOM.x, y: params.pointer.DOM.y, nonce: Math.random() },
#                    		{ priority: 'event' }
#                    	);
#                	}
#                }
#                "
#        )
}

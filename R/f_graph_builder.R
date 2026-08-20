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
    grid_size <- 40
    ox <- 14
    oy <- -5
    graph$toVisNetwork(height = "400px") |>
        visNetwork::visEvents(
            beforeDrawing = "
                function(ctx) {
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
            "
        ) |>
        visNetwork::visEvents(
            afterDrawing = sprintf("
                function(ctx) {
                // run only once after the first draw
                if (this.__alphagraphInitialSnapDone) return;
                this.__alphagraphInitialSnapDone = true;
                
                var grid = %d;
                var ox = %d;
                var oy = %d;
                
                var ids = this.body.data.nodes.getIds();
                var updates = [];
                
                for (var i = 0; i < ids.length; i++) {
                var id = ids[i];
                var n = this.body.nodes[id];
                if (!n) continue;
                
                var x = Math.round((n.x - ox) / grid) * grid + ox;
                var y = Math.round((n.y - oy) / grid) * grid + oy;
                
                updates.push({ id: id, x: x, y: y });
                }
                
                this.body.data.nodes.update(updates);
                
                // optional: fit view after snapping
                // this.fit();
                }
                ", grid_size, ox, oy)
        ) |>
        visNetwork::visEvents(
            dragEnd = sprintf("
                function (params) {
                    if (params.nodes.length === 0) return;
                    
                    var grid = %d;
                    var ox = %d;
                    var oy = %d;
                    
                    var updates = [];
                    for (var i = 0; i < params.nodes.length; i++) {
                        var nodeId = params.nodes[i];
                        var node = this.body.nodes[nodeId];
                        if (!node) continue;
                        var x = Math.round((node.x - ox) / grid) * grid + ox;
                        var y = Math.round((node.y - oy) / grid) * grid + oy;
                        updates.push({ id: nodeId, x: x, y: y });
                    }
                    this.body.data.nodes.update(updates);
                    Shiny.setInputValue('%s', {
                        nodes: updates,
                        nonce: Math.random()
                    }, {priority: 'event'});
                }
                ", grid_size, ox, oy, ns("node_positions"))
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

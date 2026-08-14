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
#' Generate a unicode-formatted label for hypotheses used in the graph
#' visualization. The function converts simple tokens (e.g. "n", "s", digits)
#' into appropriate unicode subscripts and returns a string suitable for node labels.
#'
#' @param number Integer indicating hypothesis index (0/1/2 etc.).
#' @param x Character short code describing the hypothesis (e.g. "ni", "s").
#' @param i Optional additional index or annotation appended on a new line.
#'
#' @return Character scalar with the formatted hypothesis label.
#'
#' @keywords internal
#' 
getHypothesisLabel <- function(number, x, i = NULL) {
    number <- chartr("0123456789-", "\u2080\u2081\u2082\u2083\u2084\u2085\u2086\u2087\u2088\u2089\u208B", as.character(number))
    x <- chartr("01-", "\u2080\u2081\u208B", as.character(x))
    x <- gsub("n", "\u2099", x, fixed = TRUE)
    x <- gsub("i", "\u1D62", x, fixed = TRUE)
    x <- gsub("s", "\u209B", x, fixed = TRUE)
    label <- paste0("H", number, "\u208D", x, "\u208E")
    if (!is.null(i)) {
        label <- paste0(label, "\n", i)
    }
    return(label)
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

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
    subscript <- "\u208"
    x <- sub("0", "\u2080", x)
    x <- sub("1", "\u2081", x)
    x <- sub("n", "\u2099", x)
    x <- sub("i", "\u1D62", x)
    x <- sub("s", "\u209B", x)
    x <- sub("-", "\u208B", x)
    if (number == 0) {
        number <- "\u2080"
    } else {
        number <- "\u2081"
    }
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
#'
#' @return A visNetwork object ready to be rendered.
#'
#' @keywords internal
#' 
getDemoGraph <- function(ns) {
    # ---- NODES ----
    nodes <- data.frame(
        id = c("H1_NI", "H1_S", "H2_NI", "H2_S"),
        label = c(
            getHypothesisLabel(1, "ni", 1),
            getHypothesisLabel(1, "s", 0),
            getHypothesisLabel(2, "ni", 0),
            getHypothesisLabel(2, "s", 0)
        ),
        # label = c("H1,NI\n1", "H1,S\n0", "H2,NI\n0", "H2,S\n0"),
        x = c(0, 300, 0, 300),
        y = c(0, 0, 200, 200),
        shape = "circle",
        
        color.background = c("#7CFF7C", "white", "white", "white"),
        color.border = "black",
        borderWidth = 2,
        font = list(size = 22)
    )

    # ---- EDGES ----
    edges <- data.frame(
        from = c("H1_NI", "H1_NI", "H2_NI", "H1_S", "H2_NI", "H1_S"),
        to = c("H1_S", "H2_NI", "H1_S", "H2_NI", "H2_S", "H2_S"),
        label = c("\u03BD", "1-\u03BD", "\u03C9", "t", "1-\u03C9", "1-t"),
        arrows = "to",
        smooth = c(FALSE, FALSE, TRUE, TRUE, FALSE, FALSE),
        font = list(size = 20, align = "top")
    )

    grid_size <- 40
    ox <- 14
    oy <- -5
    visNetwork::visNetwork(
        nodes,
        edges,
        width = "100%",
        height = "400px"
    ) |>
        visNetwork::visEdges(
            arrows = "to",
            smooth = list(type = "curvedCW", roundness = 0.3)
        ) |>
        visNetwork::visNodes(
            shape = "circle",
            borderWidth = 2,
            color = list(
                background = "white",
                border = "black"
            ),
            scaling = list(
                label = list(
                    enabled = TRUE, 
                    min = 10, 
                    max = 16
                )
            )
        ) |>
        visNetwork::visInteraction(
            dragNodes = TRUE,
            dragView = TRUE,
            zoomView = FALSE # disables mouse wheel zoom
        ) |>
        visNetwork::visPhysics(enabled = FALSE) |>
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
                    
                    var nodeId = params.nodes[0];
                    var node = this.body.nodes[nodeId];
                    
                    var x = Math.round((node.x - ox) / grid) * grid + ox;
                    var y = Math.round((node.y - oy) / grid) * grid + oy;
                    
                    this.body.data.nodes.update({ id: nodeId, x: x, y: y });
                }
                ", grid_size, ox, oy)
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

#' 
#' @title Return a Markdown description of the graph
#'
#' @description
#' Provide the textual description (in Markdown) that accompanies the graph
#' visualization. The returned text can be rendered in a preview pane or
#' written to a temporary file.
#'
#' @return Character scalar containing Markdown text describing the graph.
#'
#' @keywords internal
#' 
getGraphDescription <- function() {
    mdText <- 
"### Graph representing the procedure from Hung and Wang (2010)

**H1,NI**: Non-inferiority of the primary endpoint  
**H1,S**: Superiority of the primary endpoint  
**H2,NI**: Non-inferiority of the secondary endpoint  
**H2,S**: Superiority of the secondary endpoint  

**Literature:**  

Hung H.M.J., Wang S.-J. (2010). *Challenges to multiple testing in clinical trials.*  
*Biometrical Journal*, 52, 747-756."
    return(mdText)
}

#' 
#' @title Write the graph description to a temporary markdown file
#'
#' @description
#' Create a temporary file containing the Markdown returned by
#' getGraphDescription() and return the file path. This is convenient for
#' passing the description to components that expect a file path.
#'
#' @return Path to a temporary markdown file containing the graph description.
#'
#' @keywords internal
#' 
getGraphDescriptionFile <- function() {
    mdFile <- tempfile(fileext = ".md")
    writeLines(text = getGraphDescription(), con = mdFile)
    return(mdFile)
}

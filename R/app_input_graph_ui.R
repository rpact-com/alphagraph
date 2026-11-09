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
#' @title Graph editor UI module
#'
#' @description
#' Shiny UI definition for the graph editor panel. This creates the box
#' containing action buttons and the visNetwork output used by the
#' corresponding server module.
#'
#' @param id Module id (namespace) used for UI namespacing.
#'
#' @return UI elements describing the module.
#'
#' @keywords internal
#'
moduleGraphUI <- function(id) {
    ns <- shiny::NS(id)

    bs4Dash::box(
        width = 12,
        height = "430px",
        solidHeader = TRUE,
        title = iconTitle("Graph", "circle-nodes"),
        collapsible = FALSE,
        # maximizable = TRUE,

        shiny::singleton(
            shiny::tagList(
                shiny::includeCSS(getSystemFile("www/graph-panel.css")),
                shiny::includeScript(getSystemFile("www/js/graph-view-controls.js")),
                shiny::includeScript(getSystemFile("www/js/graph-edge-drawing.js"))
            )
        ),
        shiny::uiOutput(ns("ctx_overlay")),
        shiny::uiOutput(ns("ctx_menu")),
        shiny::fluidRow(
            bs4Dash::column(
                12,
                shiny::tags$div(
                    style = "margin-bottom:10px;",
                    shiny::actionButton(
                        ns("add_node"),
                        label = NULL,
                        icon = shiny::icon("plus-circle"),
                        title = "Add new node"
                    ),
                    shiny::actionButton(
                        ns("add_edge"),
                        label = NULL,
                        icon = shiny::icon("share"),
                        title = "Add new edge..."
                    ),
                    shiny::actionButton(
                        ns("edit_node"),
                        label = NULL,
                        icon = shiny::icon("edit"),
                        title = "Edit selected node"
                    ),
                    shiny::actionButton(
                        ns("edit_edge"),
                        label = NULL,
                        icon = shiny::icon("pencil-ruler"),
                        title = "Edit selected edge"
                    ),
                    shiny::actionButton(
                        ns("delete"),
                        label = NULL,
                        icon = shiny::icon("trash"),
                        title = "Delete selected node(s) or edge(s)"
                    ),
                    shiny::actionButton(
                        ns("reset_view"),
                        label = NULL,
                        icon = shiny::icon("crosshairs"),
                        title = "Reset graph view"
                    ),
                    shiny::actionButton(
                        ns("zoom_in"),
                        label = NULL,
                        icon = shiny::icon("search-plus"),
                        title = "Zoom in"
                    ),
                    shiny::actionButton(
                        ns("zoom_out"),
                        label = NULL,
                        icon = shiny::icon("search-minus"),
                        title = "Zoom out"
                    ),
                    shiny::actionButton(
                        ns("lock_pos"),
                        label = NULL,
                        icon = shiny::icon("lock"),
                        title = "Toggle graph panning"
                    )
                )
            )
        ),
        shiny::tags$div(
            class = "alpha-graph-canvas",
            visNetwork::visNetworkOutput(ns("graph"), height = "100%")
        ),
        shiny::tags$div(
            class = "alpha-graph-resize-handle",
            `data-graph-id` = ns("graph"),
            role = "separator",
            tabindex = "0",
            `aria-label` = "Resize graph panel",
            `aria-orientation` = "horizontal",
            `aria-controls` = ns("graph"),
            `aria-valuemin` = "430",
            `aria-valuenow` = "430",
            title = "Drag to resize graph panel, or use Up and Down arrow keys"
        )
    )
}

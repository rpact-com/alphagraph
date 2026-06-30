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
        title = "Graph",
        # maximizable = TRUE,
        
        shiny::uiOutput(ns("ctx_overlay")),
        shiny::uiOutput(ns("ctx_menu")),
        
        shiny::fluidRow(
            bs4Dash::column(
                12,
                shiny::tags$div(
                    style = "margin-bottom:10px;",
                    shiny::actionButton(ns("add_node"), label = NULL, icon = shiny::icon("plus-circle")),
                    shiny::actionButton(ns("add_edge"), label = NULL, icon = shiny::icon("share")),
                    shiny::actionButton(ns("edit_node"), label = NULL, icon = shiny::icon("edit")),
                    shiny::actionButton(ns("edit_edge"), label = NULL, icon = shiny::icon("pencil-ruler")),
                    shiny::actionButton(ns("delete"), label = NULL, icon = shiny::icon("trash")),
                    shiny::actionButton(ns("reset_view"), label = NULL, icon = shiny::icon("crosshairs")),
                    shiny::actionButton(ns("zoom_in"), label = NULL, icon = shiny::icon("search-plus")),
                    shiny::actionButton(ns("zoom_out"), label = NULL, icon = shiny::icon("search-minus")),
                    shiny::actionButton(ns("lock_pos"), label = NULL, icon = shiny::icon("lock")),
                    shiny::actionButton(ns("unlock_pos"), label = NULL, icon = shiny::icon("unlock"))
                )
            )
        ),

        visNetwork::visNetworkOutput(ns("graph"), height = "350px")
    )
}
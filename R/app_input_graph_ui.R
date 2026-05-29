moduleGraphUI <- function(id) {
    ns <- NS(id)
    
    box(
        width = 12,
        height = "430px",
        solidHeader = TRUE,
        title = "Graph",
        # maximizable = TRUE,
        fluidRow(
            column(
                12,
                div(
                    style = "margin-bottom:10px;",
                    actionButton(ns("add_node"), label = NULL, icon = icon("plus-circle")),
                    actionButton(ns("add_edge"), label = NULL, icon = icon("share")),
                    actionButton(ns("edit_node"), label = NULL, icon = icon("edit")),
                    actionButton(ns("edit_edge"), label = NULL, icon = icon("pencil-ruler")),
                    actionButton(ns("delete"), label = NULL, icon = icon("trash")),
                    actionButton(ns("reset_view"), label = NULL, icon = icon("crosshairs")),
                    actionButton(ns("zoom_in"), label = NULL, icon = icon("search-plus")),
                    actionButton(ns("zoom_out"), label = NULL, icon = icon("search-minus")),
                    actionButton(ns("lock_pos"), label = NULL, icon = icon("lock")),
                    actionButton(ns("unlock_pos"), label = NULL, icon = icon("unlock"))
                )
            )
        ),

        visNetwork::visNetworkOutput(ns("graph"), height = "350px")
    )
}
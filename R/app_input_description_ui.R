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

 #' @title Description panel UI module
 #'
 #' @description
 #' UI definition for the description panel that allows editing and
 #' previewing a Markdown description associated with the graph.
 #'
 #' @param id Module id (namespace) used for UI namespacing.
 #'
 #' @return UI elements for the description panel.
 #'
 #' @keywords internal
moduleDescriptionUI <- function(id) {
    ns <- shiny::NS(id)
    
    shiny::fluidRow(
        shiny::column(
            12,
            shiny::div(
                style = "margin-bottom: 10px; border-bottom: 1px solid #dee2e6; padding-bottom: 10px;",
                shiny::actionButton(
                    inputId = ns("descModePreview"),
                    label = "Preview",
                    class = "btn-sm btn-primary"
                ),
                shiny::actionButton(
                    inputId = ns("descModeEdit"),
                    label = "Edit",
                    class = "btn-sm btn-outline-secondary"
                )
            ),
            shiny::uiOutput(ns("descContent"))
        )
    )
}

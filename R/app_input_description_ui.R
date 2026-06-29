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
##
## alphagraph
##
## UI for the first shortcut-analysis workflow.
##

#' @title Analysis panel UI module
#'
#' @param id Module id used for namespacing.
#'
#' @return UI elements for configuring and displaying a shortcut analysis.
#' @keywords internal
moduleAnalysisUI <- function(id) {
    ns <- shiny::NS(id)

    bs4Dash::box(
        width = 12,
        solidHeader = TRUE,
        title = iconTitle("Results", "table"),
        shiny::fluidRow(
            style = "align-items: flex-end;",
            bs4Dash::column(
                4,
                shiny::numericInput(
                    ns("alpha"),
                    "Overall alpha",
                    value = 0.025,
                    min = 0.000001,
                    max = 1,
                    step = 0.005
                )
            ),
            bs4Dash::column(
                4,
                shiny::tags$div(
                    class = "form-group",
                    shiny::actionButton(
                        ns("runShortcut"),
                        "Run",
                        icon = shiny::icon("play"),
                        title = "Run shortcut analysis"
                    )
                )
            )
        ),
        shiny::tags$p(
            class = "text-muted small",
            "Raw p-values are stored in the project dataset, not in the graph. Results are invalidated when an input changes."
        ),
        .responsiveTableOutput(ns("analysisTable")),
        shiny::textOutput(ns("analysisStatus"))
    )
}

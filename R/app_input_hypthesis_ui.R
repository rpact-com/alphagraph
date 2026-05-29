moduleHypothesisUI <- function(id) {
    ns <- shiny::NS(id)
    
    bs4Dash::box(
        width = 12,
        height = "850px",
        solidHeader = TRUE,
        title = "Hypotheses",
        shiny::fluidRow(
            bs4Dash::column(4, shiny::tags$b("Hypothesis")),
            bs4Dash::column(4, shiny::tags$b("Weight")),
            bs4Dash::column(4, shiny::tags$b("P-Value"))
        ),
        shiny::uiOutput(ns("hypothesisPanel")),
        shiny::tags$br(),
        shiny::fluidRow(
            bs4Dash::column(
                6,
                shiny::numericInput("totalAlpha", "Total a:", value = 0.05)
            ),
            bs4Dash::column(
                6,
                shiny::actionButton("loadPValues", "Load p-values from R")
            )
        ),
        shiny::tags$hr(),
        shiny::radioButtons("corrInfo", "Correlation Information",
            choices = c(
                "No Information" = "none",
                "Use R correlation matrix" = "r",
                "Use Simes test" = "simes"
            )
        ),
        shiny::tags$hr(),
        shiny::actionButton("refresh", "Refresh"),
        shiny::actionButton("createMatrix", "Create Matrix")
    )
}
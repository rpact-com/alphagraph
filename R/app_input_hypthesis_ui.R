moduleHypothesisUI <- function(id) {
    ns <- NS(id)
    
    box(
        width = 12,
        height = "850px",
        solidHeader = TRUE,
        title = "Hypotheses",
        fluidRow(
            column(4, tags$b("Hypothesis")),
            column(4, tags$b("Weight")),
            column(4, tags$b("P-Value"))
        ),
        uiOutput(ns("hypothesisPanel")),
        br(),
        fluidRow(
            column(
                6,
                numericInput("totalAlpha", "Total a:", value = 0.05)
            ),
            column(
                6,
                actionButton("loadPValues", "Load p-values from R")
            )
        ),
        hr(),
        radioButtons("corrInfo", "Correlation Information",
            choices = c(
                "No Information" = "none",
                "Use R correlation matrix" = "r",
                "Use Simes test" = "simes"
            )
        ),
        hr(),
        actionButton("refresh", "Refresh"),
        actionButton("createMatrix", "Create Matrix")
    )
}
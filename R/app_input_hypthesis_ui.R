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

 #' @title Hypotheses UI module
 #'
 #' @description
 #' UI for entering and displaying hypotheses, weights and p-values. The
 #' module creates a panel with inputs and controls for managing hypotheses
 #' and generating the transition matrix.
 #'
 #' @param id Module id (namespace) used for UI namespacing.
 #'
 #' @return UI elements for the hypothesis panel.
 #'
 #' @keywords internal
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
            bs4Dash::column(4, shiny::tags$b("p-Value"))
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

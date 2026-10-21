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
#' UI for editing hypothesis labels, weights and p-values in a spreadsheet.
#' The table also shows the read-only label rendered by visNetwork.
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
        #height = "850px",
        solidHeader = TRUE,
        title = iconTitle("Hypotheses", "diagram-project"),
        .responsiveTableOutput(ns("hypothesisTable")), 
        shiny::tags$p(
            class = "text-muted small",
            "Cells are editable except ID and Rendered label. Use H1_ni or H1_(ni) for subscripts; weights may be decimals or fractions."
        )
    )
}

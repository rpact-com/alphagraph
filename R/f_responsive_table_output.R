##
## alphagraph
##
## This file is part of the R package alphagraph
##
## Author: Friedrich Pahlke, PhD
## Licensed under "GNU Affero General Public License" version 3.0
## License text can be found here: https://cran.r-project.org/web/licenses/AGPL-3
##
## RPACT company website: https://www.rpact.com
##

# Shared output styling: only Handsontable's master viewport may scroll.
.responsiveTableOutput <- function(outputId) {
    output <- rhandsontable::rHandsontableOutput(outputId)
    if (inherits(output, "shiny.tag")) {
        output <- shiny::tagAppendAttributes(output, class = "alpha-table-layout")
    } else {
        # Preserve the dependencies carried by htmlwidgets' tag list.
        output[[1L]] <- shiny::tagAppendAttributes(
            output[[1L]], class = "alpha-table-layout"
        )
    }
    shiny::tagList(
        shiny::singleton(shiny::includeCSS(getSystemFile("www/table-layout.css"))),
        output
    )
}

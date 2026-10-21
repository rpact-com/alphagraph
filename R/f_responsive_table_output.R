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

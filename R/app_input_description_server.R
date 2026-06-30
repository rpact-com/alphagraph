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

 #' @title Description panel server module
 #'
 #' @description
 #' Server-side logic for the description editor and preview panel. Manages
 #' the reactive state of the Markdown content and toggling between edit and
 #' preview modes.
 #'
 #' @param id Module id (namespace) for this server module.
 #' @param ... Additional parameters (reserved for future use).
 #' @param args Named list of optional parameters passed to the module.
 #'
 #' @return Invisibly NULL; registers server observers and outputs.
 #'
 #' @keywords internal
moduleDescriptionServer <- function(id, ..., args = list()) {
    shiny::moduleServer(
        id,
        function(input, output, session) {
            ns <- session$ns

            logDebug("///--- moduleDescriptionServer id = ", id, " (", session$ns("xxx"), ")")

            # Reactive values for description editor
            descRv <- shiny::reactiveValues(
                markdownContent = getGraphDescription(),
                mode = "preview" # "preview" or "edit"
            )

            # Initialize the markdown input with default content (only once)
            shiny::observe({
                shiny::updateTextAreaInput(
                    session = session,
                    inputId = ns("descMarkdownInput"),
                    value = descRv$markdownContent
                )
            }) |> 
                shiny::bindEvent(descRv$markdownContent, once = TRUE)

            # Toggle to Edit mode
            shiny::observeEvent(input$descModeEdit, {
                descRv$mode <- "edit"
                # Update button styling with JavaScript
                session$sendCustomMessage(
                    type = "toggleButtonClass",
                    message = list(
                        activeId = ns("descModeEdit"),
                        inactiveId = ns("descModePreview")
                    )
                )
            })

            # Toggle to Preview mode
            shiny::observeEvent(input$descModePreview, {
                # Save the edited content when switching to preview
                if (!is.null(input$descMarkdownInput)) {
                    descRv$markdownContent <- input$descMarkdownInput
                }
                descRv$mode <- "preview"
                # Update button styling with JavaScript
                session$sendCustomMessage(
                    type = "toggleButtonClass",
                    message = list(
                        activeId = ns("descModePreview"),
                        inactiveId = ns("descModeEdit")
                    )
                )
            })

            # Render the appropriate content based on mode
            output$descContent <- shiny::renderUI({
                if (descRv$mode == "preview") {
                    # Render markdown preview
                    shiny::div(
                        style = "padding: 10px; min-height: 300px;",
                        #shiny::HTML(markdown::markdownToHTML(text = descRv$markdownContent, fragment.only = TRUE))
                        shiny::HTML(markdown::markdownToHTML(text = htmltools::htmlEscape(descRv$markdownContent), fragment.only = TRUE))
                    )
                } else {
                    # Render markdown editor
                    shiny::textAreaInput(
                        inputId = ns("descMarkdownInput"),
                        label = NULL,
                        value = descRv$markdownContent,
                        width = "100%",
                        height = "400px",
                        resize = "vertical",
                        placeholder = "Enter markdown text here..."
                    )
                }
            })
        }
    )
}

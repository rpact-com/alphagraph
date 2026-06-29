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
                        activeId = "descModeEdit",
                        inactiveId = "descModePreview"
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
                        activeId = "descModePreview",
                        inactiveId = "descModeEdit"
                    )
                )
            })

            # Render the appropriate content based on mode
            output$descContent <- shiny::renderUI({
                if (descRv$mode == "preview") {
                    # Render markdown preview
                    shiny::div(
                        style = "padding: 10px; min-height: 300px;",
                        shiny::HTML(markdown::markdownToHTML(text = descRv$markdownContent, fragment.only = TRUE))
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

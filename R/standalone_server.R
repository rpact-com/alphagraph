#' 
#' @title 
#' Standalone Shiny server for the alphagraph app.
#' 
#' @param input The input object from the Shiny server function, containing all reactive inputs from the UI.
#' @param output The output object from the Shiny server function, used to render outputs back to the UI.
#' @param session The session object from the Shiny server function, providing information about the current 
#'        user session and allowing for session-specific operations.
#' 
#' @description
#' This function defines the server logic for the standalone version of the alphagraph Shiny app. 
#' It initializes the server-side components for the graph editor, transition matrix, and 
#' hypothesis panel modules. Additionally, it renders a description of the graph and a placeholder 
#' for analysis results. The description is loaded from a Markdown file, while the analysis output 
#' is currently a static text that can be updated with dynamic content in the future.
#' 
#' @export
#' 
createServer <- function(input, output, session) {

    moduleGraphServer("graphIO")
    moduleTransitionMatrixServer("transitionMatrix")
    moduleHypothesisServer("hypothesisPanel")

    # Reactive values for description editor
    descRv <- shiny::reactiveValues(
        markdownContent = getGraphDescription(),
        mode = "preview"  # "preview" or "edit"
    )

    # Initialize the markdown input with default content (only once)
    shiny::observe({
        shiny::updateTextAreaInput(
            session = session,
            inputId = "descMarkdownInput",
            value = descRv$markdownContent
        )
    }) |> shiny::bindEvent(descRv$markdownContent, once = TRUE)

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
                "descMarkdownInput",
                label = NULL,
                value = descRv$markdownContent,
                width = "100%",
                height = "400px",
                resize = "vertical",
                placeholder = "Enter markdown text here..."
            )
        }
    })

    output$analysis <- shiny::renderText({
        "Hypotheses, weights, and p-values go here."
    })

}

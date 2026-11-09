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

 #' @title Description panel server module
 #'
 #' @description
 #' Server-side logic for the description editor and preview panel. Manages
 #' the reactive state of the Markdown content and toggling between edit and
 #' preview modes.
 #'
 #' @param id Module id (namespace) for this server module.
 #' @param graphStore Shared `AlphaGraphStore`. If omitted, the module creates
 #'   a private store for backwards compatibility.
 #' @param ... Additional parameters (reserved for future use).
 #' @param args Named list of optional parameters. `catalogPath` can point to
 #'   an alternative graph-description catalog.
 #'
 #' @return Invisibly NULL; registers server observers and outputs.
 #'
 #' @keywords internal
moduleDescriptionServer <- function(
        id,
        graphStore = NULL,
        ...,
        args = list()) {
    shiny::moduleServer(
        id,
        function(input, output, session) {
            ns <- session$ns

            logDebug("///--- moduleDescriptionServer id = ", id, " (", session$ns("xxx"), ")")

            if (is.null(graphStore)) {
                graphStore <- createAlphaGraphStore(createDemoAlphaGraph())
            }
            if (!inherits(graphStore, "AlphaGraphStore")) {
                stop("graphStore must be an AlphaGraphStore object.", call. = FALSE)
            }

            initialDescription <- getGraphDescription(
                graph = graphStore$snapshot(),
                catalogPath = args$catalogPath %||% NULL
            )

            descRv <- shiny::reactiveValues(
                markdownContent = initialDescription,
                lastGenerated = initialDescription,
                userModified = FALSE,
                mode = "preview"
            )

            generatedDescription <- shiny::reactive({
                graphStore$revision()
                getGraphDescription(
                    graph = graphStore$snapshot(),
                    catalogPath = args$catalogPath %||% NULL
                )
            })

            replaceWithGenerated <- function() {
                generated <- generatedDescription()
                descRv$markdownContent <- generated
                descRv$lastGenerated <- generated
                descRv$userModified <- FALSE
            }

            # Follow graph changes while the generated text is untouched. If
            # an edit is open, capture its current value before deciding.
            shiny::observeEvent(generatedDescription(), {
                editorValue <- shiny::isolate(input$descMarkdownInput)
                if (identical(shiny::isolate(descRv$mode), "edit") &&
                        !is.null(editorValue) &&
                        !identical(editorValue, shiny::isolate(descRv$lastGenerated))) {
                    descRv$markdownContent <- editorValue
                    descRv$userModified <- TRUE
                }
                if (!isTRUE(shiny::isolate(descRv$userModified))) {
                    replaceWithGenerated()
                }
            }, ignoreInit = TRUE)

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
                    descRv$userModified <- !identical(
                        input$descMarkdownInput,
                        descRv$lastGenerated
                    )
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

            shiny::observeEvent(input$descRegenerate, {
                editorValue <- shiny::isolate(input$descMarkdownInput)
                if (identical(shiny::isolate(descRv$mode), "edit") &&
                        !is.null(editorValue) &&
                        !identical(editorValue, shiny::isolate(descRv$lastGenerated))) {
                    descRv$markdownContent <- editorValue
                    descRv$userModified <- TRUE
                }
                if (isTRUE(descRv$userModified)) {
                    shiny::showModal(shiny::modalDialog(
                        title = "Regenerate description template?",
                        "This replaces your current description with a new template generated from the graph.",
                        footer = shiny::tagList(
                            shiny::modalButton("Cancel"),
                            shiny::actionButton(
                                ns("descRegenerateConfirm"),
                                "Regenerate",
                                class = "btn-danger"
                            )
                        ),
                        easyClose = TRUE
                    ))
                } else {
                    replaceWithGenerated()
                }
            })

            shiny::observeEvent(input$descRegenerateConfirm, {
                shiny::removeModal()
                replaceWithGenerated()
            })

            reportMarkdown <- shiny::reactive({
                if (identical(descRv$mode, "edit") && !is.null(input$descMarkdownInput)) {
                    input$descMarkdownInput
                } else {
                    descRv$markdownContent
                }
            })

            reportDownload <- function(extension) {
                shiny::downloadHandler(
                    filename = function() paste0("alphagraph-report.", extension),
                    contentType = "text/plain; charset=utf-8",
                    content = function(file) {
                        header <- if (extension == "Rmd") {
                            "---\noutput: html_document\n---\n"
                        } else {
                            "---\nformat: html\n---\n"
                        }
                        writeLines(enc2utf8(c(header, reportMarkdown())), file, useBytes = TRUE)
                    }
                )
            }
            output$descDownloadRmd <- reportDownload("Rmd")
            output$descDownloadQmd <- reportDownload("qmd")

            # Render the appropriate content based on mode
            output$descContent <- shiny::renderUI({
                if (descRv$mode == "preview") {
                        shiny::div(
                            style = "padding: 10px; min-height: 300px;",
                            shiny::HTML(markdown::markdownToHTML(
                                text = htmltools::htmlEscape(
                                    .descriptionPreviewMarkdown(
                                        descRv$markdownContent
                                    )
                                ),
                                fragment.only = TRUE
                            ))
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

            invisible(NULL)
        }
    )
}

.descriptionPreviewMarkdown <- function(markdown) {
    # HTML comments remain in the editable Markdown but are omitted from preview.
    gsub("(?s)<!--.*?-->", "", as.character(markdown), perl = TRUE)
}

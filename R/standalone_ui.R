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

#' 
#' Create the UI for the standalone Shiny app
#' 
#' @param request The Shiny request object (not used here but required for Shiny UI functions)
#' 
#' @return A Shiny UI definition for the alphagraph app
#' 
#' @description 
#' This function defines the user interface for the standalone version of the alphagraph Shiny app. 
#' It uses the bs4Dash package to create a dashboard layout with a header, sidebar, and body. 
#' The sidebar contains menu items for navigating between the "Graph Editor" and "Settings" tabs. 
#' The body includes custom CSS for styling, as well as UI outputs for a context menu and overlay 
#' that will be rendered dynamically when the context menu is open. The "Graph Editor" tab contains 
#' a visNetwork graph and additional panels for description and analysis, while the "Settings" 
#' tab is currently a placeholder for future settings options.
#' 
#' @export
#' 
createUI <- function(request) {
    tagList(
        tags$head(
            tags$meta(charset = "UTF-8"),
            tags$meta(
                name = "keywords",
                content = "alphagraph, graphicalMCP, gMCP, Clinical Trials, Friedrich Pahlke"
            ),
            tags$meta(
                name = "description",
                content = "alphagraph"
            ),
            tags$link(href = "www/favicon.ico", rel = "icon"),
            tags$link(href = "www/favicon.ico", rel = "apple-touch-icon"),
            tags$link(href = "www/favicon.ico", rel = "shortcut icon", type = "image/vnd.microsoft.icon"),
            includeJavaScript("www/js/fix-sidebar-theme.js")
        ),
        
        bs4Dash::dashboardPage(
            title = "alphagraph",
            dark = FALSE,
            help = (if (isMaximumPackageVersion("2.3.0", "bs4Dash", inclusive = FALSE)) FALSE else NULL),
            skin = "light",
            fullscreen = TRUE,
            scrollToTop = TRUE,
            
            header = bs4Dash::dashboardHeader(
                title = bs4Dash::dashboardBrand(
                    title = "alphagraph",
                    color = "secondary",
                    href = "https://www.rpact.com",
                    image = "www/alphagraph.png",
                    opacity = 0.8
                ),
                compact = FALSE,
                skin = "dark",
                status = "gray",
                border = FALSE,
                fixed = TRUE
            ),
            
            sidebar = bs4Dash::dashboardSidebar(
                width = 80,
                collapsed = FALSE,
                expandOnHover = TRUE,
                fixed = TRUE,
                customArea = NULL,
                bs4Dash::sidebarMenu(
                    bs4Dash::menuItem("Graph", tabName = "main-tab", icon = shiny::icon("project-diagram")),
                    bs4Dash::menuItem("Settings", tabName = "settings-tab", icon = shiny::icon("sliders-h"))
                )
            ),
            
            controlbar = NULL,
            
            body = bs4Dash::dashboardBody(
                shiny::includeCSS(getSystemFile("www/context-menu.css")),
                shiny::includeCSS(getSystemFile("www/vis-network.css")),
                includeJavaScript("www/js/toggle-button.js"),
                bs4Dash::tabItems(
                    bs4Dash::tabItem(
                        tabName = "main-tab",
                        
                        bs4Dash::tabBox(
                            width = 12,
                            shiny::tabPanel(
                                title = "Design",
                                value = "design-tab",
                                shiny::fluidRow(
                                    class = "alpha-design-split",
                                    bs4Dash::column(
                                        7,
                                        id = "alpha-design-left",
                                        class = "alpha-design-left",
                                        moduleGraphUI("graphIO"),
                                        bs4Dash::box(
                                            width = 12,
                                            #height = "430px",
                                            solidHeader = TRUE,
                                            title = "Results",
                                            moduleAnalysisUI("analysisPanel")
                                        )
                                    ),
    
                                    shiny::tags$div(
                                        class = "alpha-design-splitter",
                                        role = "separator",
                                        tabindex = "0",
                                        `aria-label` = "Resize graph and results column",
                                        `aria-orientation` = "vertical",
                                        `aria-controls` = "alpha-design-left",
                                        `aria-valuemin` = "25",
                                        `aria-valuemax` = "75",
                                        `aria-valuenow` = "58.33",
                                        title = "Drag to resize columns, or use Left and Right arrow keys"
                                    ),
                                    bs4Dash::column(
                                        5,
                                        class = "alpha-design-right",
                                        moduleTransitionMatrixUI("transitionMatrix"),
                                        moduleHypothesisUI("hypothesisPanel")
                                    )
                                )
                            ),
                            shiny::tabPanel(
                                title = "Reporting",
                                value = "reporting-tab",
                                moduleDescriptionUI("descriptionPanel")
                            )
                        )
                    ),
    
                    bs4Dash::tabItem(
                        tabName = "settings-tab",
                        shiny::tags$h2("Settings"),
                        shiny::tags$p("alphagraph settings will be implemented here in the future.")
                    )
                )
            ),
            
            footer = bs4Dash::dashboardFooter(
                left = "alphagraph",
                right = "© 2026 RPACT"
            )
        )
    )
}

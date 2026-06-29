
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
    bs4Dash::dashboardPage(
        header = bs4Dash::dashboardHeader(title = "alphagraph"),
        sidebar = bs4Dash::dashboardSidebar(
            bs4Dash::sidebarMenu(
                bs4Dash::menuItem("Graph Editor", tabName = "graph", icon = shiny::icon("project-diagram")),
                bs4Dash::menuItem("Settings", tabName = "settings", icon = shiny::icon("sliders-h"))
            )
        ),
        body = bs4Dash::dashboardBody(
            shiny::includeCSS(getSystemFile("www/context-menu.css")),
            shiny::includeCSS(getSystemFile("www/vis-network.css")),
            includeJavaScript("www/js/toggle-button.js"),
            bs4Dash::tabItems(
                bs4Dash::tabItem(
                    tabName = "graph",
                    
                    shiny::fluidRow(
                        bs4Dash::column(
                            7,
                            moduleGraphUI("graphIO"),
                            bs4Dash::tabBox(
                                width = 12,
                                shiny::tabPanel(
                                    "Description",
                                    moduleDescriptionUI("descriptionPanel")
                                ),
                                shiny::tabPanel(
                                    "Analysis",
                                    moduleAnalysisUI("analysisPanel")
                                )
                            )
                        ),

                        bs4Dash::column(
                            5,
                            moduleTransitionMatrixUI("transitionMatrix"),
                            moduleHypothesisUI("hypothesisPanel")
                        )
                    )
                ),

                bs4Dash::tabItem(
                    tabName = "settings",
                    shiny::tags$h2("Settings"),
                    shiny::tags$p("alphagraph settings will be implemented here in the future.")
                )
            )
        )
    )
}

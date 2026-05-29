
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
        shiny::includeCSS(getSystemFile("www/style.css")),
        bs4Dash::dashboardHeader(title = "alphagraph"),
        bs4Dash::dashboardSidebar(
            bs4Dash::sidebarMenu(
                bs4Dash::menuItem("Graph Editor", tabName = "graph", icon = shiny::icon("project-diagram")),
                bs4Dash::menuItem("Settings", tabName = "settings", icon = shiny::icon("sliders-h"))
            )
        ),
        bs4Dash::dashboardBody(
            shiny::tags$head(
                shiny::tags$style(shiny::HTML("
                /* Click-away overlay when context menu is open */
                #ctx-overlay {
                position: fixed;
                top: 0; left: 0;
                width: 100vw; height: 100vh;
                background: rgba(0,0,0,0.0);
                z-index: 1040; /* below menu but above content */
                }
                
                /* Context menu styling (Bootstrap-like) */
                #ctx-menu {
                position: fixed;
                z-index: 1050;
                min-width: 220px;
                box-shadow: 0 .5rem 1rem rgba(0,0,0,.15);
                }
                
                /* Make menu items feel like native context menu */
                #ctx-menu .dropdown-item {
                cursor: pointer;
                }
                "))
            ),
            
            bs4Dash::tabItems(
                bs4Dash::tabItem(
                    tabName = "graph",
                    
                    # These are rendered dynamically when menu is open
                    shiny::uiOutput("ctx_overlay"),
                    shiny::uiOutput("ctx_menu"),
                    
                    shiny::fluidRow(
                        shiny::tags$head(
                            shiny::tags$style(shiny::HTML("
                                .vis-network {
                                background-image: linear-gradient(#e0e0e0 1px, transparent 1px),
                                linear-gradient(90deg, #e0e0e0 1px, transparent 1px);
                                background-size: 40px 40px;
								background-position: 0px 0px;  /* <-- Offset  */
                                }
                                "))
                        ),
                        bs4Dash::column(
                            7,
                            moduleGraphUI("graphIO"),
                            bs4Dash::tabBox(
                                width = 12,
                                shiny::tabPanel(
                                    "Description",
                                    shiny::uiOutput("desc")
                                ),
                                shiny::tabPanel(
                                    "Analysis",
                                    shiny::verbatimTextOutput("analysis")
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
                    shiny::tags$p("Here you can add app settings…")
                )
            )
        )
    )
}

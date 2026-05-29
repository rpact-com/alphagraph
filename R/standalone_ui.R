createUI <- function(request) {
    dashboardPage(
        dashboardHeader(title = "alphagraph"),
        dashboardSidebar(
            sidebarMenu(
                menuItem("Graph Editor", tabName = "graph", icon = icon("project-diagram")),
                menuItem("Settings", tabName = "settings", icon = icon("sliders-h"))
            )
        ),
        dashboardBody(
            tags$head(
                tags$style(HTML("
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
            
            tabItems(
                # ============================================================
                #                     GRAPH TAB (MAIN)
                # ============================================================
                tabItem(
                    tabName = "graph",
                    
                    # These are rendered dynamically when menu is open
                    uiOutput("ctx_overlay"),
                    uiOutput("ctx_menu"),
                    
                    fluidRow(
                        tags$head(
                            tags$style(HTML("
                                .vis-network {
                                background-image: linear-gradient(#e0e0e0 1px, transparent 1px),
                                linear-gradient(90deg, #e0e0e0 1px, transparent 1px);
                                background-size: 40px 40px;
								background-position: 0px 0px;  /* <-- Offset  */
                                }
                                "))
                        ),
                        column(
                            7,

                            # LEFT: GRAPH + Description/Analysis
                            box(
                                width = 12,
                                height = "430px",
                                solidHeader = TRUE,
                                title = "Graph",
                                # maximizable = TRUE,
                                fluidRow(
                                    column(
                                        12,
                                        div(
                                            style = "margin-bottom:10px;",
                                            actionButton("add_node", label = NULL, icon = icon("plus-circle")),
                                            actionButton("add_edge", label = NULL, icon = icon("share")),
                                            actionButton("edit_node", label = NULL, icon = icon("edit")),
                                            actionButton("edit_edge", label = NULL, icon = icon("pencil-ruler")),
                                            actionButton("delete", label = NULL, icon = icon("trash")),
                                            actionButton("reset_view", label = NULL, icon = icon("crosshairs")),
                                            actionButton("zoom_in", label = NULL, icon = icon("search-plus")),
                                            actionButton("zoom_out", label = NULL, icon = icon("search-minus")),
                                            actionButton("lock_pos", label = NULL, icon = icon("lock")),
                                            actionButton("unlock_pos", label = NULL, icon = icon("unlock"))
                                        )
                                    )
                                ),

                                # visNetwork output
                                visNetwork::visNetworkOutput("vis", height = "350px")
                            ),
                            tabBox(
                                width = 12,
                                tabPanel(
                                    "Description",
                                    uiOutput("desc")
                                ),
                                tabPanel(
                                    "Analysis",
                                    verbatimTextOutput("analysis")
                                )
                            )
                        ),

                        # RIGHT: TRANSITION MATRIX + HYPOTHESIS PANEL
                        column(
                            5,
                            moduleTransitionMatrixUI("transitionMatrix"),
                            moduleHypothesisUI("hypothesisPanel")
                        )
                    )
                ),

                # ============================================================
                #                      SETTINGS TAB
                # ============================================================
                tabItem(
                    tabName = "settings",
                    h2("Settings"),
                    p("Here you can add app settings…")
                )
            )
        )
    )
}

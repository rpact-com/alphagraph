createUI <- function(request) {
    dashboardPage(
        dashboardHeader(title = "alphagraph POC"),
        dashboardSidebar(
            sidebarMenu(
                menuItem("Graph Editor", tabName = "graph", icon = icon("project-diagram")),
                menuItem("Settings", tabName = "settings", icon = icon("sliders-h"))
            )
        ),
        dashboardBody(
            tabItems(
                # ============================================================
                #                     GRAPH TAB (MAIN)
                # ============================================================
                tabItem(
                    tabName = "graph",
                    fluidRow(
                        # LEFT: GRAPH + Description/Analysis
                        box(
                            width = 7, 
                            height = "400px", 
                            solidHeader = TRUE,
                            title = "Graph",
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
                            visNetwork::visNetworkOutput("vis", height = "350px"),
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
                        box(
                            width = 5, height = "850px", solidHeader = TRUE,
                            title = "Transition Matrix",
                            DTOutput("transitionMatrix"),
                            br(),
                            tags$h4("Hypotheses"),
                            fluidRow(
                                column(4, tags$b("Hypothesis")),
                                column(4, tags$b("Weight")),
                                column(4, tags$b("P-Value"))
                            ),
                            uiOutput("hypothesisPanel"),
                            br(),
                            fluidRow(
                                column(
                                    6,
                                    numericInput("totalAlpha", "Total a:", value = 0.05)
                                ),
                                column(
                                    6,
                                    actionButton("loadPValues", "Load p-values from R")
                                )
                            ),
                            hr(),
                            radioButtons("corrInfo", "Correlation Information",
                                choices = c(
                                    "No Information" = "none",
                                    "Use R correlation matrix" = "r",
                                    "Use Simes test" = "simes"
                                )
                            ),
                            hr(),
                            actionButton("refresh", "Refresh"),
                            actionButton("createMatrix", "Create Matrix")
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

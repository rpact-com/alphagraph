

# install.packages("visNetwork") # cran
# devtools::install_github("datastorm-open/visNetwork") # development version

library(shiny)
library(visNetwork)

ui <- fluidPage(
    tags$head(
        tags$style(HTML("
                    .vis-network {
                    background-image: linear-gradient(#e0e0e0 1px, transparent 1px),
                    linear-gradient(90deg, #e0e0e0 1px, transparent 1px);
                    background-size: 40px 40px;
                    }
                    "))
    ),
    visNetworkOutput("gmcptest", height = "700px")
)

getHypothesisLabel <- function(number, x, i) {
    subscript <- "\u208"
    x <- sub("0", "\u2080", x)
    x <- sub("1", "\u2081", x)
    x <- sub("n", "\u2099", x)
    x <- sub("i", "\u1D62", x)
    x <- sub("s", "\u209B", x)
    x <- sub("-", "\u208B", x)
    if (number == 0) {
        number <- "\u2080"
    } else {
        number <- "\u2081"
    }
    paste0("H", number, "\u208D", x, "\u208E", "\n", i)
}

server <- function(input, output) {
    
    output$gmcptest <- renderVisNetwork({
            
            # ---- NODES ----
            nodes <- data.frame(
                id    = c("H1_NI"   , "H1_S"   , "H2_NI"   , "H2_S"   ),
                label = c(
                    getHypothesisLabel(1, "ni", 1), 
                    getHypothesisLabel(1, "s", 0), 
                    getHypothesisLabel(3, "ni", 0), 
                    getHypothesisLabel(3, "s", 0)),
                #label = c("H1,NI\n1", "H₁,S\n0", "H₂,NI\n0", "H₂,S\n0"),
                x = c(0, 300, 0, 300),
                y = c(0, 0, 200, 200),
                shape = "circle",
                #color = c("#7CFF7C", "white", "white", "white"),
                color.background = c("#7CFF7C", "white", "white", "white"),
                color.border = "black",  
                borderWidth = 2,
                font = list(size = 22)
            )
            
            # ---- EDGES ----
            edges <- data.frame(
                from = c("H1_NI","H1_NI","H2_NI","H1_S" ,"H2_NI","H1_S"),
                to   = c("H1_S" ,"H2_NI","H1_S" ,"H2_NI","H2_S" ,"H2_S"),
                label = c("ν"   , "1−ν" , "ω"   , "τ"   , "1−ω" , "1−τ"),
                arrows = "to",
                smooth = c(FALSE, FALSE, TRUE, TRUE, FALSE, FALSE),
                font = list(size = 20, align = "top")
            )
            
            visNetwork(
                    nodes, 
                    edges, 
                    width = "100%", 
                    height = "700px") |>
                visEdges(arrows = "to",
                    smooth = list(type = "curvedCW", roundness = 0.3)) |>
                visNodes(
                    shape = "circle",
                    borderWidth = 2,
                    color = list(
                        background = "white",
                        border = "black"
                    ),
                    scaling = list(label = list(enabled = TRUE, min = 10, max = 30))
                ) |>
                visInteraction(
                    dragNodes = TRUE,
                    dragView = TRUE,
                    zoomView = TRUE
                ) |>
                visPhysics(enabled = FALSE)  # <– wichtig für gMCP-ähnliches Verhalten
        })
}

shinyApp(ui, server)

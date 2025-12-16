
#setwd("D:/R/workspace/alphagraph")

library(shiny)
library(bs4Dash)
library(visNetwork)
library(DT)
library(markdown)

options("shiny.autoload.r" = FALSE)

srcFiles <- list.files("R", pattern = "\\.R$")
for (src in srcFiles) {
    source(file.path("R", src),
        local = FALSE,
        encoding = "utf-8"
    )
}

shiny::shinyApp(
    ui = createUI, 
    server = createServer)



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

registerResourcePath <- function(dir) {
    resourceDir <- getSystemFile(dir)
    if (identical(resourceDir, "")) {
        stop("Could not find shiny app directory ", sQuote(dir), ". ",
            "Try re-installing `rpact.cloud`.", call. = FALSE)
    }
    shiny::addResourcePath(dir, resourceDir)
}

registerResourcePath("www")

shiny::shinyApp(
    ui = createUI, 
    server = createServer)


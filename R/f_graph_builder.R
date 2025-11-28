getHypothesisLabel <- function(number, x, i = NULL) {
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
    label <- paste0("H", number, "\u208D", x, "\u208E")
    if (!is.null(i)) {
        label <- paste0(label, "\n", i)
    }
    return(label)
}

getDemoGraph <- function() {
    # ---- NODES ----
    nodes <- data.frame(
        id = c("H1_NI", "H1_S", "H2_NI", "H2_S"),
        label = c(
            getHypothesisLabel(1, "ni", 1),
            getHypothesisLabel(1, "s", 0),
            getHypothesisLabel(2, "ni", 0),
            getHypothesisLabel(2, "s", 0)
        ),
        # label = c("H1,NI\n1", "H1,S\n0", "H2,NI\n0", "H2,S\n0"),
        x = c(0, 300, 0, 300),
        y = c(0, 0, 200, 200),
        shape = "circle",
        color.background = c("#7CFF7C", "white", "white", "white"),
        color.border = "black",
        borderWidth = 2,
        font = list(size = 22)
    )

    # ---- EDGES ----
    edges <- data.frame(
        from = c("H1_NI", "H1_NI", "H2_NI", "H1_S", "H2_NI", "H1_S"),
        to = c("H1_S", "H2_NI", "H1_S", "H2_NI", "H2_S", "H2_S"),
        label = c("\u03BD", "1-\u03BD", "\u03C9", "t", "1-\u03C9", "1-t"),
        arrows = "to",
        smooth = c(FALSE, FALSE, TRUE, TRUE, FALSE, FALSE),
        font = list(size = 20, align = "top")
    )

    visNetwork::visNetwork(
        nodes,
        edges,
        width = "100%",
        height = "700px"
    ) |>
        visNetwork::visEdges(
            arrows = "to",
            smooth = list(type = "curvedCW", roundness = 0.3)
        ) |>
        visNetwork::visNodes(
            shape = "circle",
            borderWidth = 2,
            color = list(
                background = "white",
                border = "black"
            ),
            scaling = list(label = list(enabled = TRUE, min = 10, max = 30))
        ) |>
        visNetwork::visInteraction(
            dragNodes = TRUE,
            dragView = TRUE,
            zoomView = TRUE
        ) |>
        visNetwork::visPhysics(enabled = FALSE)
}

getGraphDescription <- function() {
    mdText <- 
"### Graph representing the procedure from Hung and Wang (2010)

**H1,NI**: Non-inferiority of the primary endpoint  
**H1,S**: Superiority of the primary endpoint  
**H2,NI**: Non-inferiority of the secondary endpoint  
**H2,S**: Superiority of the secondary endpoint  

**Literature:**  

Hung H.M.J., Wang S.-J. (2010). *Challenges to multiple testing in clinical trials.*  
*Biometrical Journal*, 52, 747-756."
    return(mdText)
}

getGraphDescriptionFile <- function() {
    mdFile <- tempfile(fileext = ".md")
    writeLines(text = getGraphDescription(), con = mdFile)
    return(mdFile)
}

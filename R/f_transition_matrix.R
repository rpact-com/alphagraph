
getTransitionMatrix <- function() {
    mat <- matrix(c(
        0, "\u03BD", "1-\u03BD", 0,
        0, 0, "\u03C9", "1-t",
        0, 0, 0, "1-\u03C9",
        0, 0, 0, 0
    ), 4, 4, byrow = TRUE)
    
    #rownames(mat) <- colnames(mat) <- c("H1,NI", "H1,S", "H2,NI", "H2,S")
    rownames(mat) <- colnames(mat) <- c(
        getHypothesisLabel(1, "ni"),
        getHypothesisLabel(1, "s"),
        getHypothesisLabel(3, "ni"),
        getHypothesisLabel(3, "s")
    )
    
    return(mat)
}
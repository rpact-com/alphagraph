
#' 
#' @title Example transition matrix for the demo graph
#'
#' @description
#' Return a 4x4 character matrix that represents the transitions and
#' associated symbolic probabilities used in the demo graph visualization.
#' Row and column names are produced via getHypothesisLabel().
#'
#' @return A 4x4 character matrix representing the example transition matrix.
#'
#' @keywords internal
#' 
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
##
## alphagraph
##
## This file is part of the R package alphagraph
##
## Author: Friedrich Pahlke, PhD
## Licensed under "GNU Lesser General Public License" version 3
## License text can be found here: https://www.r-project.org/Licenses/LGPL-3
##
## RPACT company website: https://www.rpact.com
##

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
    createDemoAlphaGraph()$toTransitionMatrix(
        type = "display",
        asDataFrame = FALSE,
        rowNames = "label"
    )
}

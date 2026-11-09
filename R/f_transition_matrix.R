##
## alphagraph
##
## This file is part of the R package alphagraph
##
## Author: Friedrich Pahlke, PhD
## Licensed under "GNU Affero General Public License" version 3.0
## License text can be found here: https://cran.r-project.org/web/licenses/AGPL-3
##
## RPACT company website: https://www.rpact.com
##

#' 
#' @title Example transition matrix for the demo graph
#'
#' @description
#' Return a 4x4 character matrix that represents the numeric transitions and
#' their fraction-formatted edge weights used in the demo graph visualization.
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

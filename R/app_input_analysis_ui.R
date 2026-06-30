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

 #' @title Analysis panel UI module
 #'
 #' @description
 #' UI definition for the analysis output panel. This module provides a
 #' placeholder verbatim text output where analysis results can be displayed.
 #'
 #' @param id Module id (namespace) used for UI namespacing.
 #'
 #' @return UI elements for the analysis panel.
 #'
 #' @keywords internal
moduleAnalysisUI <- function(id) {
    ns <- shiny::NS(id)
    
    shiny::verbatimTextOutput(ns("analysis"))
}

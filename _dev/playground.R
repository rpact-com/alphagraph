# TODO: Add comment
# 
# Author: Friedrich Pahlke
###############################################################################


# multtest
# graph
if (!require("BiocManager", quietly = TRUE)) install.packages("BiocManager")
BiocManager::install("graph")
BiocManager::install("multtest")
devtools::install_github("kornl/gMCP", subdir="pkg/gMCP", dependencies = TRUE, build_vignettes = TRUE)


# Achtung: Funktioniert nur in der einfachen RGui.exe!
library(gMCP)
#library(rJava)
graphGUI()
vignette("gMCP")

# https://merck.github.io/gMCPLite/
remotes::install_github("Merck/gMCPLite")
library(gMCPLite)
hGraph()

sessionInfo()
packageVersion("gMCP")
packageVersion("rJava")
R.version.string
library(rJava)
.jinit()
.jcall("java/lang/System", "S", "getProperty", "java.runtime.version")

sessionInfo()
system.file("jri", package = "rJava")
# "C:/R/R-4.5.2/library/rJava/jri.dll"
# (?) "C:/R/R-4.5.2/library/rJava/x64/jri.dll"

# https://github.com/openpharma/graphicalMCP
install.packages("graphicalMCP")
library(graphicalMCP)
vignette("graph-examples")

# https://github.com/Merck/gMCPShiny/
# https://rinpharma.shinyapps.io/gmcp/
remotes::install_github("Merck/gMCPShiny")
gMCPShiny::run_app()


# https://mrc-bsu.shinyapps.io/20MRC_BSU_GraphApp/

# https://r-graph-gallery.com/network-interactive.html
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
#' @title Check whether current system belongs to a local developer
#'
#' @description
#' Return TRUE when the current user account matches one of the known local
#' developer accounts. This is used to enable developer-only logging and
#' diagnostics. The check is resilient and returns FALSE on error.
#'
#' @param developers Character vector of developer user names to accept.
#'
#' @return Logical scalar: TRUE if running on a local developer system,
#' FALSE otherwise.
#'
#' @keywords internal
#' 
isLocalDeveloperSystem <- function(developers = c("fried")) {
    tryCatch(
        {
            if (isAppRunningAsAPackage()) {
                return(FALSE)
            }

            return(Sys.info()[["user"]] %in% developers)
        },
        error = function(e) {
            return(FALSE)
        }
    )
}

#' 
#' @title Detect whether the app is running as an installed package
#'
#' @description
#' Attempt to determine whether the app code is running from a package
#' installation or from a source (project) directory. This helps to adapt
#' behavior (e.g. resource lookup) depending on packaging mode.
#'
#' @param loggingEnabled Logical; if TRUE, logging is enabled for error messages.
#'
#' @return Logical scalar indicating whether the app is running as a package.
#'
#' @keywords internal
#' 
isAppRunningAsAPackage <- function(loggingEnabled = TRUE) {
    tryCatch({
            appDir <- here::here()
            if (!dir.exists(file.path(appDir, "R")) ||
                length(list.files(file.path(appDir, "R"), pattern = "\\.R$")) == 0) {
                return(TRUE)
            }
        }, error = function(e) {
            msg <- paste0("Failed to check whether app is running as a package: ", e$message)
            if (loggingEnabled) {
                logError(msg)
            } else {
                message(msg)
            }
        })

    return(!isFALSE(as.logical(getOption("rpact.cloud.package.mode", TRUE))))
}

#' 
#' @title Resolve a path to a system resource file
#'
#' @description
#' Return the correct file path for resources bundled with the package when
#' the app is installed, or the corresponding 'inst/' path when running from
#' the project source tree.
#'
#' @param path Relative path inside the package resources (e.g. "www/foo.css").
#'
#' @return A string with the absolute file path to the requested resource.
#'
#' @keywords internal
#' 
getSystemFile <- function(path) {
    if (isAppRunningAsAPackage() && "alphagraph" %in% rownames(utils::installed.packages())) {
        return(system.file(path, package = "alphagraph"))
    }

    return(file.path(here::here(), 'inst', path))
}

.getClassName <- function(x) {
    return(as.character(class(x))[1])
}

#' 
#' @title Include JavaScript in Shiny UI
#'
#' @description
#' Helper that creates a script tag for including a JavaScript file in the
#' Shiny UI. The path is passed through to the tag; when packaging the app
#' callers typically use getSystemFile() to resolve resource paths.
#'
#' @param path Path or URL to the JavaScript file to include.
#'
#' @return A Shiny tag containing a script include.
#'
#' @keywords internal
#' 
includeJavaScript <- function(path) {
    # shiny::includeScript(path = getSystemFile(path))
    shiny::tags$script(src = path, defer = NA)
}

isMaximumPackageVersion <- function(maxVersion, pkg, ..., inclusive = TRUE) {
    tryCatch(
        {
            if (isFALSE(inclusive)) {
                return(utils::packageVersion(pkg) < maxVersion)
            }

            return(utils::packageVersion(pkg) <= maxVersion)
        },
        error = function(e) {
            stop("Failed to check if package ", sQuote(pkg), " has version ", maxVersion, " or older: ", e$message)
        }
    )
    return(TRUE)
}
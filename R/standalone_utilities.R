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

getSystemFile <- function(path) {
    if (isAppRunningAsAPackage() && "alphagraph" %in% rownames(utils::installed.packages())) {
        return(system.file(path, package = "alphagraph"))
    }

    return(file.path(here::here(), 'inst', path))
}

.getClassName <- function(x) {
    return(as.character(class(x))[1])
}

includeJavaScript <- function(path) {
    # shiny::includeScript(path = getSystemFile(path))
    tags$script(src = path, defer = NA)
}
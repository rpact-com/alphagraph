isLocalDeveloperSystem <- function(developers = c("fried")) {
    tryCatch(
        {
            if (isAppRunningAsAPackage()) {
                return(FALSE)
            }

            if (isProductionTestModeEnabled()) {
                return(FALSE)
            }

            return(Sys.info()[["user"]] %in% developers)
        },
        error = function(e) {
            return(FALSE)
        }
    )
}
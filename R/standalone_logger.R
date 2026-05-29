##
## RPACT Cloud
##
## This file is part of the R package rpact.cloud
##
## Author: Friedrich Pahlke, PhD
## Copyright (c) 2026 by RPACT
## All rights reserved.
##
## RPACT company website: https://www.rpact.com
##
## Learn more about RPACT Cloud at www.rpact.com/products/rpact-cloud/
##

getSessionInfo <- function() {
    if (!isLocalDeveloperSystem()) {
        return("")
    }

    session <- shiny::getDefaultReactiveDomain()
    sessionId <- ifelse(is.null(session), "unknown", session$token)
    return(paste0(" [session = ", sQuote(sessionId), "]"))
}

.log <- function(...,
        logType = c("INFO", "DEBUG", "WARNING", "ERROR", "TRACE", "TIME"),
        showOnlyInLocalDevMode = FALSE,
        once = FALSE) {
    if (isTRUE(showOnlyInLocalDevMode) && !isLocalDeveloperSystem()) {
        return(invisible())
    }

    if (isFALSE(as.logical(.GlobalEnv$C_LOGGING_ENABLED))) {
        return(invisible())
    }

    if (!isLoggingEnabled(tolower(logType))) {
        return(invisible())
    }

    logType <- match.arg(logType)
    time <- format(Sys.time(), format = "%H:%M:%S")
    args <- list(...)
    line <- ""
    msg <- ""
    tryCatch(
        {
            if (length(args) == 1 && is.list(args[[1]])) {
                line <- paste0(line, "[", time, "] ", logType, ":\n")
                args <- args[[1]]
                for (param in names(args)) {
                    value <- args[[param]]
                    if (isS4(value) || R6::is.R6(value)) {
                        value <- .getClassName(value)
                    }
                    msg <- paste0(msg, "   ", param, " = ", value, "\n")
                }
                line <- paste0(line, msg)
            } else {
                args <- unlist(args)
                line <- paste0(line, "[", time, "] ", logType)
                if (length(args) > 0) {
                    msg <- paste0(args, collapse = "")
                    line <- paste0(line, ": ", msg)
                }
            }
            if (nchar(trimws(line)) > 0) {
                line <- paste0(line, getSessionInfo())
            }
        },
        error = function(e) {
            warning("Failed to create log message: ", e$message, call. = FALSE)
        }
    )

    cat(line, "\n",
        file = stderr(), sep = "",
        fill = FALSE, labels = NULL, append = FALSE
    )

    if (isLocalDeveloperSystem() && dir.exists(file.path(getwd(), "_dev"))) {
        write(line, file = file.path(getwd(), "_dev", "log.txt"), append = TRUE)
    }
    return(invisible())
}

logInfo <- function(...) {
    .log(logType = "INFO", ...)
}

logWarning <- function(...) {
    .log(logType = "WARNING", ...)
}

logDebug <- function(...) {
    .log(logType = "DEBUG", ...)
}

logTrace <- function(...) {
    .log(logType = "TRACE", ...)
}

logError <- function(...) {
    .log(logType = "ERROR", ...)
}

getRuntimeStrings <- function(startTimes, ..., endTimes = rep(Sys.time(), length(startTimes)),
        runtimeUnits = c("secs", "auto"), addBrackets = FALSE) {
    s <- character(0)
    for (i in 1:length(startTimes)) {
        startTime <- startTimes[i]
        endTime <- endTimes[i]
        s <- c(s, getRuntimeString(
            startTime = startTime, endTime = endTime,
            runtimeUnits = runtimeUnits, addBrackets = addBrackets
        ))
    }
    return(s)
}

C_TRY_TIME_FORMATS <- c(
    "%Y-%m-%d %H:%M:%S",
    "%Y-%m-%d %H:%M:%OS",
    "%Y/%m/%d %H:%M:%OS",
    "%Y-%m-%d %H:%M",
    "%Y/%m/%d %H:%M",
    "%d.%m.%Y %H:%M:%S",
    "%d.%m.%Y %H:%M",
    "%Y-%m-%d",
    "%Y/%m/%d"
)

stringToTime <- function(s) {
    if (nchar(trimws(s)) == 0) {
        return(NA)
    }

    if (grepl("^\\d{4,4}-\\d{2,2}-\\d{2,2} \\d{2,2}:\\d{2,2}:\\d{2,2}$", s)) {
        return(strptime(s, "%Y-%m-%d %H:%M:%S"))
    }

    if (grepl("^\\d{4,4}-\\d{2,2}-\\d{2,2} \\d{2,2}:\\d{2,2}$", s)) {
        return(strptime(s, "%Y-%m-%d %H:%M"))
    }

    if (grepl("^\\d{4,4}-\\d{2,2}-\\d{2,2} \\d{2,2}:\\d{2,2}:\\d{2,2} [A-Z]{2,5}$", s)) {
        d <- regexpr(" [A-Z]{2,5}$", s)
        index <- attr(d, "match.length")
        timeZone <- substr(s, d[1] + 1, d[1] + index - 1)
        strWithoutTimeZone <- substr(s, 1, d[1] - 1)
        timeValue <- as.POSIXlt(strWithoutTimeZone, tryFormats = C_TRY_TIME_FORMATS, tz = "Europe/Berlin")
        return(timeValue)
    }

    tryCatch(
        {
            return(as.POSIXlt(s, tryFormats = C_TRY_TIME_FORMATS))
        },
        error = function(e) {
            logError("Failed to coerce time string ", sQuote(s), ": ", e$message)
        }
    )

    return(NA)
}

formatDurationAsString <- function(timeSeconds) {
    if (is.na(timeSeconds)) {
        return(NA_character_)
    }

    secs <- as.numeric(timeSeconds)
    decimalMark <- "."
    if (secs <= 60) {
        seconds <- format(round(secs, 2), decimal.mark = decimalMark)
        secondsUnit <- "sec"
        if (identical(seconds, "1")) {
            secondsUnit <- sub("(n|s)$", "", secondsUnit)
        }
        timeStr <- paste0(seconds, " ", secondsUnit)
    } else if (secs <= 3600) {
        minutes <- round(secs / 60, 2)
        minutesUnit <- "min"
        if (minutes == 1) {
            minutesUnit <- sub("(n|s)$", "", minutesUnit)
        }
        timeStr <- paste0(format(minutes, decimal.mark = decimalMark), " ", minutesUnit)
    } else {
        hours <- round(secs / 3600, 2)
        hoursUnit <- "h"
        if (hours == 1) {
            hoursUnit <- sub("(n|s)$", "", hoursUnit)
        }
        timeStr <- paste0(format(hours, decimal.mark = decimalMark), " ", hoursUnit)
    }
    return(timeStr)
}

getRuntimeString <- function(startTime,
        ...,
        endTime = Sys.time(),
        runtimeUnits = c("secs", "auto"),
        addBrackets = FALSE) {
    runtimeUnits <- match.arg(runtimeUnits)

    if (is.null(startTime) || is.null(endTime) || is.na(startTime) || is.na(endTime)) {
        return(NA_character_)
    }

    if (is.character(startTime)) {
        if (nchar(trimws(startTime)) == 0) {
            return(NA_character_)
        }

        startTime <- stringToTime(startTime)
        if (is.na(startTime)) {
            return(NA_character_)
        }
    }
    if (is.character(endTime)) {
        if (nchar(trimws(endTime)) == 0) {
            return(NA_character_)
        }

        endTime <- stringToTime(endTime)
        if (is.na(endTime)) {
            return(NA_character_)
        }
    }

    timeSeconds <- as.numeric(difftime(endTime, startTime, units = "secs"))
    if (timeSeconds < 0) {
        return(NA_character_)
    }

    timeStr <- formatDurationAsString(timeSeconds = timeSeconds)

    if (addBrackets) {
        timeStr <- paste0("[", timeStr, "]")
    }

    if (length(timeStr) == 0) {
        timeStr <- NA_character_
    }

    return(timeStr)
}

logTime <- function(s,
        ...,
        startTime,
        endTime = Sys.time(),
        runtimeUnits = c("auto", "secs")) {
    runtimeUnits <- match.arg(runtimeUnits)
    timeStr <- getRuntimeString(
        startTime = startTime,
        endTime = endTime,
        runtimeUnits = runtimeUnits,
        addBrackets = TRUE
    )
    .log(logType = "TIME", s, ..., " ", timeStr)
}

getSessionInfoLogLines <- function(logInstalledPackages = FALSE) {
    tryCatch(
        {
            si <- utils::sessionInfo()
            output <- character(0)

            h2 <- function(x) paste0("## ", x)

            output <- c(output, "# Session Info", "\n\n")

            output <- c(output, paste0(shiny::tags$h2("R Version"), "\n\n"))
            output <- c(output, paste0("- Version: ", si$R.version$version.string, "\n"))
            output <- c(output, paste0("- Platform: ", si$R.version$platform, "\n"))
            output <- c(output, paste0("- Running under: ", si$running, "\n\n"))

            output <- c(output, paste0(shiny::tags$h2("Matrix Products"), "\n\n"))
            output <- c(output, paste0("- ", si$matprod, "\n\n"))

            output <- c(output, paste0(shiny::tags$h2("Locale"), "\n\n"))
            for (locale in strsplit(si$locale, ";")[[1]]) {
                output <- c(output, paste0("- ", sub("=", " = ", locale, fixed = TRUE), "\n"))
            }
            output <- c(output, "\n")

            output <- c(output, paste0(shiny::tags$h2("Time Zone"), "\n\n"))
            output <- c(output, paste0("- Time Zone: ", si$tzone, "\n"))
            output <- c(output, paste0("- TZCode Source: ", si$tzcode_type, "\n\n"))

            output <- c(output, paste0(shiny::tags$h2("Attached Base Packages"), "\n\n"))
            for (pkg in si$basePkgs) {
                output <- c(output, paste0("- ", pkg, "\n"))
            }
            output <- c(output, "\n")

            output <- c(output, paste0(shiny::tags$h2("Other Attached Packages"), "\n\n"))
            for (pkg in names(si$otherPkgs)) {
                tryCatch(
                    {
                        output <- c(output, paste0("- ", pkg, ": ", si$otherPkgs[[pkg]]$Version, "\n"))
                    },
                    error = function(e) {
                        logError("Failed to log attached ", sQuote(pkg), " package info: ", e$message)
                    }
                )
            }
            output <- c(output, "\n")

            output <- c(output, paste0(shiny::tags$h2("Loaded via a Namespace (and not attached)"), "\n\n"))
            for (pkg in names(si$loadedOnly)) {
                tryCatch(
                    {
                        output <- c(output, paste0("- ", pkg, ": ", si$loadedOnly[[pkg]]$Version, "\n"))
                    },
                    error = function(e) {
                        logError("Failed to log loaded ", sQuote(pkg), " package info: ", e$message)
                    }
                )
            }
            output <- c(output, "\n")

            if (isTRUE(logInstalledPackages)) {
                installedPackages <- utils::installed.packages()[, c("Package", "Version")]
                output <- c(output, paste0(shiny::tags$h2("Installed Packages"), "\n\n"))
                for (i in 1:nrow(installedPackages)) {
                    pkg <- installedPackages[i, "Package"]
                    tryCatch(
                        {
                            vers <- installedPackages[i, "Version"]
                            output <- c(output, paste0("- ", pkg, ": ", vers, "\n"))
                        },
                        error = function(e) {
                            logError("Failed to log installed ", sQuote(pkg), " package info: ", e$message)
                        }
                    )
                }
                output <- c(output, "\n")
            }

            openTelemtryVars <- Sys.getenv(c(
                "OTEL_SDK_DISABLED",
                "OTEL_TRACES_EXPORTER",
                "OTEL_LOGS_EXPORTER",
                "OTEL_METRICS_EXPORTER",
                "OTEL_EXPORTER_OTLP_ENDPOINT",
                "OTEL_EXPORTER_OTLP_PROTOCOL"
            ), unset = "<unset>")
            output <- c(output, "\n")
            output <- c(output, paste0(shiny::tags$h2("OpenTelemetry related environment variables"), "\n\n"))
            for (var in names(openTelemtryVars)) {
                output <- c(output, paste0("- ", var, ": ", openTelemtryVars[[var]], "\n"))
            }
            output <- c(output, "\n")

            return(output)
        },
        error = function(e) {
            msg <- paste("Failed to log session info: ", e$message)
            logError(msg)
            return(msg)
        }
    )
}

logSessionInfo <- function(silent = isLocalDeveloperSystem()) {
    if (isTRUE(silent)) {
        return(invisible())
    }

    cat(paste(getSessionInfoLogLines(), collapse = ""))
}

setAppLogLevel <- function(logLevel) {
    if (is.null(logLevel) || length(logLevel) != 1 || is.na(logLevel) ||
            !is.character(logLevel) || nchar(trimws(logLevel)) == 0) {
        logLevel <- "trace"
    }
    logLevel <- tolower(logLevel)
    if (!logLevel %in% c("trace", "debug", "info", "warn", "error", "disabled")) {
        logLevel <- "trace"
    }
    Sys.setenv("RPACT_CLOUD_LOG_LEVEL" = logLevel)
}

#'
#' @examples
#' setAppLogLevel("warn")
#' isLoggingEnabled("info")
#' isLoggingEnabled("error")
#'
#' @noRd
#'
isLoggingEnabled <- function(logLevel) {
    logLevel <- tolower(logLevel)
    if (identical(logLevel, "disabled")) {
        return(FALSE)
    }

    minLogLevel <- Sys.getenv("RPACT_CLOUD_LOG_LEVEL")
    if (identical(minLogLevel, "")) {
        minLogLevel <- "trace"
        Sys.setenv("RPACT_CLOUD_LOG_LEVEL" = minLogLevel)
    }

    if (identical(logLevel, "time")) {
        logLevel <- "info"
    }

    availableLogLevels <- c("trace", "debug", "info", "warning", "error")
    pos <- which(availableLogLevels == minLogLevel)
    availableLogLevels <- availableLogLevels[pos:length(availableLogLevels)]
    return(logLevel %in% availableLogLevels)
}

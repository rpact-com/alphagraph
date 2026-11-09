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
#' @title Get a short session identifier for logging
#'
#' @description
#' Return a short string identifying the current Shiny session for use in
#' log messages. On non-developer systems an empty string is returned.
#'
#' @return A short string with session information (possibly empty).
#'
#' @keywords internal
#' 
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

#' 
#' @title Log an informational message
#'
#' @description
#' Convenience wrapper to create an INFO level log entry. Accepts the same
#' arguments as the internal logging helper.
#'
#' @param ... Arguments passed to the logger; these are concatenated.
#'
#' @return Invisibly returns NULL.
#'
#' @keywords internal
#' 
logInfo <- function(...) {
    .log(logType = "INFO", ...)
}

#' 
#' @title Log a warning message
#'
#' @description
#' Convenience wrapper to create a WARNING level log entry.
#'
#' @param ... Arguments passed to the logger.
#'
#' @return Invisibly returns NULL.
#'
#' @keywords internal
#' 
logWarning <- function(...) {
    .log(logType = "WARNING", ...)
}

#' 
#' @title Log a debug message
#'
#' @description
#' Convenience wrapper to create a DEBUG level log entry. Useful for
#' developer diagnostics and verbose tracing.
#'
#' @param ... Arguments passed to the logger.
#'
#' @return Invisibly returns NULL.
#'
#' @keywords internal
#' 
logDebug <- function(...) {
    .log(logType = "DEBUG", ...)
}

#' 
#' @title Log a trace message
#'
#' @description
#' Convenience wrapper to create a TRACE level log entry. TRACE is the most
#' verbose level and intended for fine-grained diagnostics.
#'
#' @param ... Arguments passed to the logger.
#'
#' @return Invisibly returns NULL.
#'
#' @keywords internal
#' 
logTrace <- function(...) {
    .log(logType = "TRACE", ...)
}

#' 
#' @title Log an error message
#'
#' @description
#' Convenience wrapper to create an ERROR level log entry.
#'
#' @param ... Arguments passed to the logger.
#'
#' @return Invisibly returns NULL.
#'
#' @keywords internal
#' 
logError <- function(...) {
    .log(logType = "ERROR", ...)
}

#' 
#' @title Format multiple runtimes as strings
#'
#' @description
#' Given vectors of start (and optionally end) times, return a character
#' vector with human-friendly runtime descriptions.
#'
#' @param startTimes Vector of start times (POSIXct/POSIXlt or character).
#' @param ... Additional arguments passed to getRuntimeString.
#' @param endTimes Vector of end times; recycled to match startTimes if needed.
#' @param runtimeUnits Character, either "secs" or "auto"; passed to formatter.
#' @param addBrackets Logical; if TRUE the returned strings are wrapped in brackets.
#'
#' @return Character vector with formatted runtime strings.
#'
#' @keywords internal
#' 
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

#' 
#' @title Tried time formats for parsing
#'
#' @description
#' A small vector of time format strings that are used as candidates when
#' attempting to coerce character time values into POSIXlt values.
#'
#' @keywords internal
#' 
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

#' 
#' @title Coerce a string to POSIXlt time
#'
#' @description
#' Attempt to parse a variety of commonly used time/date string formats into
#' a POSIXlt object. On failure NA is returned and an error is logged.
#'
#' @param s Character scalar with the time representation.
#'
#' @return POSIXlt object on success or NA on failure.
#'
#' @keywords internal
#' 
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

#' 
#' @title Format a duration in seconds as a human readable string
#'
#' @description
#' Convert a numeric duration given in seconds into a short readable string
#' using seconds, minutes or hours depending on magnitude.
#'
#' @param timeSeconds Numeric number of seconds (or NA).
#'
#' @return Character scalar with formatted duration or NA_character_.
#'
#' @keywords internal
#' 
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

#' 
#' @title Format runtime between two times
#'
#' @description
#' Compute and format the elapsed time between start and end times. Inputs
#' can be POSIX objects or character strings parsable by stringToTime().
#'
#' @param startTime Start time (POSIXct/POSIXlt or character).
#' @param ... Additional arguments (currently unused).
#' @param endTime End time (defaults to now).
#' @param runtimeUnits Character option: "secs" or "auto".
#' @param addBrackets Logical; if TRUE result is wrapped in brackets.
#'
#' @return Formatted runtime string or NA_character_.
#'
#' @keywords internal
#' 
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

#' 
#' @title Log a timed message with elapsed time
#'
#' @description
#' Log a message and append the elapsed time computed between startTime and
#' endTime. The elapsed time is formatted and appended to the message.
#'
#' @param s Message text to log (character).
#' @param ... Additional values concatenated to the message.
#' @param startTime Start time for timing (POSIX or character).
#' @param endTime End time (defaults to Sys.time()).
#' @param runtimeUnits Units formatting preference passed to getRuntimeString.
#'
#' @return Invisibly NULL.
#'
#' @keywords internal
#' 
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

#' 
#' @title Produce lines describing the current session environment
#'
#' @description
#' Collect session information (R version, attached packages, loaded
#' namespaces, environment variables etc.) and return it as a character
#' vector suitable for saving or display.
#'
#' @param logInstalledPackages Logical; if TRUE also list all installed packages.
#'
#' @return Character vector with lines describing the session information.
#'
#' @keywords internal
#' 
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

#' 
#' @title Print session information unless silenced
#'
#' @description
#' Wrapper that prints the output of getSessionInfoLogLines() unless the
#' 'silent' argument is TRUE. By default silent is TRUE on developer systems.
#'
#' @param silent Logical; when TRUE nothing is printed.
#'
#' @return Invisibly NULL.
#'
#' @keywords internal
#' 
logSessionInfo <- function(silent = isLocalDeveloperSystem()) {
    if (isTRUE(silent)) {
        return(invisible())
    }

    cat(paste(getSessionInfoLogLines(), collapse = ""))
}

#' 
#' @title Set the application log level
#'
#' @description
#' Set the minimum log level for the application by modifying the
#' RPACT_CLOUD_LOG_LEVEL environment variable. Accepted levels are
#' "trace", "debug", "info", "warn", "error", and "disabled".
#'
#' @param logLevel Character string specifying the minimum log level.
#'
#' @return Invisibly NULL.
#'
#' @keywords internal
#' 
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
#' @title Check whether a log level is enabled
#'
#' @description
#' Determine whether messages with the specified log level should be emitted
#' based on the current application minimum log level stored in the
#' RPACT_CLOUD_LOG_LEVEL environment variable. The comparison is performed
#' case-insensitively. The special level "disabled" always returns FALSE and
#' the alias "time" is treated as "info".
#'
#' @param logLevel Character scalar indicating the log level to test. Supported
#' values include "trace", "debug", "info", "warning", "error",
#' "time" and "disabled".
#'
#' @return Logical scalar: TRUE if messages at the given level should be
#' logged, FALSE otherwise.
#'
#' @examples
#' \dontrun{
#' setAppLogLevel("warn")
#' isLoggingEnabled("info")
#' isLoggingEnabled("error")
#' }
#' 
#' @keywords internal
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

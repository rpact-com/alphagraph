##
## alphagraph
##
## Numeric input and presentation helpers. Canonical graph values remain
## numeric; fractions are accepted and produced only at the UI/model boundary.
##

#' @title Parse and format AlphaGraph numeric values
#'
#' @description
#' Strictly parse decimal and fraction input while keeping canonical graph
#' values numeric, and format numeric values as fractions when the configured
#' complexity limit permits it.
#'
#' @param x A scalar number, fraction string, or matrix-like object.
#' @param name Name used in validation messages.
#' @param fractions Whether simple fractions should be displayed.
#' @param maxDenominator Largest denominator allowed in a displayed fraction.
#' @param tolerance Maximum relative approximation error for fraction display.
#' @param digits Number of significant digits used for decimal display.
#' @param matrix A numeric matrix.
#' @param settings Number-format settings from an `AlphaGraph`.
#'
#' @return A numeric value or matrix when parsing, and character output when
#'   formatting.
#'
#' @name alphaGraphNumberFormat
#' @keywords internal
NULL

#' @rdname alphaGraphNumberFormat
alphaGraphParseNumber <- function(x, name = "value") {
    if (is.numeric(x) || is.logical(x)) {
        if (length(x) != 1L || is.na(x) || !is.finite(x)) {
            stop(name, " must be one finite number.", call. = FALSE)
        }
        return(as.numeric(x))
    }

    if (!is.character(x) || length(x) != 1L || is.na(x)) {
        stop(name, " must be a number or a fraction such as 1/2.", call. = FALSE)
    }

    text <- trimws(x)
    numberPattern <- "^[+-]?(?:[0-9]+(?:\\.[0-9]*)?|\\.[0-9]+)(?:[eE][+-]?[0-9]+)?$"
    parsePart <- function(part) {
        if (!grepl(numberPattern, part, perl = TRUE)) {
            return(NA_real_)
        }
        suppressWarnings(as.numeric(part))
    }

    parts <- strsplit(text, "/", fixed = TRUE)[[1L]]
    if (length(parts) == 1L) {
        value <- parsePart(trimws(parts))
    } else if (length(parts) == 2L) {
        numerator <- parsePart(trimws(parts[[1L]]))
        denominator <- parsePart(trimws(parts[[2L]]))
        if (is.na(denominator) || denominator == 0) {
            stop(name, " has an invalid or zero denominator.", call. = FALSE)
        }
        value <- numerator / denominator
    } else {
        value <- NA_real_
    }

    if (length(value) != 1L || is.na(value) || !is.finite(value)) {
        stop(name, " must be a number or a fraction such as 1/2.", call. = FALSE)
    }
    value
}

#' @rdname alphaGraphNumberFormat
alphaGraphTryParseNumber <- function(x) {
    tryCatch(alphaGraphParseNumber(x), error = function(error) NULL)
}

#' @rdname alphaGraphNumberFormat
alphaGraphFormatNumber <- function(
        x,
        fractions = TRUE,
        maxDenominator = 16L,
        tolerance = 1e-8,
        digits = 8L) {
    if (is.null(x) || length(x) != 1L || is.na(x) || !is.finite(x)) {
        return(NULL)
    }

    x <- as.numeric(x)
    maxDenominator <- max(1L, as.integer(maxDenominator))
    tolerance <- max(0, as.numeric(tolerance))
    digits <- max(1L, as.integer(digits))

    if (isTRUE(fractions)) {
        denominators <- seq_len(maxDenominator)
        numerators <- round(x * denominators)
        errors <- abs(x - numerators / denominators)
        best <- which.min(errors)
        scale <- max(1, abs(x))
        if (errors[[best]] <= tolerance * scale) {
            numerator <- numerators[[best]]
            denominator <- denominators[[best]]
            divisor <- .alphaGraphGreatestCommonDivisor(abs(numerator), denominator)
            numerator <- numerator / divisor
            denominator <- denominator / divisor
            if (denominator == 1) {
                return(format(numerator, trim = TRUE, scientific = FALSE))
            }
            return(paste0(numerator, "/", denominator))
        }
    }

    format(signif(x, digits = digits), trim = TRUE, scientific = FALSE)
}

.alphaGraphGreatestCommonDivisor <- function(a, b) {
    a <- as.integer(round(a))
    b <- as.integer(round(b))
    while (b != 0L) {
        remainder <- a %% b
        a <- b
        b <- remainder
    }
    max(1L, a)
}

#' @rdname alphaGraphNumberFormat
alphaGraphFormatMatrix <- function(matrix, settings = list()) {
    matrix <- as.matrix(matrix)
    formatted <- vapply(
        as.vector(matrix),
        alphaGraphFormatNumber,
        character(1L),
        fractions = settings$enabled %||% TRUE,
        maxDenominator = settings$maxDenominator %||% 16L,
        tolerance = settings$tolerance %||% 1e-8,
        digits = settings$digits %||% 8L
    )
    dim(formatted) <- dim(matrix)
    dimnames(formatted) <- dimnames(matrix)
    formatted
}

#' @rdname alphaGraphNumberFormat
alphaGraphParseMatrix <- function(x) {
    if (is.list(x) && !is.data.frame(x) && !is.matrix(x)) {
        x <- do.call(rbind, lapply(x, unlist, use.names = FALSE))
    }
    matrix <- as.matrix(x)
    parsed <- vapply(
        as.vector(matrix),
        alphaGraphParseNumber,
        numeric(1L),
        name = "Transition matrix cell"
    )
    dim(parsed) <- dim(matrix)
    dimnames(parsed) <- dimnames(matrix)
    parsed
}

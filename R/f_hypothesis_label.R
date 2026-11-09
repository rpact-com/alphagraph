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
# Change this option once during application startup to configure the maximum
# effective length accepted by the hypotheses table, for example:
# options(alphagraph.maxHypothesisLabelLength = 10L)
.alphaGraphMaxHypothesisLabelLength <- function() {
    value <- getOption("alphagraph.maxHypothesisLabelLength", 7L)
    if (length(value) != 1L || is.na(value) || !is.numeric(value) ||
            value < 1 || value != as.integer(value)) {
        stop(
            "Option 'alphagraph.maxHypothesisLabelLength' must be a positive integer.",
            call. = FALSE
        )
    }
    as.integer(value)
}

#' @title Calculate the effective length of a hypothesis label
#' @name alphaGraphHypothesisLabels
#'
#' @description
#' Underscores do not count. Characters following an underscore count at half
#' length, rounded up, because they are rendered as subscripts.
#'
#' @param label Character scalar containing a hypothesis label.
#'
#' @return Numeric scalar with the effective label length.
#'
#' @rdname alphaGraphHypothesisLabels
#' @keywords internal
alphaGraphHypothesisLabelLength <- function(label) {
    if (!is.character(label) || length(label) != 1L || is.na(label)) {
        stop("A hypothesis label must be one character scalar.", call. = FALSE)
    }

    lines <- strsplit(label, "\n", fixed = TRUE)[[1L]]
    sum(vapply(lines, function(line) {
        underscore <- regexpr("_", line, fixed = TRUE)[[1L]]
        if (underscore == -1L) {
            return(nchar(line, type = "chars"))
        }

        baseLength <- underscore - 1L
        subscriptLength <- nchar(line, type = "chars") - underscore
        baseLength + ceiling(subscriptLength / 2)
    }, numeric(1L)))
}

#' @param maxLength Positive integer giving the maximum effective label length.
#'
#' @return `alphaGraphValidateHypothesisLabelLengths()` returns the calculated
#'   effective lengths invisibly.
#'
#' @rdname alphaGraphHypothesisLabels
#' @keywords internal
alphaGraphValidateHypothesisLabelLengths <- function(
        labels,
        maxLength = .alphaGraphMaxHypothesisLabelLength()) {
    if (!is.character(labels) || anyNA(labels)) {
        stop("Hypothesis labels must be text.", call. = FALSE)
    }
    if (length(maxLength) != 1L || is.na(maxLength) || !is.numeric(maxLength) ||
            maxLength < 1 || maxLength != as.integer(maxLength)) {
        stop("maxLength must be a positive integer.", call. = FALSE)
    }

    lengths <- vapply(
        labels,
        alphaGraphHypothesisLabelLength,
        numeric(1L)
    )
    tooLong <- which(lengths > maxLength)
    if (length(tooLong)) {
        index <- tooLong[[1L]]
        stop(
            "Hypothesis label '", labels[[index]], "' has effective length ",
            lengths[[index]], "; the maximum is ", as.integer(maxLength), ". ",
            "The underscore does not count and two subscript characters count as one.",
            call. = FALSE
        )
    }

    invisible(lengths)
}

#' @title Render the editable hypothesis-label syntax
#'
#' @description
#' Converts the part after a single underscore to Unicode subscripts supported
#' by visNetwork. For example, `H1_ni` becomes `H1` followed by subscript `ni`,
#' and `H1_(ni)` also renders the parentheses as subscripts.
#'
#' @param label Non-empty character scalar containing the editable label.
#'
#' @return Character scalar suitable for display in visNetwork.
#'
#' @rdname alphaGraphHypothesisLabels
#' @keywords internal
alphaGraphRenderHypothesisLabel <- function(label) {
    if (!is.character(label) || length(label) != 1L || is.na(label) ||
            !nzchar(label)) {
        stop("Hypothesis labels must be non-empty text.", call. = FALSE)
    }

    lines <- strsplit(label, "\n", fixed = TRUE)[[1L]]
    rendered <- vapply(lines, .alphaGraphRenderHypothesisLabelLine,
                       character(1L))
    paste(rendered, collapse = "\n")
}

#' @param labels Character vector of canonical hypothesis labels.
#'
#' @return `alphaGraphValidateHypothesisLabels()` returns the rendered labels
#'   invisibly after validating syntax and uniqueness.
#'
#' @rdname alphaGraphHypothesisLabels
#' @keywords internal
alphaGraphValidateHypothesisLabels <- function(labels) {
    if (!is.character(labels) || anyNA(labels) || any(!nzchar(labels))) {
        stop("Hypothesis labels must be non-empty text.", call. = FALSE)
    }
    if (anyDuplicated(labels)) {
        stop("Hypothesis labels must be unique.", call. = FALSE)
    }
    rendered <- vapply(
        labels,
        alphaGraphRenderHypothesisLabel,
        character(1L)
    )
    if (anyDuplicated(rendered)) {
        stop(
            "Hypothesis labels must also be unique after rendering subscripts.",
            call. = FALSE
        )
    }
    invisible(rendered)
}

.alphaGraphRenderHypothesisLabelLine <- function(label) {
    positions <- gregexpr("_", label, fixed = TRUE)[[1L]]
    underscoreCount <- if (identical(positions[[1L]], -1L)) 0L else
        length(positions)
    if (underscoreCount == 0L) {
        return(label)
    }
    if (underscoreCount != 1L || positions[[1L]] == 1L ||
            positions[[1L]] == nchar(label)) {
        stop(
            "A hypothesis label may contain one underscore followed by ",
            "a non-empty subscript, for example H1_ni or H1_(ni).",
            call. = FALSE
        )
    }

    base <- substr(label, 1L, positions[[1L]] - 1L)
    suffix <- substr(label, positions[[1L]] + 1L, nchar(label))
    hasParenthesis <- grepl("[()]", suffix)
    if (hasParenthesis && !grepl("^\\([^()]+\\)$", suffix)) {
        stop(
            "Parentheses in a hypothesis subscript must enclose the complete subscript, for example H1_(ni).",
            call. = FALSE
        )
    }

    characters <- strsplit(suffix, "", fixed = TRUE)[[1L]]
    subscriptMap <- .alphaGraphSubscriptMap()
    normalizedCharacters <- tolower(characters)
    unsupported <- unique(characters[
        !normalizedCharacters %in% names(subscriptMap)
    ])
    if (length(unsupported)) {
        stop(
            "Unsupported subscript character(s): ",
            paste(unsupported, collapse = ", "),
            ". Supported characters are: 0-9, +, -, =, parentheses, and the letters ",
            "a, e, h, i, j, k, l, m, n, o, p, r, s, t, u, v, x (case-insensitive).",
            call. = FALSE
        )
    }

    paste0(
        base,
        paste0(unname(subscriptMap[normalizedCharacters]), collapse = "")
    )
}

.alphaGraphSubscriptMap <- function() {
    stats::setNames(
        c(
            "\u2080", "\u2081", "\u2082", "\u2083", "\u2084",
            "\u2085", "\u2086", "\u2087", "\u2088", "\u2089",
            "\u208A", "\u208B", "\u208C", "\u208D", "\u208E",
            "\u2090", "\u2091", "\u2095", "\u1D62", "\u2C7C",
            "\u2096", "\u2097", "\u2098", "\u2099", "\u2092",
            "\u209A", "\u1D63", "\u209B", "\u209C", "\u1D64",
            "\u1D65", "\u2093"
        ),
        c(
            as.character(0:9), "+", "-", "=", "(", ")",
            "a", "e", "h", "i", "j", "k", "l", "m", "n", "o",
            "p", "r", "s", "t", "u", "v", "x"
        )
    )
}

##
## alphagraph
##
## Dynamic Markdown descriptions for canonical AlphaGraph objects.
##

#' @title Generate a Markdown description template for an AlphaGraph
#'
#' @description
#' Builds an editable description from the canonical graph and enriches known
#' example graphs with curated context from a JSON catalog.
#'
#' @param graph An `AlphaGraph` instance.
#' @param catalogPath Optional path to the curated graph-description JSON.
#'
#' @return Character scalar containing a Markdown description template.
#'
#' @keywords internal
getGraphDescription <- function(
        graph = createDemoAlphaGraph(),
        catalogPath = NULL) {
    if (!inherits(graph, "AlphaGraph")) {
        stop("graph must be an AlphaGraph object.", call. = FALSE)
    }

    data <- graph$toList()
    known <- .findKnownGraphDescription(data$id, catalogPath)
    nodes <- data$nodes
    edges <- data$edges
    parameters <- data$parameters %||% list()
    nodeIds <- vapply(nodes, function(node) node$id, character(1L))
    nodeLabels <- stats::setNames(
        vapply(nodes, function(node) {
            alphaGraphRenderHypothesisLabel(node$label)
        }, character(1L)),
        nodeIds
    )

    knownMeanings <- .knownHypothesisMeanings(known)
    hypotheses <- vapply(nodes, function(node) {
        meaning <- node$metadata$meaning %||%
            node$metadata$description %||%
            knownMeanings[[node$id]]
        meaning %||% "_Please add the clinical or statistical meaning of this hypothesis._"
    }, character(1L))
    weights <- vapply(nodes, function(node) {
        node$weight$display %||% graph$formatNumber(node$weight$value) %||% "0"
    }, character(1L))
    outgoing <- vapply(nodeIds, function(id) {
        outgoingEdges <- Filter(function(edge) edge$from == id, edges)
        values <- vapply(outgoingEdges, function(edge) {
            .numericSpecValue(edge$transition)
        }, numeric(1L))
        if (all(is.finite(values))) graph$formatNumber(sum(values)) else "symbolic"
    }, character(1L))
    numericWeights <- vapply(nodes, function(node) {
        .numericSpecValue(node$weight)
    }, numeric(1L))
    totalWeight <- if (all(is.finite(numericWeights))) {
        graph$formatNumber(sum(numericWeights))
    } else {
        "symbolic / not evaluable without parameter values"
    }

    summaries <- unique(c(
        .asCharacterEntries(known$summary),
        .asCharacterEntries(data$description)
    ))

    lines <- c(
        paste0("# ", .markdownText(data$label %||% data$id)),
        "",
        "<!-- This template was generated from the current AlphaGraph. -->",
        "<!-- Replace italic placeholders and add study-specific details as needed. -->",
        paste0("<!--  Graph ID: ", .markdownCode(data$id), " -->"),
        "",
        if (length(summaries)) paste(summaries, collapse = "\n\n") else
            "_Please add a concise summary of the purpose of this graph._",
        "",
        "## Graph overview",
        "",
        paste0("- Number of hypotheses: ", length(nodes)),
        paste0("- Number of directed transitions: ", length(edges)),
        paste0("- Number of parameters: ", length(parameters)),
        paste0("- Total initial hypothesis weight: ", totalWeight),
        "",
        if (length(parameters)) c(
            "## Parameters",
            "",
            "| Parameter | Technical ID | Value | Description |",
            "|---|---|---:|---|",
            vapply(parameters, function(parameter) {
                value <- if (is.null(parameter$value)) "_not set_" else
                    graph$formatNumber(parameter$value)
                paste0(
                    "| ", .markdownTable(parameter$label %||% parameter$id),
                    " | `", .markdownCode(parameter$id), "`",
                    " | ", .markdownTable(value),
                    " | ", .markdownTable(parameter$description %||%
                        "_Please describe this parameter._"), " |"
                )
            }, character(1L)),
            ""
        ) else NULL,
        "## Hypotheses and interpretation",
        "",
        "| Hypothesis | Technical ID | Meaning | Initial weight | Sum of outgoing transition weights |",
        "|---|---|---|---:|---:|",
        vapply(seq_along(nodes), function(i) {
            paste0(
                "| ", .markdownTable(nodeLabels[[i]]),
                " | `", .markdownCode(nodeIds[[i]]), "`",
                " | ", .markdownTable(hypotheses[[i]]),
                " | ", .markdownTable(weights[[i]]),
                " | ", .markdownTable(outgoing[[i]]), " |"
            )
        }, character(1L)),
        "",
        "## Transition structure",
        "",
        "A transition weight specifies the proportion of a rejected hypothesis's weight that is transferred along the directed edge.",
        ""
    )

    if (length(edges) == 0L) {
        lines <- c(lines, "_No non-zero transitions are currently defined._")
    } else {
        lines <- c(lines, vapply(edges, function(edge) {
            note <- edge$metadata$description %||% edge$metadata$meaning
            suffix <- if (is.null(note)) "" else paste0(" \u2014 ", note)
            paste0(
                "- **", .markdownText(nodeLabels[[edge$from]] %||% edge$from),
                " \u2192 ", .markdownText(nodeLabels[[edge$to]] %||% edge$to),
                ":** ", edge$transition$display %||%
                    graph$formatNumber(edge$transition$value),
                suffix
            )
        }, character(1L)))
    }

    context <- unique(c(
        .asCharacterEntries(known$context),
        .asCharacterEntries(data$metadata$context)
    ))
    interpretation <- unique(c(
        .asCharacterEntries(known$interpretation),
        .asCharacterEntries(data$metadata$interpretation)
    ))
    literature <- unique(c(
        .literatureEntries(known$literature),
        .literatureEntries(data$metadata$literature)
    ))

    lines <- c(
        lines,
        "",
        "## Context",
        "",
        if (length(context)) paste0("- ", context) else
            "_Please add the clinical context, endpoints, populations, and treatment comparisons._",
        "",
        "## Interpretation notes",
        "",
        if (length(interpretation)) paste0("- ", interpretation) else
            "_Please explain the testing sequence, decision rules, and how weight propagation should be interpreted._",
        "",
        "## Additional notes",
        "",
        "_Add assumptions, limitations, version information, or study-specific decisions here._",
        "",
        "## Literature",
        "",
        if (length(literature)) paste0("- ", literature) else
            "_Please add supporting publications, protocol references, or other sources._"
    )

    paste(lines, collapse = "\n")
}

#' @title Write a dynamic graph description to a Markdown file
#'
#' @param graph An `AlphaGraph` instance.
#' @param catalogPath Optional path to the curated graph-description JSON.
#'
#' @return Path to a temporary Markdown file.
#'
#' @keywords internal
getGraphDescriptionFile <- function(
        graph = createDemoAlphaGraph(),
        catalogPath = NULL) {
    mdFile <- tempfile(fileext = ".md")
    connection <- file(mdFile, open = "wt", encoding = "UTF-8")
    on.exit(close(connection), add = TRUE)
    writeLines(
        getGraphDescription(graph = graph, catalogPath = catalogPath),
        con = connection
    )
    mdFile
}

.graphDescriptionCatalogPath <- function() {
    getSystemFile("data/graph-description-catalog.json")
}

.readGraphDescriptionCatalog <- function(path = NULL) {
    path <- path %||% .graphDescriptionCatalogPath()
    if (!length(path) || !nzchar(path) || !file.exists(path)) {
        return(list(schema_version = 1L, graphs = list()))
    }
    catalog <- jsonlite::fromJSON(path, simplifyVector = FALSE)
    if (!is.list(catalog) || is.null(catalog$graphs) || !is.list(catalog$graphs)) {
        stop("Invalid graph-description catalog: expected a 'graphs' array.",
             call. = FALSE)
    }
    version <- catalog$schema_version
    if (length(version) != 1L || is.na(suppressWarnings(as.integer(version))) ||
            as.integer(version) != 1L) {
        stop("Unsupported graph-description catalog schema version.", call. = FALSE)
    }
    ids <- vapply(catalog$graphs, function(item) {
        if (!is.list(item) || !is.character(item$id) ||
                length(item$id) != 1L || !nzchar(item$id)) {
            stop("Every graph-description entry must have a non-empty 'id'.",
                 call. = FALSE)
        }
        item$id
    }, character(1L))
    if (anyDuplicated(ids)) {
        stop("Graph-description catalog contains duplicate graph ids.",
             call. = FALSE)
    }
    catalog
}

.findKnownGraphDescription <- function(id, catalogPath = NULL) {
    catalog <- .readGraphDescriptionCatalog(catalogPath)
    matches <- Filter(function(item) identical(item$id, id), catalog$graphs)
    if (!length(matches)) list() else matches[[1L]]
}

.knownHypothesisMeanings <- function(known) {
    entries <- known$hypotheses %||% list()
    if (!length(entries)) {
        return(list())
    }
    result <- lapply(entries, function(entry) entry$meaning %||% NULL)
    names(result) <- vapply(entries, function(entry) entry$id, character(1L))
    result
}

.asCharacterEntries <- function(x) {
    if (is.null(x)) {
        return(character())
    }
    values <- trimws(as.character(unlist(x, use.names = FALSE)))
    values[nzchar(values)]
}

.literatureEntries <- function(x) {
    if (is.null(x)) {
        return(character())
    }
    entries <- vapply(x, function(entry) {
        if (is.character(entry)) {
            return(entry[[1L]])
        }
        citation <- entry$citation %||% ""
        if (!is.null(entry$url) && nzchar(entry$url)) {
            return(paste0(citation, " [Link](", entry$url, ")"))
        }
        citation
    }, character(1L))
    entries[nzchar(trimws(entries))]
}

.markdownText <- function(x) {
    x <- gsub("*", "\\*", as.character(x), fixed = TRUE)
    gsub("_", "\\_", x, fixed = TRUE)
}

.markdownTable <- function(x) {
    x <- gsub("\\|", "\\\\|", as.character(x))
    gsub("[\r\n]+", " ", x)
}

.markdownCode <- function(x) {
    gsub("`", "'", as.character(x), fixed = TRUE)
}

.numericSpecValue <- function(spec) {
    value <- spec$value
    if (is.numeric(value) && length(value) == 1L && is.finite(value)) {
        return(as.numeric(value))
    }
    NA_real_
}

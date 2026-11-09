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
#' @title
#' JSON-friendly alphagraph project model
#'
#' @description
#' Stores a reusable `AlphaGraph` together with p-value datasets, analysis
#' specifications, optional derived results, project UI state, and metadata.
#' Raw and adjusted p-values deliberately remain outside the graph definition.
#'
#' @details
#' Create an instance with `AlphaProject$new()`. P-values use stable hypothesis
#' identifiers. Dataset and analysis updates remove affected results; graph
#' replacement removes all results. Use `$resultIsCurrent()` before displaying a
#' stored result and `$validate(requireComputable = TRUE, analysisId = ...)`
#' before a calculation. Derived results contain input hashes and engine versions.
#' Mutating methods return the instance invisibly unless stated otherwise.
#'
#' @return An R6 class generator.
#'
#' @examples
#' project <- createDemoAlphaProject()
#' project$pValues("primary")
#' project$validate(requireComputable = TRUE, analysisId = "shortcut-primary")
#'
#' @keywords internal
#' @importFrom graphicalMCP graph_test_shortcut
#'
AlphaProject <- R6::R6Class(
    "AlphaProject",
    public = list(
        #'
        #' @description
        #' Initialize a project containing a graph and empty datasets, analyses, and results.
        #'
        #' @param graph An AlphaGraph instance; the demo graph is used when omitted at
        #'   initialization.
        #' @param id Character scalar identifying the object within its collection.
        #' @param label Human-readable character label.
        #' @param description Optional character description; NULL omits it.
        #' @param ui Named list of JSON-compatible project UI state; empty by default.
        #' @param metadata Named list of JSON-compatible metadata; empty by default.
        #'
        initialize = function(
                graph = createDemoAlphaGraph(),
                id = "project",
                label = id,
                description = NULL,
                ui = list(),
                metadata = list()) {
            if (!inherits(graph, "AlphaGraph")) {
                stop("graph must be an AlphaGraph object.", call. = FALSE)
            }
            private$data <- list(
                format = "alphagraph.project",
                version = 1L,
                id = as.character(id),
                label = as.character(label %||% id),
                description = description,
                graph = graph$toList(),
                datasets = list(),
                analyses = list(),
                results = list(),
                ui = private$objectList(ui, "ui"),
                metadata = private$objectList(metadata, "metadata")
            )
            invisible(self)
        },

        #'
        #' @description
        #' Restore a JSON-compatible project list and optionally validate it.
        #'
        #' @param x Canonical JSON-compatible list.
        #' @param validate Logical; whether to validate the imported, returned, or modified
        #'   object.
        #'
        loadList = function(x, validate = TRUE) {
            if (!is.list(x)) {
                stop("x must be a list.", call. = FALSE)
            }
            required <- c(
                "format", "version", "id", "label", "description", "graph",
                "datasets", "analyses", "results", "ui", "metadata"
            )
            if (!all(required %in% names(x))) {
                stop("Project data is missing required fields: ",
                    paste(setdiff(required, names(x)), collapse = ", "), ".",
                    call. = FALSE
                )
            }
            private$data <- x
            private$data$version <- as.integer(private$data$version)
            private$data$ui <- private$objectList(private$data$ui, "ui")
            private$data$metadata <- private$objectList(
                private$data$metadata,
                "metadata"
            )
            private$data$datasets <- lapply(private$data$datasets, function(dataset) {
                dataset$metadata <- private$objectList(
                    dataset$metadata %||% list(),
                    "dataset metadata"
                )
                dataset
            })
            private$data$analyses <- lapply(private$data$analyses, function(analysis) {
                analysis$options <- private$objectList(
                    analysis$options %||% list(),
                    "analysis options"
                )
                analysis$metadata <- private$objectList(
                    analysis$metadata %||% list(),
                    "analysis metadata"
                )
                analysis
            })
            private$data$results <- lapply(private$data$results, function(result) {
                result$metadata <- private$objectList(
                    result$metadata %||% list(),
                    "result metadata"
                )
                result
            })
            if (isTRUE(validate)) {
                self$validate()
            }
            invisible(self)
        },

        #'
        #' @description
        #' Return the JSON-compatible project list including stored results.
        #'
        toList = function() {
            private$data
        },

        #'
        #' @description
        #' Serialize the project with jsonlite::toJSON(); ... supplies additional JSON options.
        #'
        #' @param pretty Logical; whether to indent JSON output. Defaults to TRUE.
        #' @param auto_unbox Logical; whether to unbox JSON scalars. Defaults to TRUE.
        #' @param null Character NULL encoding passed to jsonlite::toJSON(); defaults to
        #'   "null".
        #' @param ... Named arguments passed to the operation described above.
        #'
        toJson = function(pretty = TRUE, auto_unbox = TRUE, null = "null", ...) {
            jsonlite::toJSON(
                private$data,
                pretty = pretty,
                auto_unbox = auto_unbox,
                null = null,
                ...
            )
        },

        #'
        #' @description
        #' Return an independent AlphaGraph restored from the embedded graph definition.
        #'
        graph = function() {
            alphaGraphFromList(private$data$graph)
        },

        #'
        #' @description
        #' Replace the embedded graph and clear all derived results.
        #'
        #' @param graph An AlphaGraph instance; the demo graph is used when omitted at
        #'   initialization.
        #'
        setGraph = function(graph) {
            if (!inherits(graph, "AlphaGraph")) {
                stop("graph must be an AlphaGraph object.", call. = FALSE)
            }
            graph$validate(requireNumeric = FALSE)
            private$data$graph <- graph$toList()
            private$data$results <- list()
            invisible(self)
        },

        #'
        #' @description
        #' Return the list of supplied p-value datasets.
        #'
        datasets = function() private$data$datasets,
        #'
        #' @description
        #' Return the list of stored analysis specifications.
        #'
        analyses = function() private$data$analyses,
        #'
        #' @description
        #' Return the list of stored derived results, including any stale results.
        #'
        results = function() private$data$results,

        #'
        #' @description
        #' Return the dataset with the requested identifier; error if it does not exist.
        #'
        #' @param id Character scalar identifying the object within its collection.
        #'
        dataset = function(id) {
            private$data$datasets[[private$requireIndex("datasets", id)]]
        },

        #'
        #' @description
        #' Return the analysis with the requested identifier; error if it does not exist.
        #'
        #' @param id Character scalar identifying the object within its collection.
        #'
        analysis = function(id) {
            private$data$analyses[[private$requireIndex("analyses", id)]]
        },

        #'
        #' @description
        #' Return the latest stored result for an analysis, or NULL; use resultIsCurrent() to
        #'   check validity.
        #'
        #' @param analysisId Character identifier of an analysis specification.
        #'
        result = function(analysisId) {
            indices <- which(vapply(
                private$data$results,
                function(result) identical(result$analysisId, analysisId),
                logical(1L)
            ))
            if (!length(indices)) {
                return(NULL)
            }
            private$data$results[[tail(indices, 1L)]]
        },

        #'
        #' @description
        #' Insert or replace a p-value dataset and remove results for affected analyses.
        #'
        #' @param id Character scalar identifying the object within its collection.
        #' @param pValues Named numeric vector or list of hypothesisId/value records, keyed by
        #'   stable hypothesis IDs.
        #' @param label Human-readable character label.
        #' @param description Optional character description; NULL omits it.
        #' @param metadata Named list of JSON-compatible metadata; empty by default.
        #'
        upsertDataset = function(
                id,
                pValues,
                label = id,
                description = NULL,
                metadata = list()) {
            private$assertId(id, "Dataset id")
            dataset <- list(
                id = as.character(id),
                label = as.character(label %||% id),
                description = description,
                pValues = private$pValueRecords(pValues),
                metadata = private$objectList(metadata, "dataset metadata")
            )
            private$upsert("datasets", dataset)

            affected <- vapply(
                private$data$analyses,
                function(analysis) identical(analysis$datasetId, id),
                logical(1L)
            )
            affectedIds <- vapply(
                private$data$analyses[affected],
                function(analysis) analysis$id,
                character(1L)
            )
            private$removeResults(affectedIds)
            invisible(self)
        },

        #'
        #' @description
        #' Return a numeric p-value vector named by stable hypothesis IDs.
        #'
        #' @param datasetId Character identifier of a p-value dataset.
        #'
        pValues = function(datasetId) {
            records <- self$dataset(datasetId)$pValues
            values <- vapply(records, function(record) as.numeric(record$value), numeric(1L))
            names(values) <- vapply(
                records,
                function(record) record$hypothesisId,
                character(1L)
            )
            values
        },

        #'
        #' @description
        #' Insert or replace an analysis specification and remove its derived results.
        #'
        #' @param id Character scalar identifying the object within its collection.
        #' @param datasetId Character identifier of a p-value dataset.
        #' @param method Character choice of "shortcut" or "closure"; only shortcut execution
        #'   is implemented.
        #' @param alpha Numeric significance level in (0, 1]; defaults to 0.025.
        #' @param testGroups List of closure groups; empty by default. Each group has
        #'   hypothesisIds, optional id and label, testType (bonferroni, parametric, simes, or
        #'   hochberg), and a correlation matrix for parametric tests. Groups must partition
        #'   the hypotheses.
        #' @param verbose Logical passed to graphicalMCP shortcut execution; defaults to FALSE.
        #' @param testValues Logical passed as test_values to graphicalMCP; defaults to FALSE.
        #' @param label Human-readable character label.
        #' @param description Optional character description; NULL omits it.
        #' @param metadata Named list of JSON-compatible metadata; empty by default.
        #'
        upsertAnalysis = function(
                id,
                datasetId,
                method = c("shortcut", "closure"),
                alpha = 0.025,
                testGroups = list(),
                verbose = FALSE,
                testValues = FALSE,
                label = id,
                description = NULL,
                metadata = list()) {
            method <- match.arg(method)
            private$assertId(id, "Analysis id")
            private$assertId(datasetId, "Dataset id")
            analysis <- list(
                id = as.character(id),
                label = as.character(label %||% id),
                description = description,
                datasetId = as.character(datasetId),
                method = method,
                alpha = as.numeric(alpha),
                testGroups = private$testGroups(testGroups),
                options = list(
                    verbose = isTRUE(verbose),
                    testValues = isTRUE(testValues)
                ),
                metadata = private$objectList(metadata, "analysis metadata")
            )
            private$upsert("analyses", analysis)
            private$removeResults(id)
            invisible(self)
        },

        #'
        #' @description
        #' Return a fingerprint of the graph, dataset, and analysis inputs.
        #'
        #' @param analysisId Character identifier of an analysis specification.
        #'
        inputHash = function(analysisId) {
            analysis <- self$analysis(analysisId)
            dataset <- self$dataset(analysis$datasetId)
            alphaGraphInputHash(list(
                graph = private$data$graph,
                dataset = dataset,
                analysis = analysis
            ))
        },

        #'
        #' @description
        #' Return TRUE only when a stored result matches the current input fingerprint.
        #'
        #' @param analysisId Character identifier of an analysis specification.
        #'
        resultIsCurrent = function(analysisId) {
            result <- self$result(analysisId)
            !is.null(result) && identical(result$inputHash, self$inputHash(analysisId))
        },

        #'
        #' @description
        #' Run graphicalMCP shortcut testing and invisibly return the stored result with input
        #'   hash and engine versions.
        #'
        #' @param analysisId Character identifier of an analysis specification.
        #'
        runShortcut = function(analysisId) {
            if (!requireNamespace("graphicalMCP", quietly = TRUE)) {
                stop("Package 'graphicalMCP' is required to run an analysis.",
                    call. = FALSE
                )
            }
            self$validate(requireComputable = TRUE, analysisId = analysisId)
            analysis <- self$analysis(analysisId)
            if (!identical(analysis$method, "shortcut")) {
                stop("Analysis '", analysisId, "' is not a shortcut analysis.",
                    call. = FALSE
                )
            }

            graph <- self$graph()
            graphInput <- graph$toGraphicalMCP(hypNames = "id")
            pValues <- self$pValues(analysis$datasetId)
            nodeIds <- names(graphInput$hypotheses)
            pValues <- unname(pValues[nodeIds])
            report <- graphicalMCP::graph_test_shortcut(
                graph = graphInput,
                p = pValues,
                alpha = analysis$alpha,
                verbose = analysis$options$verbose,
                test_values = analysis$options$testValues
            )

            adjusted <- report$outputs$adjusted_p
            rejected <- report$outputs$rejected
            names(adjusted) <- names(adjusted) %||% nodeIds
            names(rejected) <- names(rejected) %||% nodeIds
            result <- list(
                id = paste0(analysisId, "-result"),
                analysisId = analysisId,
                inputHash = self$inputHash(analysisId),
                createdAt = format(Sys.time(), "%Y-%m-%d %H:%M", tz = "UTC"),
                engine = list(
                    alphagraphVersion = alphaGraphPackageVersion("alphagraph"),
                    graphicalMCPVersion = alphaGraphPackageVersion("graphicalMCP")
                ),
                adjustedP = private$pValueRecords(adjusted[nodeIds]),
                rejected = private$decisionRecords(rejected[nodeIds]),
                metadata = setNames(list(), character())
            )
            private$removeResults(analysisId)
            private$data$results[[length(private$data$results) + 1L]] <- result
            self$validate()
            invisible(result)
        },

        #'
        #' @description
        #' Validate project structure and optionally computability; return TRUE on success.
        #'
        #' @param requireComputable Logical; whether to require complete numeric inputs for the
        #'   selected analysis.
        #' @param analysisId Character identifier of an analysis specification. Required when
        #'   requireComputable is TRUE.
        #' @param tolerance Numeric validation tolerance; defaults to
        #'   sqrt(.Machine$double.eps).
        #'
        validate = function(requireComputable = FALSE, analysisId = NULL,
                            tolerance = sqrt(.Machine$double.eps)) {
            if (!identical(private$data$format, "alphagraph.project")) {
                stop("Unsupported project format.", call. = FALSE)
            }
            if (!identical(as.integer(private$data$version), 1L)) {
                stop("Unsupported alphagraph project version.", call. = FALSE)
            }
            private$assertId(private$data$id, "Project id")
            if (!is.character(private$data$label) || length(private$data$label) != 1L) {
                stop("Project label must be a character scalar.", call. = FALSE)
            }
            if (!is.null(private$data$description) &&
                    (!is.character(private$data$description) ||
                        length(private$data$description) != 1L)) {
                stop("Project description must be null or a character scalar.",
                    call. = FALSE
                )
            }
            if (!is.list(private$data$datasets) || !is.list(private$data$analyses) ||
                    !is.list(private$data$results) || !is.list(private$data$ui) ||
                    !is.list(private$data$metadata)) {
                stop("Project collections, ui, and metadata must be objects or arrays.",
                    call. = FALSE
                )
            }

            graph <- self$graph()
            graph$validate(requireNumeric = FALSE)
            nodeIds <- vapply(
                graph$toList()$nodes,
                function(node) node$id,
                character(1L)
            )

            datasetIds <- private$validateIds(private$data$datasets, "Dataset")
            for (dataset in private$data$datasets) {
                private$validateLabelDescription(dataset, "Dataset")
                private$validateHypothesisRecords(
                    dataset$pValues,
                    nodeIds,
                    "Dataset p-values",
                    logicalValues = FALSE
                )
            }

            analysisIds <- private$validateIds(private$data$analyses, "Analysis")
            for (analysis in private$data$analyses) {
                private$validateLabelDescription(analysis, "Analysis")
                if (!analysis$datasetId %in% datasetIds) {
                    stop("Analysis '", analysis$id, "' references unknown dataset '",
                        analysis$datasetId, "'.", call. = FALSE
                    )
                }
                if (!analysis$method %in% c("shortcut", "closure")) {
                    stop("Unsupported analysis method '", analysis$method, "'.",
                        call. = FALSE
                    )
                }
                if (length(analysis$alpha) != 1L || !is.numeric(analysis$alpha) ||
                        !is.finite(analysis$alpha) || analysis$alpha <= 0 ||
                        analysis$alpha > 1) {
                    stop("Analysis alpha must be a finite number in (0, 1].",
                        call. = FALSE
                    )
                }
                if (!is.list(analysis$options) ||
                        !all(c("verbose", "testValues") %in% names(analysis$options)) ||
                        !is.logical(analysis$options$verbose) ||
                        length(analysis$options$verbose) != 1L ||
                        !is.logical(analysis$options$testValues) ||
                        length(analysis$options$testValues) != 1L) {
                    stop("Analysis options must contain logical verbose and testValues.",
                        call. = FALSE
                    )
                }
                private$validateTestGroups(analysis, nodeIds, tolerance)
            }

            resultIds <- private$validateIds(private$data$results, "Result")
            invisible(resultIds)
            for (result in private$data$results) {
                if (!result$analysisId %in% analysisIds) {
                    stop("Result '", result$id, "' references unknown analysis '",
                        result$analysisId, "'.", call. = FALSE
                    )
                }
                private$assertId(result$inputHash, "Result input hash")
                if (!is.character(result$createdAt) || length(result$createdAt) != 1L ||
                        !nzchar(result$createdAt)) {
                    stop("Result createdAt must be a non-empty date-time string.",
                        call. = FALSE
                    )
                }
                if (!is.list(result$engine) ||
                        !all(c("alphagraphVersion", "graphicalMCPVersion") %in%
                            names(result$engine))) {
                    stop("Result engine must contain alphagraph and graphicalMCP versions.",
                        call. = FALSE
                    )
                }
                private$validateHypothesisRecords(
                    result$adjustedP,
                    nodeIds,
                    "Adjusted p-values",
                    logicalValues = FALSE
                )
                private$validateHypothesisRecords(
                    result$rejected,
                    nodeIds,
                    "Rejection decisions",
                    logicalValues = TRUE
                )
                adjustedIds <- vapply(
                    result$adjustedP,
                    function(record) record$hypothesisId,
                    character(1L)
                )
                rejectedIds <- vapply(
                    result$rejected,
                    function(record) record$hypothesisId,
                    character(1L)
                )
                if (!setequal(adjustedIds, nodeIds) || !setequal(rejectedIds, nodeIds)) {
                    stop("Results must contain every project hypothesis exactly once.",
                        call. = FALSE
                    )
                }
            }

            if (isTRUE(requireComputable)) {
                if (is.null(analysisId)) {
                    stop("analysisId is required for computable validation.",
                        call. = FALSE
                    )
                }
                analysis <- self$analysis(analysisId)
                pIds <- names(self$pValues(analysis$datasetId))
                if (!setequal(pIds, nodeIds) || length(pIds) != length(nodeIds)) {
                    stop("The selected dataset must contain exactly one p-value for every hypothesis.",
                        call. = FALSE
                    )
                }
                graph$validate(requireNumeric = TRUE)
            }
            TRUE
        }
    ),

    private = list(
        data = NULL,

        assertId = function(x, name) {
            if (!is.character(x) || length(x) != 1L || !nzchar(x)) {
                stop(name, " must be a non-empty character scalar.", call. = FALSE)
            }
        },

        objectList = function(x, name) {
            x <- x %||% list()
            if (!is.list(x)) {
                stop(name, " must be an object.", call. = FALSE)
            }
            if (!length(x)) {
                return(setNames(list(), character()))
            }
            if (is.null(names(x)) || any(!nzchar(names(x)))) {
                stop(name, " must be a named object.", call. = FALSE)
            }
            x
        },

        index = function(collection, id) {
            match(id, vapply(
                private$data[[collection]],
                function(item) item$id,
                character(1L)
            ))
        },

        requireIndex = function(collection, id) {
            index <- private$index(collection, id)
            if (is.na(index)) {
                stop("Unknown ", sub("s$", "", collection), " '", id, "'.",
                    call. = FALSE
                )
            }
            index
        },

        upsert = function(collection, item) {
            index <- private$index(collection, item$id)
            if (is.na(index)) {
                private$data[[collection]][[length(private$data[[collection]]) + 1L]] <- item
            } else {
                private$data[[collection]][[index]] <- item
            }
            invisible(NULL)
        },

        removeResults = function(analysisIds) {
            if (!length(analysisIds) || !length(private$data$results)) {
                return(invisible(NULL))
            }
            keep <- !vapply(
                private$data$results,
                function(result) result$analysisId %in% analysisIds,
                logical(1L)
            )
            private$data$results <- private$data$results[keep]
            invisible(NULL)
        },

        pValueRecords = function(pValues) {
            if (is.list(pValues) && !is.numeric(pValues)) {
                records <- pValues
            } else {
                if (is.null(names(pValues)) || any(!nzchar(names(pValues)))) {
                    stop("pValues must be a named numeric vector.", call. = FALSE)
                }
                records <- lapply(seq_along(pValues), function(i) list(
                    hypothesisId = names(pValues)[[i]],
                    value = as.numeric(pValues[[i]])
                ))
            }
            lapply(records, function(record) list(
                hypothesisId = as.character(record$hypothesisId),
                value = as.numeric(record$value)
            ))
        },

        decisionRecords = function(decisions) {
            if (is.null(names(decisions)) || any(!nzchar(names(decisions)))) {
                stop("decisions must be a named logical vector.", call. = FALSE)
            }
            lapply(seq_along(decisions), function(i) list(
                hypothesisId = names(decisions)[[i]],
                value = isTRUE(decisions[[i]])
            ))
        },

        testGroups = function(groups) {
            lapply(seq_along(groups), function(i) {
                group <- groups[[i]]
                if (is.character(group)) {
                    group <- list(hypothesisIds = group)
                }
                correlation <- group$correlation %||% NULL
                if (is.matrix(correlation)) {
                    correlation <- lapply(seq_len(nrow(correlation)), function(row) {
                        as.numeric(correlation[row, ])
                    })
                }
                list(
                    id = as.character(group$id %||% paste0("group-", i)),
                    label = as.character(group$label %||% group$id %||% paste0("Group ", i)),
                    hypothesisIds = as.character(group$hypothesisIds),
                    testType = tolower(as.character(group$testType %||% "bonferroni")),
                    correlation = correlation
                )
            })
        },

        validateIds = function(items, type) {
            ids <- vapply(items, function(item) item$id, character(1L))
            if (any(!nzchar(ids)) || anyDuplicated(ids)) {
                stop(type, " ids must be unique and non-empty.", call. = FALSE)
            }
            ids
        },

        validateLabelDescription = function(item, type) {
            if (!is.character(item$label) || length(item$label) != 1L) {
                stop(type, " labels must be character scalars.", call. = FALSE)
            }
            if (!is.null(item$description) &&
                    (!is.character(item$description) || length(item$description) != 1L)) {
                stop(type, " descriptions must be null or character scalars.",
                    call. = FALSE
                )
            }
        },

        validateHypothesisRecords = function(records, nodeIds, name, logicalValues) {
            if (!is.list(records)) {
                stop(name, " must be an array.", call. = FALSE)
            }
            ids <- vapply(records, function(record) record$hypothesisId, character(1L))
            if (any(!nzchar(ids)) || anyDuplicated(ids)) {
                stop(name, " must contain unique, non-empty hypothesis ids.",
                    call. = FALSE
                )
            }
            unknown <- setdiff(ids, nodeIds)
            if (length(unknown)) {
                stop(name, " reference unknown hypotheses: ",
                    paste(unknown, collapse = ", "), ".", call. = FALSE
                )
            }
            values <- lapply(records, function(record) record$value)
            if (isTRUE(logicalValues)) {
                valid <- vapply(values, function(value) {
                    is.logical(value) && length(value) == 1L && !is.na(value)
                }, logical(1L))
            } else {
                valid <- vapply(values, function(value) {
                    is.numeric(value) && length(value) == 1L && is.finite(value) &&
                        value >= 0 && value <= 1
                }, logical(1L))
            }
            if (any(!valid)) {
                stop(name, if (logicalValues) " must contain logical values."
                    else " must contain finite values between 0 and 1.",
                    call. = FALSE
                )
            }
            invisible(TRUE)
        },

        validateTestGroups = function(analysis, nodeIds, tolerance) {
            if (!is.list(analysis$testGroups)) {
                stop("Analysis testGroups must be an array.", call. = FALSE)
            }
            if (identical(analysis$method, "shortcut") && length(analysis$testGroups)) {
                stop("Shortcut analyses must not define test groups.", call. = FALSE)
            }
            groupIds <- private$validateIds(analysis$testGroups, "Test group")
            invisible(groupIds)
            supported <- c("bonferroni", "parametric", "simes", "hochberg")
            groupedHypotheses <- character()
            for (group in analysis$testGroups) {
                if (!is.character(group$label) || length(group$label) != 1L) {
                    stop("Test group labels must be character scalars.", call. = FALSE)
                }
                ids <- group$hypothesisIds
                if (!is.character(ids) || !length(ids) || anyDuplicated(ids) ||
                        any(!ids %in% nodeIds)) {
                    stop("Test group '", group$id,
                        "' must reference unique, existing hypotheses.", call. = FALSE
                    )
                }
                if (!group$testType %in% supported) {
                    stop("Unsupported test type '", group$testType, "'.", call. = FALSE)
                }
                groupedHypotheses <- c(groupedHypotheses, ids)
                if (identical(group$testType, "parametric")) {
                    matrix <- private$correlationMatrix(group$correlation, length(ids))
                    if (any(abs(matrix - t(matrix)) > tolerance) ||
                            any(abs(diag(matrix) - 1) > tolerance)) {
                        stop("Correlation matrix for test group '", group$id,
                            "' must be symmetric with diagonal 1.", call. = FALSE
                        )
                    }
                    if (min(eigen(matrix, symmetric = TRUE, only.values = TRUE)$values) <
                            -tolerance) {
                        stop("Correlation matrix for test group '", group$id,
                            "' must be positive semidefinite.", call. = FALSE
                        )
                    }
                } else if (!is.null(group$correlation)) {
                    stop("Only parametric test groups may define a correlation matrix.",
                        call. = FALSE
                    )
                }
            }
            if (length(analysis$testGroups) &&
                    (anyDuplicated(groupedHypotheses) ||
                        !setequal(groupedHypotheses, nodeIds))) {
                stop("Closure test groups must partition all project hypotheses.",
                    call. = FALSE
                )
            }
            invisible(TRUE)
        },

        correlationMatrix = function(x, size) {
            if (is.null(x)) {
                stop("Parametric test groups require a correlation matrix.",
                    call. = FALSE
                )
            }
            matrix <- if (is.matrix(x)) x else do.call(rbind, x)
            storage.mode(matrix) <- "double"
            if (!identical(dim(matrix), c(size, size)) || any(!is.finite(matrix)) ||
                    any(matrix < -1 | matrix > 1)) {
                stop("Correlation matrix dimensions and values are invalid.",
                    call. = FALSE
                )
            }
            matrix
        }
    )
)

#'
#' @title
#' Create an AlphaProject from a JSON-compatible list
#'
#' @description
#' Restore a project from its canonical list, optionally validating its contents.
#'
#' @param x Canonical JSON-compatible project list.
#' @param validate Logical; whether to validate the restored project.
#' @return An `AlphaProject` instance.
#'
#' @keywords internal
#'
alphaProjectFromList <- function(x, validate = TRUE) {
    project <- AlphaProject$new()
    project$loadList(x, validate = validate)
    project
}

#'
#' @title
#' Create an AlphaProject from JSON
#'
#' @description
#' Read a serialized project with jsonlite and restore its canonical model.
#'
#' @param json JSON text, connection, or file path accepted by `jsonlite::fromJSON()`.
#' @param validate Logical; whether to validate the restored project.
#' @return An `AlphaProject` instance.
#'
#' @keywords internal
#'
alphaProjectFromJson <- function(json, validate = TRUE) {
    alphaProjectFromList(
        jsonlite::fromJSON(json, simplifyVector = FALSE),
        validate = validate
    )
}

#'
#' @title
#' Create the default alphagraph demonstration project
#'
#' @description
#' Construct the demo graph, primary dataset, and shortcut analysis specification.
#'
#' @return An `AlphaProject` with the demo graph, primary p-values, and a shortcut analysis.
#'
#' @keywords internal
#'
createDemoAlphaProject <- function() {
    project <- AlphaProject$new(
        graph = createDemoAlphaGraph(),
        id = "demo-project",
        label = "Demo project"
    )
    project$upsertDataset(
        id = "primary",
        label = "Primary analysis data",
        pValues = c(H1_NI = 0.012, H1_S = 0.001, H2_NI = 0.030, H2_S = 0.162)
    )
    project$upsertAnalysis(
        id = "shortcut-primary",
        datasetId = "primary",
        method = "shortcut",
        alpha = 0.025,
        label = "Shortcut analysis"
    )
    project$validate()
    project
}

#'
#' @title
#' Create a deterministic fingerprint for analysis inputs
#'
#' @description
#' Serialize JSON-compatible inputs and compute their MD5 fingerprint.
#'
#' @param x JSON-compatible object containing analysis inputs.
#' @return A character scalar containing the MD5 fingerprint of the serialized inputs.
#'
#' @keywords internal
#'
alphaGraphInputHash <- function(x) {
    path <- tempfile(fileext = ".json")
    on.exit(unlink(path), add = TRUE)
    writeLines(
        jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", digits = NA),
        path,
        useBytes = TRUE
    )
    unname(as.character(tools::md5sum(path)))
}

#'
#' @title
#' Obtain a package version without requiring attachment
#'
#' @description
#' Look up an installed package version, returning NULL if it is unavailable.
#'
#' @param package Character scalar naming a package.
#' @return A character version string, or NULL if the package is unavailable.
#'
#' @keywords internal
#'
alphaGraphPackageVersion <- function(package) {
    if (!requireNamespace(package, quietly = TRUE)) {
        return(NULL)
    }
    as.character(utils::packageVersion(package))
}

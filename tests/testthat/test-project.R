test_that("graphicalMCP conversion uses stable hypothesis ids by default", {
    graph <- createDemoAlphaGraph()
    input <- graph$toGraphicalMCPInput()
    ids <- vapply(graph$toList()$nodes, function(node) node$id, character(1L))

    expect_identical(names(input$hypotheses), ids)
    expect_identical(rownames(input$transitions), ids)
    expect_identical(colnames(input$transitions), ids)
})

test_that("graph validation distinguishes drafts from computable graphs", {
    graph <- AlphaGraph$new()
    graph$addNode(
        "H1",
        weight = NULL,
        weightExpression = "missing_parameter",
        weightLabel = "x"
    )

    expect_true(graph$validate(requireNumeric = FALSE))
    expect_error(
        graph$validate(requireNumeric = TRUE),
        "Unknown parameter"
    )

    malformed <- createDemoAlphaGraph()$toList()
    malformed$nodes[[1L]]$weight$value <- Inf
    expect_error(alphaGraphFromList(malformed), "finite numeric")
})

test_that("project schemas are valid JSON and compose the graph schema", {
    graphSchemaPath <- getSystemFile("schema/alphagraph-graph-v1.schema.json")
    projectSchemaPath <- getSystemFile("schema/alphagraph-project-v1.schema.json")
    if (!nzchar(graphSchemaPath)) {
        graphSchemaPath <- file.path("inst", "schema", "alphagraph-graph-v1.schema.json")
    }
    if (!nzchar(projectSchemaPath)) {
        projectSchemaPath <- file.path("inst", "schema", "alphagraph-project-v1.schema.json")
    }

    graphSchema <- jsonlite::fromJSON(graphSchemaPath, simplifyVector = FALSE)
    projectSchema <- jsonlite::fromJSON(projectSchemaPath, simplifyVector = FALSE)

    expect_identical(graphSchema$properties$format$const, "alphagraph.graph")
    expect_identical(projectSchema$properties$format$const, "alphagraph.project")
    expect_identical(
        projectSchema$properties$graph$`$ref`,
        "alphagraph-graph-v1.schema.json"
    )
    expect_true("description" %in% unlist(graphSchema$required))
})

test_that("AlphaProject round-trips all user inputs without putting p-values in graph", {
    project <- createDemoAlphaProject()
    restored <- alphaProjectFromJson(project$toJson())

    expect_true(restored$validate())
    expect_equal(restored$pValues("primary"), project$pValues("primary"))
    expect_null(restored$graph()$toList()$nodes[[1L]]$pValue)
    expect_identical(restored$analysis("shortcut-primary")$method, "shortcut")
})

test_that("project validation checks dataset references and computability", {
    project <- AlphaProject$new(createDemoAlphaGraph())
    project$upsertDataset("partial", c(H1_NI = 0.01))
    project$upsertAnalysis("a", "partial", method = "shortcut")

    expect_true(project$validate())
    expect_error(
        project$validate(requireComputable = TRUE, analysisId = "a"),
        "exactly one p-value"
    )

    invalid <- project$toList()
    invalid$datasets[[1L]]$pValues[[1L]]$hypothesisId <- "unknown"
    expect_error(
        alphaProjectFromList(invalid),
        "unknown hypotheses"
    )
})

test_that("closure test-group validation checks correlation semantics", {
    project <- createDemoAlphaProject()
    project$upsertAnalysis(
        id = "closure",
        datasetId = "primary",
        method = "closure",
        testGroups = list(
            list(
                id = "primary-group",
                hypothesisIds = c("H1_NI", "H1_S"),
                testType = "parametric",
                correlation = matrix(c(1, 0.5, 0.5, 1), nrow = 2)
            ),
            list(
                id = "secondary-group",
                hypothesisIds = c("H2_NI", "H2_S"),
                testType = "bonferroni"
            )
        )
    )
    expect_true(project$validate())

    invalid <- project$toList()
    invalid$analyses[[2L]]$testGroups[[1L]]$correlation <- list(
        c(1, 0.9), c(0.2, 1)
    )
    expect_error(alphaProjectFromList(invalid), "symmetric")
})

test_that("shortcut results are separate, reproducible, and invalidated by inputs", {
    skip_if_not_installed("graphicalMCP")
    project <- createDemoAlphaProject()
    result <- project$runShortcut("shortcut-primary")

    expect_true(project$resultIsCurrent("shortcut-primary"))
    expect_true(result$engine$graphicalMCPVersion >= "0.2.9")
    expect_equal(
        .projectResultValues(result$adjustedP),
        c(H1_NI = 0.012, H1_S = 0.012, H2_NI = 0.04, H2_S = 0.162)
    )

    project$upsertDataset(
        "primary",
        c(H1_NI = 0.02, H1_S = 0.001, H2_NI = 0.030, H2_S = 0.162)
    )
    expect_null(project$result("shortcut-primary"))
})

test_that("documented graphicalMCP closure example is a shortcut regression test", {
    skip_if_not_installed("graphicalMCP")
    hypotheses <- c(H1 = 0.5, H2 = 0.5, H3 = 0, H4 = 0, H5 = 0, H6 = 0)
    epsilon <- 1e-5
    transitions <- rbind(
        c(0, 0.5, 0.25, 0, 0.25, 0),
        c(0.5, 0, 0, 0.25, 0, 0.25),
        c(0, 0, 0, 0, 1, 0),
        c(epsilon, 0, 0, 0, 0, 1 - epsilon),
        c(0, epsilon, 1 - epsilon, 0, 0, 0),
        c(0, 0, 0, 1, 0, 0)
    )
    dimnames(transitions) <- list(names(hypotheses), names(hypotheses))
    graph <- alphaGraphFromTransitionMatrix(
        transitions,
        hypotheses = hypotheses,
        nodeIds = names(hypotheses),
        nodeLabels = names(hypotheses)
    )
    project <- AlphaProject$new(graph)
    project$upsertDataset(
        "reference",
        c(H1 = 0.015, H2 = 0.013, H3 = 0.01, H4 = 0.007, H5 = 0.1, H6 = 0.0124)
    )
    project$upsertAnalysis(
        "reference-shortcut",
        "reference",
        method = "shortcut",
        alpha = 0.025
    )
    result <- project$runShortcut("reference-shortcut")

    expect_equal(
        .projectResultValues(result$adjustedP),
        c(H1 = 0.026, H2 = 0.026, H3 = 0.028, H4 = 0.028, H5 = 0.1, H6 = 0.028),
        tolerance = 1e-10
    )
    expect_false(any(.projectResultValues(result$rejected)))
})

test_that("analysis table joins graph, raw inputs, and current results", {
    skip_if_not_installed("graphicalMCP")
    project <- createDemoAlphaProject()
    before <- .analysisTableData(project)
    expect_true(all(is.na(before[["Adjusted p-value"]])))

    project$runShortcut("shortcut-primary")
    after <- .analysisTableData(project)
    expect_equal(after[["Raw p-value"]], c(0.012, 0.001, 0.030, 0.162))
    expect_equal(after[["Adjusted p-value"]], c(0.012, 0.012, 0.04, 0.162))
    expect_identical(after$Rejected, c(TRUE, TRUE, FALSE, FALSE))
})

test_that("analysis module runs against the shared project store", {
    skip_if_not_installed("graphicalMCP")
    store <- createAlphaProjectStore(createDemoAlphaProject())

    shiny::testServer(
        moduleAnalysisServer,
        args = list(projectStore = store),
        {
            session$setInputs(alpha = 0.025, runShortcut = 0)
            session$flushReact()
            session$setInputs(runShortcut = 1)
            session$flushReact()
            expect_false(is.null(store$result("shortcut-primary")))
            expect_match(output$analysisStatus, "graphicalMCP")

            store$graphStore$transact("test:graph-change", function(graph) {
                graph$setNodeWeight("H1_NI", 0.9)
                graph$setNodeWeight("H1_S", 0.1)
            }, requireNumeric = TRUE)
            session$flushReact()
            expect_null(store$result("shortcut-primary"))
        }
    )
})

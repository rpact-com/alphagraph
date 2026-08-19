test_that("AlphaGraphStore commits atomic revisions and blocks nested updates", {
    shiny::testServer(function(input, output, session) {
        store <- createAlphaGraphStore()
        session$userData$store <- store
    }, {
        store <- session$userData$store
        start <- store$revision()
        nestedResult <- NULL

        store$transact("outer", function(graph) {
            nestedResult <<- store$transact("nested", function(candidate) {
                candidate$addNode("nested")
            })
            graph$setNodeColor("H1_NI", background = "red")
        })

        expect_false(nestedResult)
        expect_equal(store$revision(), start + 1L)
        expect_equal(store$source(), "outer")
        expect_equal(
            store$graph()$toVisNetworkData()$nodes$color.background[1],
            "red"
        )
        expect_false("nested" %in% store$graph()$toVisNetworkData()$nodes$id)
    })
})

test_that("graph toolbar has one view-lock toggle", {
    html <- as.character(moduleGraphUI("graph"))

    expect_match(html, 'id="graph-lock_pos"', fixed = TRUE)
    expect_match(html, "fa-lock", fixed = TRUE)
    expect_false(grepl("unlock_pos", html, fixed = TRUE))
    expect_match(html, 'id="graph-reset_view"', fixed = TRUE)
    expect_match(html, 'id="graph-zoom_in"', fixed = TRUE)
    expect_match(html, 'id="graph-zoom_out"', fixed = TRUE)
})

test_that("graph view controls accept clicks and validate their zoom factor", {
    shiny::testServer(function(input, output, session) {
        moduleGraphServer(
            "graph",
            graphStore = createAlphaGraphStore(createDemoAlphaGraph()),
            args = list(zoomFactor = 1.25)
        )
    }, {
        session$flushReact()
        session$setInputs(`graph-zoom_in` = 1)
        session$setInputs(`graph-zoom_out` = 1)
        session$setInputs(`graph-reset_view` = 1)
        session$flushReact()
        succeed()
    })

    expect_error(
        shiny::testServer(function(input, output, session) {
            moduleGraphServer(
                "graph",
                graphStore = createAlphaGraphStore(createDemoAlphaGraph()),
                args = list(zoomFactor = 1)
            )
        }),
        "zoomFactor"
    )
})

test_that("matrix and graph modules share one canonical graph", {
    shiny::testServer(function(input, output, session) {
        store <- createAlphaGraphStore()
        moduleTransitionMatrixServer("matrix", graphStore = store)
        moduleGraphServer(
            "graph",
            graphStore = store,
            args = list(positionDebounceMillis = 1L)
        )
        session$userData$store <- store
    }, {
        store <- session$userData$store
        session$flushReact()
        start <- store$revision()

        matrix <- store$snapshot()$toTransitionMatrix(
            type = "value", asDataFrame = FALSE, rowNames = "id"
        )
        matrix["H1_NI", "H1_S"] <- 1 / 4
        display <- alphaGraphFormatMatrix(matrix, store$snapshot()$numberFormat())
        .updateGraphFromTransitionTable(store, display)
        session$flushReact()

        expect_equal(store$revision(), start + 1L)
        expect_equal(store$source(), "matrix:table-edit")
        expect_equal(
            store$graph()$toTransitionMatrix(
                type = "value",
                asDataFrame = FALSE,
                rowNames = "id"
            )["H1_NI", "H1_S"],
            0.25
        )
        expect_equal(
            store$graph()$toVisNetworkData()$edges$label[
                store$graph()$toVisNetworkData()$edges$id == "H1_NI__H1_S"
            ],
            "1/4"
        )

        session$setInputs(`graph-add_node` = 1)
        session$flushReact()

        expect_equal(store$revision(), start + 2L)
        expect_equal(store$source(), "graph:add-node")
        expect_equal(nrow(store$graph()$toVisNetworkData()$nodes), 5L)
        expect_equal(
            nrow(store$graph()$toTransitionMatrix(asDataFrame = TRUE)),
            5L
        )
    })
})

test_that("invalid matrix edits do not commit a revision", {
    shiny::testServer(function(input, output, session) {
        store <- createAlphaGraphStore()
        moduleTransitionMatrixServer("matrix", graphStore = store)
        session$userData$store <- store
    }, {
        store <- session$userData$store
        session$flushReact()
        start <- store$revision()

        matrix <- store$snapshot()$toTransitionMatrix(
            type = "value", asDataFrame = FALSE, rowNames = "id"
        )
        matrix[1, 1] <- 0.5
        expect_error(
            .updateGraphFromTransitionTable(store, matrix),
            "diagonal"
        )
        session$flushReact()

        expect_equal(store$revision(), start)
        expect_equal(
            store$graph()$toTransitionMatrix(
                type = "value",
                asDataFrame = FALSE,
                rowNames = "id"
            )[1, 1],
            0
        )
    })
})

test_that("matrix zero/non-zero edits create and remove graph edges", {
    shiny::testServer(function(input, output, session) {
        store <- createAlphaGraphStore()
        moduleTransitionMatrixServer("matrix", graphStore = store)
        moduleGraphServer("graph", graphStore = store)
        session$userData$store <- store
    }, {
        store <- session$userData$store
        session$flushReact()
        start <- store$revision()

        matrix <- store$snapshot()$toTransitionMatrix(
            type = "value", asDataFrame = FALSE, rowNames = "id"
        )
        matrix["H2_S", "H1_NI"] <- 1 / 3
        display <- alphaGraphFormatMatrix(matrix, store$snapshot()$numberFormat())
        .updateGraphFromTransitionTable(store, display)
        session$flushReact()

        edges <- store$graph()$toVisNetworkData()$edges
        expect_equal(store$revision(), start + 1L)
        expect_true("H2_S__H1_NI" %in% edges$id)
        expect_equal(edges$label[edges$id == "H2_S__H1_NI"], "1/3")

        matrix["H2_S", "H1_NI"] <- 0
        display <- alphaGraphFormatMatrix(matrix, store$snapshot()$numberFormat())
        .updateGraphFromTransitionTable(store, display)
        session$flushReact()

        expect_equal(store$revision(), start + 2L)
        expect_false("H2_S__H1_NI" %in% store$graph()$toVisNetworkData()$edges$id)
    })
})

test_that("matrix edits curve reciprocal edges and straighten a remaining edge", {
    graph <- AlphaGraph$new()
    graph$addNode("H1", weight = 1)
    graph$addNode("H2", weight = 0)

    shiny::testServer(function(input, output, session) {
        store <- createAlphaGraphStore(graph)
        moduleTransitionMatrixServer("matrix", graphStore = store)
        session$userData$store <- store
    }, {
        store <- session$userData$store
        matrix <- matrix(
            c(0, 0.5, 0, 0),
            nrow = 2,
            byrow = TRUE,
            dimnames = list(c("H1", "H2"), c("H1", "H2"))
        )

        .updateGraphFromTransitionTable(store, matrix)
        edges <- store$snapshot()$toList()$edges
        expect_length(edges, 1L)
        expect_false(edges[[1L]]$vis$smooth)

        matrix["H2", "H1"] <- 0.5
        .updateGraphFromTransitionTable(store, matrix)
        edges <- store$snapshot()$toList()$edges
        expect_true(all(vapply(
            edges,
            function(edge) edge$vis$smooth,
            logical(1L)
        )))

        matrix["H2", "H1"] <- 0
        .updateGraphFromTransitionTable(store, matrix)
        edges <- store$snapshot()$toList()$edges
        expect_length(edges, 1L)
        expect_false(edges[[1L]]$vis$smooth)
    })
})

test_that("hypothesis labels synchronize with graph and matrix views", {
    shiny::testServer(function(input, output, session) {
        store <- createAlphaGraphStore()
        moduleGraphServer("graph", graphStore = store)
        moduleTransitionMatrixServer("matrix", graphStore = store)
        moduleHypothesisServer("hypotheses", graphStore = store)
        session$userData$store <- store
    }, {
        store <- session$userData$store
        session$flushReact()
        start <- store$revision()

        table <- .hypothesisTableData(
            store$snapshot(),
            c(H1_NI = 0.012, H1_S = 0.001, H2_NI = 0.030, H2_S = 0.162)
        )
        table$Hypothesis[table$ID == "H1_S"] <- "Primary"
        .updateGraphFromHypothesisTable(
            store,
            table,
            stats::setNames(as.numeric(table[["p-Value"]]), table$ID)
        )
        session$flushReact()

        graph <- store$graph()
        nodes <- graph$toList()$nodes
        node <- Filter(function(x) x$id == "H1_S", nodes)[[1L]]
        matrix <- graph$toTransitionMatrix(
            type = "expression",
            asDataFrame = FALSE,
            rowNames = "label"
        )

        expect_equal(store$revision(), start + 1L)
        expect_equal(store$source(), "hypotheses:table-edit")
        expect_equal(node$label, "Primary")
        expect_match(node$vis$label, "Primary", fixed = TRUE)
        expect_true("Primary" %in% rownames(matrix))
        expect_true("Primary" %in% colnames(matrix))
    })
})

test_that("hypothesis weights synchronize with node labels and graphicalMCP", {
    graph <- createDemoAlphaGraph()
    graph$setNodeWeight("H1_NI", weight = 0.8)
    graph$setNodeWeight("H1_S", weight = 0.2)

    shiny::testServer(function(input, output, session) {
        store <- createAlphaGraphStore(graph)
        moduleHypothesisServer("hypotheses", graphStore = store)
        session$userData$store <- store
    }, {
        store <- session$userData$store
        session$flushReact()

        initial <- store$graph()$toList()$nodes
        h1s <- Filter(function(x) x$id == "H1_S", initial)[[1L]]
        table <- .hypothesisTableData(
            store$snapshot(),
            c(H1_NI = 0.012, H1_S = 0.001, H2_NI = 0.030, H2_S = 0.162)
        )
        expect_equal(h1s$weight$value, 0.2)
        expect_match(h1s$vis$label, "\\n1/5$")
        expect_equal(table$Weight[table$ID == "H1_S"], "1/5")
        expect_equal(table[["Rendered label"]][table$ID == "H1_S"],
                     alphaGraphRenderHypothesisLabel("H1_(s)"))

        start <- store$revision()
        table$Weight[table$ID == "H1_S"] <- "1/10"
        .updateGraphFromHypothesisTable(
            store,
            table,
            stats::setNames(as.numeric(table[["p-Value"]]), table$ID)
        )
        session$flushReact()

        updated <- store$graph()
        node <- Filter(
            function(x) x$id == "H1_S",
            updated$toList()$nodes
        )[[1L]]
        gmcp <- updated$toGraphicalMCPInput(hypNames = "id")

        expect_equal(store$revision(), start + 1L)
        expect_equal(store$source(), "hypotheses:table-edit")
        expect_equal(node$weight$value, 0.1)
        expect_equal(node$weight$display, "1/10")
        expect_match(node$vis$label, "\\n1/10$")
        expect_equal(unname(gmcp$hypotheses["H1_S"]), 0.1)
    })
})

test_that("invalid hypothesis edits are not committed", {
    shiny::testServer(function(input, output, session) {
        store <- createAlphaGraphStore()
        moduleHypothesisServer("hypotheses", graphStore = store)
        session$userData$store <- store
    }, {
        store <- session$userData$store
        session$flushReact()
        start <- store$revision()

        table <- .hypothesisTableData(
            store$snapshot(),
            c(H1_NI = 0.012, H1_S = 0.001, H2_NI = 0.030, H2_S = 0.162)
        )

        longLabelTable <- table
        longLabelTable$Hypothesis[longLabelTable$ID == "H1_S"] <- "H1234567"
        expect_error(
            .updateGraphFromHypothesisTable(
                store,
                longLabelTable,
                stats::setNames(
                    as.numeric(longLabelTable[["p-Value"]]),
                    longLabelTable$ID
                )
            ),
            "effective length 8; the maximum is 7"
        )
        expect_equal(store$revision(), start)

        table$Weight[table$ID == "H1_S"] <- "0.5"
        expect_error(
            .updateGraphFromHypothesisTable(
                store,
                table,
                stats::setNames(as.numeric(table[["p-Value"]]), table$ID)
            ),
            "sum to at most 1"
        )
        session$flushReact()

        expect_equal(store$revision(), start)
        expect_equal(unname(store$graph()$toHypotheses()["H1_S"]), 0)
    })
})

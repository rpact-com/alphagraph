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

        session$setInputs(
            `matrix-transitionMatrix_cell_edit` = list(
                row = 1,
                col = 2,
                value = "0.25"
            )
        )
        session$flushReact()

        expect_equal(store$revision(), start + 1L)
        expect_equal(store$source(), "matrix:cell-edit")
        expect_equal(
            store$graph()$toTransitionMatrix(
                type = "expression",
                asDataFrame = FALSE,
                rowNames = "id"
            )["H1_NI", "H1_S"],
            "0.25"
        )
        expect_equal(
            store$graph()$toVisNetworkData()$edges$label[
                store$graph()$toVisNetworkData()$edges$id == "H1_NI__H1_S"
            ],
            "0.25"
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

        session$setInputs(
            `matrix-transitionMatrix_cell_edit` = list(
                row = 1,
                col = 1,
                value = "0.5"
            )
        )
        session$flushReact()

        expect_equal(store$revision(), start)
        expect_equal(
            store$graph()$toTransitionMatrix(
                type = "expression",
                asDataFrame = FALSE,
                rowNames = "id"
            )[1, 1],
            "0"
        )
    })
})

test_that("canvas grid and snapping share the graph configuration", {
    data <- createDemoAlphaGraph()$toList()
    data$settings$grid <- list(size = 24.5, origin = list(x = 3.5, y = -7.25))
    graph <- AlphaGraph$new()
    graph$loadList(data)
    widget <- getDemoGraph(shiny::NS("graph"), graph)
    expect_true(any(vapply(widget$dependencies, function(dep) {
        dep$name == "alphagraph-grid" && dep$script == "graph-grid.js"
    }, logical(1L))))
    for (event in c("initRedraw", "dragging", "beforeDrawing", "dragEnd")) {
        expect_match(widget$x$events[[event]],
                     '{"size":24.5,"origin":{"x":3.5,"y":-7.25}}', fixed = TRUE)
    }
    expect_match(widget$x$events$dragEnd, '"graph-node_positions"', fixed = TRUE)
    expect_null(widget$x$events$afterDrawing)

    data$settings$grid$size <- 0
    graph$loadList(data)
    expect_error(getDemoGraph(identity, graph), "Grid size must be positive")
})

test_that("snapped node positions persist across later graph edits", {
    shiny::testServer(function(input, output, session) {
        store <- createAlphaGraphStore(createDemoAlphaGraph())
        moduleGraphServer("graph", graphStore = store, args = list(positionDebounceMillis = 1L))
        session$userData$store <- store
    }, {
        session$flushReact()
        session$setInputs(`graph-node_positions` = list(nodes = list(
            list(id = "H1_NI", x = 54, y = -45)
        )))
        session$elapse(10)
        store <- session$userData$store
        store$transact("test:color", function(graph) {
            graph$setNodeColor("H1_NI", background = "white")
        })
        node <- Filter(function(node) node$id == "H1_NI", store$snapshot()$toList()$nodes)[[1L]]
        expect_equal(node$vis$x, 54)
        expect_equal(node$vis$y, -45)
    })
})

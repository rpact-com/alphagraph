test_that("drawn edges open a prefilled weight dialog and save through the graph store", {
    modal <- NULL
    dialogCount <- 0L
    notices <- character()
    local_mocked_bindings(
        showModal = function(ui, ...) {
            modal <<- as.character(ui)
            dialogCount <<- dialogCount + 1L
        },
        showNotification = function(ui, ...) notices <<- c(notices, ui),
        .package = "shiny"
    )
    shiny::testServer(function(input, output, session) {
        graph <- AlphaGraph$new()
        graph$addNode("A", label = "A", weight = 1)
        graph$addNode("B", label = "B", weight = 0)
        graph$addNode("C", label = "C", weight = 0)
        store <- createAlphaGraphStore(graph)
        moduleGraphServer("graph", graphStore = store)
        session$userData$store <- store
    }, {
        store <- session$userData$store
        session$flushReact()
        session$setInputs(`graph-edge_drawn` = list(from = "C", to = "B"))
        expect_match(modal, 'value="C" selected', fixed = TRUE)
        expect_match(modal, 'value="B" selected', fixed = TRUE)
        expect_match(modal, 'graph-edge_transition', fixed = TRUE)
        expect_length(store$snapshot()$toList()$edges, 0L)

        session$setInputs(`graph-edge_from` = "C", `graph-edge_to` = "B",
                          `graph-edge_transition` = "1/2", `graph-save_edge` = 1)
        edges <- store$snapshot()$toList()$edges
        expect_length(edges, 1L)
        expect_equal(edges[[1L]]$from, "C")
        expect_equal(edges[[1L]]$to, "B")
        expect_equal(edges[[1L]]$transition$value, 0.5)
        expect_equal(store$revision(), 1L)

        session$setInputs(`graph-edge_drawn` = list(from = "C", to = "B", nonce = 2))
        expect_equal(dialogCount, 1L)
        expect_match(tail(notices, 1), "already exists", fixed = TRUE)
        session$setInputs(`graph-edge_transition` = "1/4", `graph-save_edge` = 2)
        expect_equal(store$revision(), 1L)

        for (pair in list(list(from = "A", to = "A"), list(from = "missing", to = "B"))) {
            session$setInputs(`graph-edge_drawn` = pair)
            expect_equal(dialogCount, 1L)
        }
        session$setInputs(`graph-edge_from` = "A", `graph-edge_to` = "A",
                          `graph-save_edge` = 3)
        expect_equal(store$revision(), 1L)

        session$setInputs(`graph-edge_from` = "A", `graph-edge_to` = "B",
                          `graph-edge_transition` = "2", `graph-save_edge` = 4)
        expect_equal(store$revision(), 1L)

        # Reverse direction remains a separate, permitted transition.
        session$setInputs(`graph-edge_drawn` = list(from = "B", to = "C"))
        expect_match(modal, "Add transition", fixed = TRUE)
        session$setInputs(`graph-edge_from` = "B", `graph-edge_to` = "C",
                          `graph-edge_transition` = "1/4", `graph-save_edge` = 5)
        expect_length(store$snapshot()$toList()$edges, 2L)
    })
})

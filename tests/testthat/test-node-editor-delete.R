test_that("node editor deletes the edited node only after confirmation", {
    modal <- NULL
    closed <- FALSE
    local_mocked_bindings(
        showModal = function(ui, ...) modal <<- as.character(ui),
        removeModal = function(...) closed <<- TRUE,
        .package = "shiny"
    )

    shiny::testServer(function(input, output, session) {
        store <- createAlphaGraphStore(createDemoAlphaGraph())
        moduleGraphServer("graph", graphStore = store)
        session$userData$store <- store
    }, {
        store <- session$userData$store
        session$flushReact()
        before <- store$snapshot()$toList()
        session$setInputs(`graph-node_right_click` = list(id = "H1_S", x = 20, y = 20))
        session$setInputs(`graph-ctx_action` = list(action = "edit"))

        expect_match(modal, "window.confirm(&#39;Delete this node and all connected edges?&#39;)",
                     fixed = TRUE)
        expect_match(modal, "graph-delete_edited_node", fixed = TRUE)
        expect_match(modal, "float: left; margin-right: auto;", fixed = TRUE)
        expect_lt(regexpr(">Delete</button>", gsub("\\s+", "", modal), fixed = TRUE)[1L],
                  regexpr(">Cancel</button>", gsub("\\s+", "", modal), fixed = TRUE)[1L])
        expect_equal(store$snapshot()$toList(), before)
        expect_false(closed)

        # Only a confirmed browser prompt emits this input event.
        session$setInputs(`graph-delete_edited_node` = 1)
        after <- store$snapshot()$toList()
        expect_false("H1_S" %in% vapply(after$nodes, function(node) node$id, character(1L)))
        expect_length(after$nodes, length(before$nodes) - 1L)
        expect_false(any(vapply(after$edges, function(edge) {
            edge$from == "H1_S" || edge$to == "H1_S"
        }, logical(1L))))
        expect_equal(store$revision(), 1L)
        expect_true(closed)
    })
})

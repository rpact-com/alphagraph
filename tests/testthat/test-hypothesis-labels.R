test_that("editable hypothesis labels render supported Unicode subscripts", {
    expect_equal(getHypothesisLabel(1, "ni"), "H1_(ni)")
    expect_equal(
        alphaGraphRenderHypothesisLabel("H1_ni"),
        paste0("H1", "\u2099", "\u1D62")
    )
    expect_equal(
        alphaGraphRenderHypothesisLabel("H1_(ni)"),
        paste0("H1", "\u208D", "\u2099", "\u1D62", "\u208E")
    )
    expect_equal(
        alphaGraphRenderHypothesisLabel("H1_NI"),
        alphaGraphRenderHypothesisLabel("H1_ni")
    )
    expect_equal(alphaGraphRenderHypothesisLabel("Primary endpoint"),
                 "Primary endpoint")
})

test_that("unsupported or ambiguous subscript syntax is rejected", {
    expect_error(alphaGraphRenderHypothesisLabel("H1_beta"),
                 "Unsupported subscript character")
    expect_error(alphaGraphRenderHypothesisLabel("H1_(ni"),
                 "Parentheses")
    expect_error(alphaGraphRenderHypothesisLabel("H1_ni_extra"),
                 "one underscore")
    expect_error(alphaGraphRenderHypothesisLabel("H1_"),
                 "non-empty subscript")
})

test_that("canonical and rendered labels remain separate", {
    graph <- AlphaGraph$new()
    graph$addNode("H1", label = "H1_(ni)", weight = 1)

    node <- graph$toList()$nodes[[1L]]

    expect_equal(node$label, "H1_(ni)")
    expect_equal(
        node$vis$label,
        paste0("H1", "\u208D", "\u2099", "\u1D62", "\u208E", "\n1")
    )
})

test_that("labels must also be unique after rendering", {
    expect_error(
        alphaGraphValidateHypothesisLabels(c("H1_ni", "H1_NI")),
        "unique after rendering"
    )
})

test_that("effective label length accounts for compact subscripts", {
    expect_equal(alphaGraphHypothesisLabelLength("H_123456"), 4)
    expect_equal(alphaGraphHypothesisLabelLength("H123456"), 7)
    expect_equal(alphaGraphHypothesisLabelLength("H1_123"), 4)

    expect_silent(alphaGraphValidateHypothesisLabelLengths("H123456"))
    expect_error(
        alphaGraphValidateHypothesisLabelLengths("H1234567"),
        "effective length 8; the maximum is 7"
    )
})

test_that("maximum effective label length is centrally configurable", {
    oldOptions <- options(alphagraph.maxHypothesisLabelLength = 4L)
    on.exit(options(oldOptions), add = TRUE)

    expect_equal(.alphaGraphMaxHypothesisLabelLength(), 4L)
    expect_silent(alphaGraphValidateHypothesisLabelLengths("H_123456"))
    expect_error(alphaGraphValidateHypothesisLabelLengths("H12345"),
                 "maximum is 4")
})

test_that("graph property editor stores syntax and renders its subscript", {
    oldLocale <- Sys.getlocale("LC_CTYPE")
    on.exit(Sys.setlocale("LC_CTYPE", oldLocale), add = TRUE)
    Sys.setlocale("LC_CTYPE", "C")

    modal <- NULL
    local_mocked_bindings(
        showModal = function(ui, ...) modal <<- as.character(ui),
        .package = "shiny"
    )

    shiny::testServer(function(input, output, session) {
        store <- createAlphaGraphStore(createDemoAlphaGraph())
        moduleGraphServer("graph", graphStore = store)
        session$userData$store <- store
    }, {
        store <- session$userData$store
        session$flushReact()

        session$setInputs(
            `graph-selection` = list(nodes = "H1_S", edges = character()),
            `graph-edit_node` = 1
        )
        session$flushReact()
        session$setInputs(`graph-node_label` = "H1_(ni)")
        expect_match(modal, '"theme":"nano"', fixed = TRUE)
        expect_match(modal, 'id="graph-node_color"', fixed = TRUE)
        expect_equal(
            as.character(output[["graph-node_label_preview"]]$html),
            paste0("<span>", alphaGraphRenderHypothesisLabel("H1_(ni)"), "</span>")
        )
        session$setInputs(`graph-node_label` = "H1_beta")
        expect_error(output[["graph-node_label_preview"]],
                     "Unsupported subscript character")
        session$setInputs(`graph-node_label` = "H1_ni")
        expect_equal(
            as.character(output[["graph-node_label_preview"]]$html),
            paste0("<span>", alphaGraphRenderHypothesisLabel("H1_ni"), "</span>")
        )
        session$setInputs(`graph-node_label` = "<b>H1</b>")
        expect_equal(as.character(output[["graph-node_label_preview"]]$html),
                     "<span>&lt;b&gt;H1&lt;/b&gt;</span>")
        expect_equal(store$revision(), 0L)
        session$setInputs(
            `graph-node_label` = "H1_ni",
            `graph-node_color` = "#2468AC",
            `graph-save_node_label` = 1
        )
        session$flushReact()

        node <- Filter(
            function(item) item$id == "H1_S",
            store$snapshot()$toList()$nodes
        )[[1L]]
        expect_equal(node$label, "H1_ni")
        expect_equal(node$vis$color$background, "#2468AC")
        expect_match(
            node$vis$label,
            paste0("H1", "\u2099", "\u1D62"),
            fixed = TRUE
        )
        session$setInputs(`graph-edit_node` = 2)
        expect_match(modal, '"default":"#2468AC"', fixed = TRUE)

        # Editing alone leaves the graph unchanged, and invalid labels cannot
        # partially commit a new color.
        session$setInputs(`graph-node_color` = "#123456")
        expect_equal(store$revision(), 1L)
        session$setInputs(`graph-node_label` = "H1_beta",
                          `graph-save_node_label` = 2)
        expect_equal(store$revision(), 1L)
        expect_equal(
            Filter(function(item) item$id == "H1_S",
                   store$snapshot()$toList()$nodes)[[1L]]$vis$color$background,
            "#2468AC"
        )
    })
})

test_that("circle labels are drawn at the node center", {
    widget <- getDemoGraph(identity, createDemoAlphaGraph())
    beforeDrawing <- widget$x$events$beforeDrawing

    expect_match(beforeDrawing, "node.options.shape !== 'circle'", fixed = TRUE)
    expect_match(beforeDrawing, "node.x", fixed = TRUE)
    expect_match(beforeDrawing, "__alphaGraphCentered", fixed = TRUE)
})

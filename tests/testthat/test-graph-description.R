test_that("description template contains canonical graph information", {
    graph <- AlphaGraph$new(
        id = "custom_graph",
        label = "Custom *Graph*",
        description = "A study-specific graph.",
        metadata = list(context = "Confirmatory analysis")
    )
    graph$addNode(
        "primary_ni",
        label = "Primary NI",
        weight = 1,
        metadata = list(meaning = "Non-inferiority of the primary endpoint")
    )
    graph$addNode("primary_s", label = "Primary superiority", weight = 0)
    graph$addEdge("primary_ni", "primary_s", transition = 0.5)

    description <- getGraphDescription(
        graph,
        catalogPath = tempfile(fileext = ".json")
    )

    expect_match(description, "# Custom \\*Graph\\*", fixed = TRUE)
    expect_match(description, "`custom_graph`", fixed = TRUE)
    expect_match(description, "A study-specific graph.", fixed = TRUE)
    expect_match(description, "Non-inferiority of the primary endpoint", fixed = TRUE)
    expect_match(description, "Primary NI → Primary superiority", fixed = TRUE)
    expect_match(description, "1/2", fixed = TRUE)
    expect_match(description, "Confirmatory analysis", fixed = TRUE)
    expect_match(description, "Please add the clinical or statistical meaning", fixed = TRUE)
    expect_false(grepl("Hung H.M.J.", description, fixed = TRUE))
})

test_that("known graph descriptions are enriched from the JSON catalog", {
    description <- getGraphDescription(createDemoAlphaGraph())

    expect_match(description, "Non-inferiority of the primary endpoint", fixed = TRUE)
    expect_match(description, "Superiority of the secondary endpoint", fixed = TRUE)
    expect_match(description, "Hung H.M.J., Wang S.-J. (2010)", fixed = TRUE)
    expect_match(description, "10.1002/bimj.200900206", fixed = TRUE)
})

test_that("symbolic graphs produce a useful template without evaluation", {
    graph <- AlphaGraph$new(id = "symbolic", label = "Symbolic graph")
    graph$addParameter("nu", label = "Nu", description = "Allocation parameter")
    graph$addNode("H1", weight = "nu")
    graph$addNode("H2", weight = "1 - nu")
    graph$addEdge("H1", "H2", transition = "nu")

    description <- getGraphDescription(
        graph,
        catalogPath = tempfile(fileext = ".json")
    )

    expect_match(description, "## Parameters", fixed = TRUE)
    expect_match(description, "Allocation parameter", fixed = TRUE)
    expect_match(description, "symbolic / not evaluable", fixed = TRUE)
    expect_match(description, "**H1 → H2:** nu", fixed = TRUE)
})

test_that("description module follows the store without overwriting edits", {
    shiny::testServer(function(input, output, session) {
        store <- createAlphaGraphStore(createDemoAlphaGraph())
        moduleDescriptionServer("description", graphStore = store)
        session$userData$store <- store
    }, {
        store <- session$userData$store
        session$flushReact()
        expect_match(
            output[["description-descContent"]]$html,
            "Hung and Wang (2010)",
            fixed = TRUE
        )

        store$transact("test:label", function(graph) {
            graph$setNodeLabel("H1_NI", "Updated primary NI")
        })
        session$flushReact()
        expect_match(
            output[["description-descContent"]]$html,
            "Updated primary NI",
            fixed = TRUE
        )

        session$setInputs(`description-descModeEdit` = 1)
        session$flushReact()
        session$setInputs(
            `description-descMarkdownInput` = "# My manually edited description",
            `description-descModePreview` = 1
        )
        session$flushReact()

        store$transact("test:second-label", function(graph) {
            graph$setNodeLabel("H1_NI", "Changed again")
        })
        session$flushReact()
        expect_match(
            output[["description-descContent"]]$html,
            "My manually edited description",
            fixed = TRUE
        )
        expect_false(grepl(
            "Changed again",
            output[["description-descContent"]]$html,
            fixed = TRUE
        ))

        session$setInputs(`description-descRegenerateConfirm` = 1)
        session$flushReact()
        expect_match(
            output[["description-descContent"]]$html,
            "Changed again",
            fixed = TRUE
        )

        store$transact("test:third-label", function(graph) {
            graph$setNodeLabel("H1_NI", "Automatically refreshed")
        })
        session$flushReact()
        expect_match(
            output[["description-descContent"]]$html,
            "Automatically refreshed",
            fixed = TRUE
        )
    })
})

test_that("invalid description catalogs fail with a useful message", {
    path <- tempfile(fileext = ".json")
    writeLines('{"schema_version":1,"items":[]}', path)

    expect_error(
        getGraphDescription(createDemoAlphaGraph(), catalogPath = path),
        "expected a 'graphs' array",
        fixed = TRUE
    )
})

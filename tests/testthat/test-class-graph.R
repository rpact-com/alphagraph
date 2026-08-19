test_that("canonical graph JSON round-trips without loss", {
    graph <- createDemoAlphaGraph()
    restored <- alphaGraphFromJson(graph$toJson())

    expect_identical(restored$toList(), graph$toList())
    expect_match(graph$toJson(), '"metadata": \\{\\}', perl = TRUE)
    expect_match(graph$toJson(), '"maxDenominator": 16', fixed = TRUE)
    expect_true(restored$validate(requireNumeric = FALSE))
})

test_that("graph view is locked by default while nodes remain draggable", {
    interaction <- createDemoAlphaGraph()$toList()$settings$vis$interaction

    expect_false(interaction$dragView)
    expect_true(interaction$dragNodes)
})

test_that("demo graph and transition matrix use the same canonical edges", {
    graph <- createDemoAlphaGraph()
    matrix <- getTransitionMatrix()
    vis <- graph$toVisNetworkData()

    expect_equal(matrix[getHypothesisLabel(1, "s"), getHypothesisLabel(2, "ni")], "1/2")
    expect_equal(matrix[getHypothesisLabel(2, "ni"), getHypothesisLabel(1, "s")], "1/2")
    expect_equal(nrow(vis$edges), sum(matrix != "0"))
    expect_true(all(vis$edges$label == "1/2"))
})

test_that("matrix updates preserve styles and synchronize edges", {
    graph <- createDemoAlphaGraph()
    matrix <- graph$toTransitionMatrix(
        type = "value",
        asDataFrame = FALSE,
        rowNames = "id"
    )
    matrix["H1_S", "H2_NI"] <- 0.25
    matrix["H2_NI", "H1_S"] <- 0
    matrix["H2_S", "H1_NI"] <- 0.5

    graph$updateFromTransitionMatrix(matrix, type = "value")
    data <- graph$toList()
    edge <- Filter(function(x) x$from == "H1_S" && x$to == "H2_NI", data$edges)[[1L]]

    expect_false(edge$vis$smooth)
    expect_equal(edge$transition$value, 0.25)
    expect_length(Filter(function(x) x$from == "H2_NI" && x$to == "H1_S", data$edges), 0L)
    expect_length(Filter(function(x) x$from == "H2_S" && x$to == "H1_NI", data$edges), 1L)
})

test_that("reciprocal edges curve and single edges remain straight", {
    graph <- AlphaGraph$new()
    graph$addNode("H1")
    graph$addNode("H2")

    graph$addEdge("H1", "H2", transition = 0.5)
    edges <- graph$toList()$edges
    expect_false(edges[[1L]]$vis$smooth)

    graph$addEdge("H2", "H1", transition = 0.5)
    edges <- graph$toList()$edges
    expect_true(all(vapply(edges, function(edge) edge$vis$smooth, logical(1L))))

    graph$removeEdge(from = "H2", to = "H1")
    edges <- graph$toList()$edges
    expect_length(edges, 1L)
    expect_false(edges[[1L]]$vis$smooth)
})

test_that("graphicalMCP inputs use the canonical numeric values", {
    graph <- createDemoAlphaGraph()
    input <- graph$toGraphicalMCPInput(hypNames = "id")

    expect_equal(input$hypotheses, c(H1_NI = 1, H1_S = 0, H2_NI = 0, H2_S = 0))
    expect_equal(input$transitions["H1_NI", "H2_NI"], 0.5)
    expect_equal(input$transitions["H2_NI", "H1_S"], 0.5)
    expect_equal(unname(rowSums(input$transitions)), c(1, 1, 1, 0))

    restored <- alphaGraphFromGraphicalMCP(input)
    expect_equal(
        restored$toGraphicalMCPInput(hypNames = "id")$transitions,
        input$transitions
    )
})

test_that("fractions are presentation only and their complexity is configurable", {
    graph <- createDemoAlphaGraph()

    expect_equal(graph$parseNumber("1/2"), 0.5)
    expect_equal(graph$formatNumber(0.5), "1/2")
    expect_equal(graph$formatNumber(1 / 17), "0.05882353")

    graph$setNumberFormat(maxDenominator = 20L)
    expect_equal(graph$formatNumber(1 / 17), "1/17")
    graph$setTransitionWeight("H2_S", "H1_NI", "1/17")

    edge <- Filter(
        function(item) item$from == "H2_S" && item$to == "H1_NI",
        graph$toList()$edges
    )[[1L]]
    expect_equal(edge$transition$value, 1 / 17)
    expect_equal(edge$vis$label, "1/17")
    expect_error(graph$parseNumber("1/0"), "denominator")
    expect_error(graph$parseNumber("nu"), "number or a fraction")
})

test_that("zero edge weights remove edges and non-zero weights create them", {
    graph <- createDemoAlphaGraph()

    graph$setTransitionWeight("H1_NI", "H1_S", 0)
    expect_false("H1_NI__H1_S" %in% graph$toVisNetworkData()$edges$id)

    graph$setTransitionWeight("H1_NI", "H1_S", "1/3")
    vis <- graph$toVisNetworkData()$edges
    expect_equal(vis$label[vis$id == "H1_NI__H1_S"], "1/3")
    expect_equal(
        graph$toTransitionMatrix(type = "value", asDataFrame = FALSE,
                                 rowNames = "id")["H1_NI", "H1_S"],
        1 / 3
    )
})

test_that("JSON expressions are restricted to arithmetic and parameters", {
    graph <- AlphaGraph$new()
    graph$addNode("H1", weight = 1)
    graph$addNode("H2", weight = 0)
    graph$addEdge("H1", "H2", expression = "system('whoami')", label = "unsafe")

    expect_error(
        graph$toGraphicalMCPInput(),
        "not allowed"
    )
})

test_that("visNetwork data round-trips visual properties", {
    graph <- createDemoAlphaGraph()
    vis <- graph$toVisNetworkData()
    vis$nodes$x[vis$nodes$id == "H1_NI"] <- 123
    vis$nodes$color.background[vis$nodes$id == "H1_NI"] <- "red"

    graph$updateFromVisNetworkData(vis$nodes, vis$edges)
    updated <- graph$toVisNetworkData()

    expect_equal(updated$nodes$x[updated$nodes$id == "H1_NI"], 123)
    expect_equal(updated$nodes$color.background[updated$nodes$id == "H1_NI"], "red")
    expect_equal(unname(graph$toHypotheses()["H1_NI"]), 1)
})

test_that("a matrix can initialize a canonical graph", {
    transitions <- matrix(
        c(0, 1, 0.5, 0),
        nrow = 2,
        byrow = TRUE,
        dimnames = list(c("primary", "secondary"), c("primary", "secondary"))
    )
    graph <- alphaGraphFromTransitionMatrix(
        transitions,
        hypotheses = c(primary = 0.75, secondary = 0.25)
    )

    expect_equal(graph$toTransitionMatrix(type = "value", asDataFrame = FALSE), transitions)
    expect_true(graph$validate(requireNumeric = TRUE))
})

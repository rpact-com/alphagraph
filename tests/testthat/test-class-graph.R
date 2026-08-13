test_that("canonical graph JSON round-trips without loss", {
    graph <- createDemoAlphaGraph()
    restored <- alphaGraphFromJson(graph$toJson())

    expect_identical(restored$toList(), graph$toList())
    expect_match(graph$toJson(), '"metadata": \\{\\}', perl = TRUE)
    expect_match(graph$toJson(), '"value": null', fixed = TRUE)
    expect_true(restored$validate(requireNumeric = FALSE))
})

test_that("demo graph and transition matrix use the same canonical edges", {
    graph <- createDemoAlphaGraph()
    matrix <- getTransitionMatrix()
    vis <- graph$toVisNetworkData()

    expect_equal(matrix[getHypothesisLabel(1, "s"), getHypothesisLabel(2, "ni")], "t")
    expect_equal(matrix[getHypothesisLabel(2, "ni"), getHypothesisLabel(1, "s")], "ω")
    expect_equal(nrow(vis$edges), sum(matrix != "0"))
})

test_that("matrix updates preserve styles and synchronize edges", {
    graph <- createDemoAlphaGraph()
    matrix <- graph$toTransitionMatrix(
        type = "expression",
        asDataFrame = FALSE,
        rowNames = "id"
    )
    matrix["H1_S", "H2_NI"] <- "0.25"
    matrix["H2_NI", "H1_S"] <- "0"
    matrix["H2_S", "H1_NI"] <- "0.5"

    graph$updateFromTransitionMatrix(matrix, type = "expression")
    data <- graph$toList()
    edge <- Filter(function(x) x$from == "H1_S" && x$to == "H2_NI", data$edges)[[1L]]

    expect_true(edge$vis$smooth)
    expect_equal(edge$transition$value, 0.25)
    expect_length(Filter(function(x) x$from == "H2_NI" && x$to == "H1_S", data$edges), 0L)
    expect_length(Filter(function(x) x$from == "H2_S" && x$to == "H1_NI", data$edges), 1L)
})

test_that("graphicalMCP inputs resolve symbolic values", {
    graph <- createDemoAlphaGraph()
    input <- graph$toGraphicalMCPInput(
        parameters = list(nu = 0.3, omega = 0.4, t = 0.2),
        hypNames = "id"
    )

    expect_equal(input$hypotheses, c(H1_NI = 1, H1_S = 0, H2_NI = 0, H2_S = 0))
    expect_equal(input$transitions["H1_NI", "H2_NI"], 0.7)
    expect_equal(input$transitions["H2_NI", "H1_S"], 0.4)
    expect_equal(unname(rowSums(input$transitions)), c(1, 1, 1, 0))

    restored <- alphaGraphFromGraphicalMCP(input)
    expect_equal(
        restored$toGraphicalMCPInput(hypNames = "id")$transitions,
        input$transitions
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

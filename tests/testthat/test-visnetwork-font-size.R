test_that("visNetwork uses the reduced default font sizes", {
    graph <- AlphaGraph$new(id = "font_test")
    graph$addNode("H1")
    graph$addNode("H2")
    graph$addEdge("H1", "H2", transition = 1)

    vis <- graph$toVisNetworkData()

    expect_equal(vis$nodes$font.size, c(16.5, 16.5))
    expect_equal(vis$edges$font.size, 15)
})

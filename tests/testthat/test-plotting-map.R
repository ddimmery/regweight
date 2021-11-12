test_that("map plotting works", {
    y <- rnorm(100)
    a <- rbinom(100, 1, 0.5)
    x <- rnorm(100)
    g <- sample(c("NC", "SC", "GA", "TN"), 100, replace = TRUE)
    mappy <- list()

    m1 <- stats::lm(y ~ a + x)
    m2 <- estimatr::lm_robust(y ~ a + x)

    w1 <- calculate_weights(m1, "a")
    w2 <- calculate_weights(m2, "a")

    expect_error(plot_weighting_map(w1, g, mappy), class = "regweight_not_implemented")

    expect_error(plot_weighting_map(w2, g, mappy), class = "regweight_not_implemented")
})
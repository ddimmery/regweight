test_that("discrete plotting works", {
    y <- rnorm(100)
    a <- rbinom(100, 1, 0.5)
    x <- rnorm(100)
    g <- sample(1:4, 100, replace = TRUE)
    m1 <- stats::lm(y ~ a + x)
    m2 <- estimatr::lm_robust(y ~ a + x)

    w1 <- calculate_weights(m1, "a")
    w2 <- calculate_weights(m2, "a")

    expect_error(plot_weighting_discrete(w1, g), NA)

    expect_error(plot_weighting_discrete(w2, g), NA)
})

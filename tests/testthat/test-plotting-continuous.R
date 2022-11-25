test_that("continuous plotting works", {
    y <- rnorm(100)
    a <- rbinom(100, 1, 0.5)
    x <- rnorm(100)
    cov <- runif(100)
    m1 <- stats::lm(y ~ a + x)
    m2 <- estimatr::lm_robust(y ~ a + x)

    w1 <- calculate_weights(m1, "a")
    w2 <- calculate_weights(m2, "a")

    expect_error(plot_weighting_continuous(w1, cov), NA)

    expect_error(plot_weighting_continuous(w2, cov), NA)
})

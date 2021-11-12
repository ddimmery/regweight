test_that("s3 plotting works", {
    set.seed(100)
    
    y <- rnorm(100)
    a <- rbinom(100, 1, 0.5)
    x <- rnorm(100)
    cov <- runif(100)
    g <- sample(1:4, 100, replace = TRUE)
    gish <- sample(1:85, 100, replace = TRUE)

    m1 <- stats::lm(y ~ a + x)
    m2 <- estimatr::lm_robust(y ~ a + x)

    w1 <- calculate_weights(m1, "a")
    w2 <- calculate_weights(m2, "a")

    expect_error(plot(w1, cov), NA)
    expect_error(plot(w2, cov), NA)

    expect_error(plot(w1, g), NA)
    expect_error(plot(w2, g), NA)

    expect_error(plot(w1, gish), class = "regweight_plot_type")
    expect_error(plot(w2, gish), class = "regweight_plot_type")
})
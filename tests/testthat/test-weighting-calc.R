test_that("calculate_weights basically works", {
    y <- rnorm(100)
    a <- rbinom(100, 1, 0.5)
    x <- rnorm(100)
    m1 <- stats::lm(y ~ a + x)
    m2 <- estimatr::lm_robust(y ~ a + x)

    checkmate::expect_class(
        w1 <- calculate_weights(m1, "a"),
        "regweight"
    )

    checkmate::expect_class(
        w2 <- calculate_weights(m2, "a"),
        "regweight"
    )

    expect_equal(w1$weights, w2$weights)
})

test_that("calculate_weights works gracefully with missingness", {
    y <- rnorm(100)
    y[sample(100, 3)] <- NA
    a <- rbinom(100, 1, 0.5)
    a[sample(100, 3)] <- NA
    x <- rnorm(100)
    x[sample(100, 3)] <- NA
    m1 <- stats::lm(y ~ a + x)
    m2 <- estimatr::lm_robust(y ~ a + x)

    checkmate::expect_class(
        w1 <- calculate_weights(m1, "a"),
        "regweight"
    )

    checkmate::expect_class(
        w2 <- calculate_weights(m2, "a"),
        "regweight"
    )

    expect_equal(w1$weights, w2$weights)
})
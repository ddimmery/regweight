test_that("", {
    y <- rnorm(100)
    a <- rbinom(100, 1, 0.5)
    x <- rnorm(100)
    g1 <- sample(1:4, 100, replace = TRUE)
    g2 <- sample(1:7, 100, replace = TRUE)
    c1 <- runif(100)
    c2 <- runif(100)
    u <- replicate(100, list(a = rnorm(3)))

    m1 <- stats::lm(y ~ a + x)
    m2 <- estimatr::lm_robust(y ~ a + x)

    w <- calculate_weights(m1, "a")

    covs <- dplyr::tibble(x, g1, g2, c1, c2)
    covs_cts <- dplyr::tibble(c1, c2)
    covs_disc <- dplyr::tibble(g1, g2)
    covs_w_u <- dplyr::tibble(x, g1, g2, c1, c2, u = u)

    checkmate::expect_tibble(summary(w, covs, output = "tibble"))

    checkmate::expect_tibble(summary(w, covs_cts, output = "tibble"))

    checkmate::expect_tibble(summary(w, covs_disc, output = "tibble"))

    expect_error(
        summary(w, covs, output = "nonsense"),
        class = "regweight_table_output_type"
    )

    expect_error(
        summary(w, covs_w_u, output = "tibble"),
        class = "regweight_table_type"
    )
    checkmate::expect_class(summary(w, covs, output = "latex"), "knit_asis")
    checkmate::expect_class(summary(w, covs, output = "html"), "html")
})
test_that("s3 plotting works", {
    set.seed(100)
    
    y <- rnorm(100)
    a <- rbinom(100, 1, 0.5)
    x <- rnorm(100)
    cov <- runif(100)
    g <- sample(1:4, 100, replace = TRUE)
    a_df <- dplyr::tibble(a = letters[1:25], b = 1:25)

    m1 <- stats::lm(y ~ a + x)
    m2 <- estimatr::lm_robust(y ~ a + x)

    w1 <- calculate_weights(m1, "a")
    w2 <- calculate_weights(m2, "a")

    expect_error(plot(w1, cov), NA)
    expect_error(plot(w2, cov), NA)

    expect_error(plot(w1, g), NA)
    expect_error(plot(w2, g), NA)

    expect_error(plot(w1, a_df), class = "regweight_plot_type")
    expect_error(plot(w2, a_df), class = "regweight_plot_type")
})

test_that("s3 map plotting works", {
    skip_if_not_installed("sf")
    try(attachNamespace("sf"), silent = TRUE)
    y <- rnorm(100)
    a <- rbinom(100, 1, 0.5)
    x <- rnorm(100)
    g <- sample(c("NC", "SC", "GA", "TN"), 100, replace = TRUE)
    geoms <- USAboundaries::us_states(states = c("NC", "SC", "GA", "TN"))

    geoms <- dplyr::left_join(
        dplyr::tibble(state_abbr = g),
        dplyr::select(geoms, state_abbr, geometry),
        by = "state_abbr"
    )$geometry

    m1 <- stats::lm(y ~ a + x)
    m2 <- estimatr::lm_robust(y ~ a + x)

    w1 <- calculate_weights(m1, "a")
    w2 <- calculate_weights(m2, "a")

    expect_error(plot(w1, geoms), NA)

    expect_error(plot(w2, geoms), NA)
})
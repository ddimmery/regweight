test_that("map plotting works", {
    skip_if_not_installed("sf")
    try(attachNamespace("sf"))
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

    expect_error(plot_weighting_map(w1, geoms), NA)

    expect_error(plot_weighting_map(w2, geoms), NA)
})
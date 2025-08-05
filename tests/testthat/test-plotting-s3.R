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
    skip_on_cran()
    try(attachNamespace("sf"), silent = TRUE)
    y <- rnorm(100)
    a <- rbinom(100, 1, 0.5)
    x <- rnorm(100)
    g <- sample(c("NC", "SC", "GA", "TN"), 100, replace = TRUE)
    
    # Download and extract US state boundaries from Census Bureau
    temp_zip <- tempfile(fileext = ".zip")
    temp_dir <- tempdir()
    download.file("https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_us_state_20m.zip", 
                temp_zip, mode = "wb", quiet = TRUE)
    unzip(temp_zip, exdir = temp_dir)
    geoms <- sf::st_read(file.path(temp_dir, "cb_2018_us_state_20m.shp"), quiet = TRUE) %>% dplyr::rename(state_abbr = STUSPS)

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

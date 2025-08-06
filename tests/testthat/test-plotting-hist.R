test_that("histogram works", {
  y <- rnorm(100)
  a <- rbinom(100, 1, 0.5)
  x <- rnorm(100)

  m1 <- stats::lm(y ~ a + x)
  m2 <- estimatr::lm_robust(y ~ a + x)

  w1 <- calculate_weights(m1, "a")
  w2 <- calculate_weights(m2, "a")

  expect_error(hist(w1), NA)

  expect_error(hist(w2), NA)

  expect_error(hist(w1, bw = 0.025), NA)
})

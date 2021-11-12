test_that("weighting function errors", {
  y <- rnorm(10)
  x1 <- runif(10)
  x2 <- runif(10)
  m <- lm(y ~ x1 + x2)

  expect_error(
    calculate_weights("string", "a"),
    class = "regweight_model_argument"
  )

  expect_error(
    calculate_weights(m, "a"),
    class = "regweight_term_argument"
  )

  expect_error(
    calculate_weights(m, "x"),
    class = "regweight_term_argument"
  )

  expect_error(
    calculate_weights("string", "x"),
    class = "regweight_model_argument"
  )

  expect_error(
    calculate_weights(m, 3),
    class = "regweight_term_argument"
  )

  class(m) <- "lm_robust"
  expect_error(
    calculate_weights(m, 3),
    class = "regweight_term_argument"
  )
})

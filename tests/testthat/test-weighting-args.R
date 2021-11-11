test_that("weighting function errors", {
  m <- list()
  class(m) <- "lm"

  expect_error(
    calculate_weights("string", "a"),
    class = "regweights_model_argument"
  )

  expect_error(
    calculate_weights(m, 3),
    class = "regweights_term_argument"
  )

  class(m) <- "lm_robust"
  expect_error(
    calculate_weights(m, 3),
    class = "regweights_term_argument"
  )
})

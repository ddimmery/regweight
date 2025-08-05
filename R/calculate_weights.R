#' Calculate regression weights
#'
#' Given a model and a term of interest, calculate
#' the Aronow and Samii (2015) \doi{10.1111/ajps.12185} regression
#' weights and return an object which can be used to diagnose these
#' implicit weights.
#' @param mod The linear model object from `lm` or `lm_robust`.
#' @param term String indicating the term for which
#' to calculate the implicit regression weights. This must uniquely match
#' a coefficient name (i.e. it must be a string which appears in only one
#' element of `coef(mod)`).
#' @return An object of class `regweight` containing:\tabular{ll}{
#' \code{term} \tab Term for which weights were calculated. \cr
#' \tab \cr
#' \code{model} \tab The partial regression model object. \cr
#' \tab \cr
#' \code{weights} \tab The implicit regression weights. \cr
#' }
#' @details
#' This calculates the implicit regression weights for a particular term
#' in a given regression model.
#'
#' In short, this calculates the weights for a coefficient \eqn{\beta} such
#' that:
#'
#' \deqn{\frac{\mathrm{E}[w_i \beta_i]}{\mathrm{E}[w_i]} \to \beta}
#'
#' where \eqn{\beta_i} is the unit level effect. The expectation of \eqn{w_i}
#' is the conditional variance of the variable of interest.
#'
#' For details and examples, view the vignette:
#' \code{vignette("example-usage", package = "regweight")}
#' @references Aronow, P.M. and Samii, C. (2016), "Does Regression Produce
#' Representative Estimates of Causal Effects?". *American Journal of Political
#' Science*, 60: 250-267. \doi{10.1111/ajps.12185}
#' @examples
#' y <- rnorm(100)
#' a <- rbinom(100, 1, 0.5)
#' x <- rnorm(100)
#' m1 <- stats::lm(y ~ a + x)
#'
#' w1 <- calculate_weights(m1, "a")
#' @importFrom checkmate test_class test_character
#' @importFrom rlang abort
#' @importFrom stats setNames
#' @importFrom glue glue
#' @export
calculate_weights <- function(mod, term) {
    is_lm <- checkmate::test_class(mod, "lm")
    is_lmr <- checkmate::test_class(mod, "lm_robust")

    if (!is_lm && !is_lmr) {
        rlang::abort(c(
            "Must supply one of the following classes of model object:",
            x = "`stats::lm`",
            x = "`estimatr::lm_robust`"
        ),
        class = "regweight_model_argument"
        )
    }

    is_char <- checkmate::test_character(term, len = 1)
    if (!is_char) {
        rlang::abort(
            "`term` argument must be a character vector of length 1.",
            class = "regweight_term_argument"
        )
    }

    terms <- names(stats::coef(mod))
    term_of_interest <- grep(term, terms, value = TRUE)

    if (length(term_of_interest) < 1) {
        rlang::abort(
            c(
                "Provided `term` matches no terms in the model.",
                i = "Valid terms are:",
                paste(terms, collapse = ", ")
            ),
            class = "regweight_term_argument"
        )
    }

    if (length(term_of_interest) > 1) {
        rlang::abort(
            c(
                "Provided `term` matches multiple terms in the model.",
                i = glue::glue("Provided term, `{term}`, matches:"),
                term_of_interest
            ),
            class = "regweight_term_argument"
        )
    }

    new_fm <- stats::update(
        stats::formula(mod),
        glue::glue("{term} ~ . - {term}")
    )

    wt_lm <- stats::update(mod, new_fm)

    o <- list()

    o$term <- term
    mdf <- stats::model.frame(wt_lm)
    has_na <- "na.action" %in% names(attributes(mdf))
    if (has_na) {
        na_rows <- attr(mdf, "na.action")
        n <- nrow(mdf) + length(na_rows)
        o$weights <- rep(NA, n)
        o$weights[-na_rows] <- (mdf[[term]] - wt_lm$fitted.values) ^ 2
    } else {
        o$weights <- (mdf[[term]] - wt_lm$fitted.values) ^ 2
    }

    o$weights <- (
        o$weights / sum(o$weights, na.rm = TRUE) * sum(!is.na(o$weights))
    )
    o$model <- wt_lm

    names(o$weights) <- NULL

    class(o) <- "regweight"
    o
}

#' Calculate regression weights
#'
#' Given a model and a term of interest, calculate
#' the Aronow and Samii (2015) regression weights and return
#' an object which can be used to diagnose these implicit
#' weights.
#' @param mod The linear model object from `lm` or `lm_robust`.
#' @param term String indicating the term for which
#' to calculate the implicit regression weights. This must uniquely match
#' a coefficient name (i.e. it must be a string which appears in only one
#' element of `coef(mod)`).
#' @importFrom checkmate test_class test_character
#' @importFrom rlang abort
#' @importFrom stats setNames
#' @importFrom glue glue
#' @export
calculate_weights <- function(mod, term) {
    is_lm <- checkmate::test_class(mod, "lm")
    is_lmr <- checkmate::test_class(mod, "lm_robust")

    if (!is_lm & !is_lmr) {
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
    term_of_interest <- match.arg(term, terms)

    if (length(term_of_interest) < 1) {
        rlang::abort(
            c(
                "Provided `term` matches no terms in the model.",
                i = "Valid terms are:",
                " " = paste(terms, collapse = ", ")
            )
        )
    }

    new_fm <- stats::update(
        stats::formula(mod),
        glue::glue("{term} ~ . - {term}")
    )

    wt_lm <- stats::update(mod, new_fm)

    o <- list()

    o$term <- term
    mdf <- stats::model.frame(wt_lm, na.action = stats::na.pass)
    if (is_lm) {
        n <- nrow(mdf)
        o$weights <- rep(NA, n)
        if (is.null(stats::na.action(wt_lm))) {
            idx <- 1:n
        } else {
            idx <- (1:n)[-stats::na.action(wt_lm)]
        }
        o$weights[idx] <- stats::residuals(wt_lm) ^ 2
    } else {
        o$weights <- (
            mdf[[wt_lm$outcome]] -
            stats::predict(wt_lm)
        ) ^ 2
    }

    names(o$weights) <- NULL

    class(o) <- "regweight"
    o
}

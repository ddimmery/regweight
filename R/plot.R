#' Plot diagnostics for implicit regression weights
#'
#' This provid
#' @param x Weighting model object
#' @param covariate Covariate vector
#' @param ... additional arguments passed to plotting functions
#' @importFrom ggplot2 ggplot aes geom_pointrange geom_point
#' @importFrom dplyr tibble group_by summarize mutate n
#' @importFrom checkmate test_character
#' @importFrom rlang abort
#' @export
plot.regweight <- function(x, covariate, ...) {
    num_levels <- length(unique(covariate))
    lvl_pc <- num_levels / length(covariate)

    is_fct <- checkmate::test_factor(covariate)
    is_char <- checkmate::test_character(covariate)
    is_unq_numeric <- checkmate::test_numeric(covariate, unique = TRUE)
    is_numeric <- checkmate::test_numeric(covariate)
    is_sfc <- checkmate::test_class(covariate, "sfc")

    if (!is_sfc && (is_fct || is_char || num_levels < 20 || lvl_pc < 0.25)) {
        plot_weighting_discrete(x, covariate, ...)
    } else if (is_unq_numeric || (is_numeric && lvl_pc > 0.75)) {
        plot_weighting_continuous(x, covariate, ...)
    } else if (is_sfc) {
        plot_weighting_map(x, covariate, ...)
    } else {
        rlang::abort(
            c(
                "The type of `covariate` cannot be determined.",
                "Directly use individual plotting functions:",
                "i" = "`regweight::plot_weighting_discrete`",
                "i" = "`regweight::plot_weighting_continuous`",
                "i" = "`regweight::plot_weighting_map`"
            ),
            class = "regweight_plot_type"
        )
    }
}

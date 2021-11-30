#' Plot diagnostics for implicit regression weights
#'
#' This provides access to all plotting functions and tries
#' to smartly use the appropriate one based on the covariate
#' provided. If covariate type is inappropriately recognized
#' please use the appropriate plotting function directly.
#' @param x Weighting model object
#' @param covariate Covariate vector
#' @param ... additional arguments passed to individual plotting functions
#' @return A `ggplot2::ggplot` object.
#' @seealso [calculate_weights()], [plot_weighting_discrete()], [plot_weighting_continuous()], [plot_weighting_map()]
#' @importFrom ggplot2 ggplot aes geom_pointrange geom_point
#' @importFrom dplyr tibble group_by summarize mutate n
#' @importFrom checkmate test_character
#' @importFrom rlang abort
#' @export
plot.regweight <- function(x, covariate, ...) {
    num_levels <- length(unique(covariate))

    is_fct <- checkmate::test_factor(covariate)
    is_char <- checkmate::test_character(covariate)
    is_numeric <- checkmate::test_numeric(covariate)
    is_sfc <- checkmate::test_class(covariate, "sfc")

    if (is_sfc) {
        plot_weighting_map(x, covariate, ...)
    } else if (is_fct || is_char || (is_numeric && num_levels < 10)) {
        plot_weighting_discrete(x, covariate, ...)
    } else if (is_numeric) {
        plot_weighting_continuous(x, covariate, ...)
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

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
plot.regweights <- function(x, covariate, ...) {
    num_levels <- length(unique(covariate))
    lvl_pc <- num_levels / length(covariate)

    is_char <- checkmate::test_character(covariate)
    is_numeric <- checkmate::test_numeric(covariate)

    if (is_char || num_levels < 20 || lvl_pc < 0.25) {
        plot_weighting_discrete(x, covariate, ...)
    } else if (is_numeric) {
        plot_weighting_continuous(x, covariate, ...)
    } else {
        rlang::abort(
            c(
                "The type of `covariate` cannot be determined.",
                "Directly use individual plotting functions:",
                " " = "`regweight::plot_weighting_discrete`",
                " " = "`regweight::plot_weighting_continuous`"
            ),
            class = "regweight_plot_type"
        )
    }
}

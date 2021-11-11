#' Plot diagnostics for implicit regression weights
#'
#' This provid
#' @param mod Weighting model object
#' @param covariate Covariate vector
#' @param alpha Number between zero and one indicating the desired alpha level
#' for confidence intervals.
#' @param ... additional arguments passed to plotting functions
#' @importFrom ggplot2 ggplot aes geom_pointrange geom_point
#' @importFrom dplyr tibble group_by summarize mutate n
#' @importFrom checkmate test_character
#' @importFrom rlang abort
plot.regweights <- function(mod, covariate, ...) {
    num_levels <- length(unique(covariate))
    lvl_pc <- num_levels / length(covariate)

    is_char <- checkmate::test_character(character)

    if (is_char || num_levels < 20 || lvl_pc < 0.25) {
        plot_discrete(mod, covariate, ...)
    } else if (is_numeric) {
        plot_weighting_continuous(mod, covariate, ...)
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
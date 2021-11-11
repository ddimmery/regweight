#' Plot weights in a choropleth
#'
#' This provid
#' @param mod Weighting model object
#' @param covariate Covariate vector
#' @param map Map object
#' @param alpha Number between zero and one indicating the desired alpha level
#' for confidence intervals.
#' @param ... additional arguments
#' @importFrom ggplot2 ggplot aes geom_line scale_x_discrete scale_y_continuous
#' @importFrom ggplot2 scale_fill_manual scale_color_manual
#' @importFrom ggplot2 scale_alpha_continuous scale_linetype_discrete
#' @importFrom ggplot2 theme_minimal
#' @importFrom dplyr tibble group_by summarize mutate n %>%
#' @importFrom rlang .data
#' @export
plot_weighting_map <- function(mod, covariate, map, alpha = 0.05, ...) {
    rlang::abort(
        c("x" = "This function is not yet implemented."),
        class = "regweight_not_implemented"
    )
}

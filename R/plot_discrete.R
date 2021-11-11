#' Plot weights subdivided by a discrete covariate
#'
#' This provid
#' @param mod Weighting model object
#' @param covariate Covariate vector
#' @param alpha Number between zero and one indicating the desired alpha level
#' for confidence intervals.
#' @param ... additional arguments
#' @importFrom ggplot2 ggplot aes geom_line scale_x_discrete scale_y_continuous
#' @importFrom ggplot2 scale_fill_manual scale_color_manual
#' @importFrom ggplot2 scale_alpha_continuous scale_linetype_discrete
#' @importFrom ggplot2 theme_minimal
#' @importFrom dplyr tibble group_by summarize mutate n
plot_weighting_discrete <- function(mod, covariate, alpha = 0.05, ...) {
    checkmate::assert_class(mod, "regweight")

    tbl <- dplyr::tibble(
        weights = mod$weights / sum(mod$weights),
        covariate = covariate
    ) %>%
    dplyr::group_by(covariate) %>%
    dplyr::summarize(
        n = dplyr::n(),
        avg_weight = n * mean(weights),
        std_error = n * stats::sd(weights) / sqrt(n),
        lwr = avg_weight - stats::qnorm(1 - alpha / 2) * std_error,
        upr = avg_weight + stats::qnorm(1 - alpha / 2) * std_error
    ) %>%
    dplyr::mutate(
        nominal_weight = n / sum(n)
    )

    ggplot2::ggplot(tbl, ggplot2::aes(covariate)) +
    ggplot2::geom_pointrange(
        ggplot2::aes(
            y = avg_weight,
            ymin = lwr,
            ymax = upr,
            color = "Implicit regression"
        )
    ) +
    ggplot2::geom_point(
        ggplot2::aes(y = nominal_weight, color = "Nominal"),
        alpha = 0.5,
        size = 3
    ) +
    ggplot2::expand_limits(y = 0) +
    ggplot2::scale_x_discrete("") +
    ggplot2::scale_y_continuous("Probability mass (%)") +
    ggplot2::scale_color_manual("",
        values = c("Implicit regression" = "black", "Nominal" = "red")
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")
}
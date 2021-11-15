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
#' @importFrom scales percent
#' @importFrom dplyr tibble group_by summarize mutate n %>%
#' @importFrom rlang .data
#' @export
plot_weighting_discrete <- function(mod, covariate, alpha = 0.05, ...) {
    checkmate::assert_class(mod, "regweight")

    tbl <- dplyr::tibble(
        weights = mod$weights / sum(mod$weights),
        covariate = covariate
    ) %>%
    dplyr::group_by(.data$covariate) %>%
    dplyr::summarize(
        n = dplyr::n(),
        avg_weight = .data$n * mean(.data$weights),
        std_error = .data$n * stats::sd(.data$weights) / sqrt(.data$n),
        lwr = .data$avg_weight - stats::qnorm(1 - alpha / 2) * .data$std_error,
        upr = .data$avg_weight + stats::qnorm(1 - alpha / 2) * .data$std_error
    ) %>%
    dplyr::mutate(
        nominal_weight = .data$n / sum(.data$n)
    )

    ggplot2::ggplot(tbl, ggplot2::aes(covariate)) +
    ggplot2::geom_pointrange(
        ggplot2::aes(
            y = .data$avg_weight,
            ymin = .data$lwr,
            ymax = .data$upr,
            color = "Implicit regression"
        )
    ) +
    ggplot2::geom_point(
        ggplot2::aes(y = .data$nominal_weight, color = "Nominal"),
        alpha = 0.5,
        size = 3
    ) +
    ggplot2::expand_limits(y = 0) +
    ggplot2::scale_x_discrete("") +
    ggplot2::scale_y_continuous("Probability mass (%)", labels = scales::percent) +
    ggplot2::scale_color_manual("",
        values = c("Implicit regression" = "black", "Nominal" = "red")
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")
}
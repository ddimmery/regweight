#' Plot weights across a continuous covariate
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
#' @importFrom lpdensity lpdensity
#' @importFrom dplyr tibble
plot_weighting_continuous <- function(mod, covariate, alpha = 0.05, ...) {
    checkmate::assert_class(mod, "regweights")

    tbl <- dplyr::tibble(
        weights = mod$weights / sum(mod$weights),
        covariate = covariate
    )

    range <- stats::quantile(covariate, probs = c(0.05, 0.95))
    eval_pts <- seq(range[1], range[2], length = 250)

    wkde <- lpdensity::lpdensity(
        covariate,
        grid = eval_pts,
        Pweights = mod$weights / sum(mod$weights) * length(covariate),
        kernel = "epanechnikov",
        bwselect = "imse-dpi"
    )

    kde <- lpdensity::lpdensity(
        covariate,
        grid = eval_pts,
        kernel = "epanechnikov",
        bwselect = "imse-dpi"
    )

    tbl <- dplyr::tibble(
        weight = rep(c("Implicit regression", "Nominal"), c(250, 250)),
        transp = rep(c(1, 0.5), c(250, 250)),
        covariate = c(eval_pts, eval_pts),
        density = c(wkde$Estimate[, "f_p"], kde$Estimate[, "f_p"]),
        std_error = c(wkde$Estimate[, "se_q"], kde$Estimate[, "se_q"]),
        lwr = density - stats::qnorm(1 - alpha / 2) * std_error,
        upr = density + stats::qnorm(1 - alpha / 2) * std_error
    )

    ggplot2::ggplot(tbl,
        ggplot2::aes(
            x = covariate,
            alpha = transp,
            color = weight,
            fill = weight
        )
    ) +
    ggplot2::geom_line(aes(y = density)) +
    ggplot2::geom_line(aes(y = lwr), linetype = "dashed") +
    ggplot2::geom_line(aes(y = upr), linetype = "dashed") +
    ggplot2::scale_x_discrete("") +
    ggplot2::scale_y_continuous("Covariate density") +
    ggplot2::scale_fill_manual("",
        values = c("Implicit regression" = "black", "Nominal" = "red")
    ) +
    ggplot2::scale_color_manual("",
        values = c("Implicit regression" = "black", "Nominal" = "red")
    ) +
    ggplot2::scale_alpha_continuous(guide = "none", limits = c(0, 1)) +
    ggplot2::scale_linetype_discrete(guide = "none") +
    ggplot2::theme_minimal()
}
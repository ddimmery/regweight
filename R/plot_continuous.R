#' Plot weights across a continuous covariate
#'
#' This provides a simple plot for the distribution of a single
#' continuous covariate in the nominal sample and the implicit sample
#' defined by the Aronow and Samii (2015) \doi{10.1111/ajps.12185} regression
#' weights.
#' @param mod Weighting model object
#' @param covariate Covariate vector
#' @param alpha Number between zero and one indicating the desired alpha level
#' for confidence intervals.
#' @param num_eval Number of points at which to evaluate the density.
#' @param ... unused arguments
#' @details
#' Kernel density estimates use the bias-corrected methods of
#' Cattaneo et al (2020).
#' @return A `ggplot2::ggplot` object.
#' @examples
#' y <- rnorm(100)
#' a <- rbinom(100, 1, 0.5)
#' x <- rnorm(100)
#' cov <- runif(100)
#' mod <- stats::lm(y ~ a + x)
#' rw_mod <- calculate_weights(mod, "a")
#' plot_weighting_continuous(rw_mod, cov, num_eval = 25)
#' @seealso [lpdensity::lpdensity()]
#' @references \itemize{
#'  \item Cattaneo, Jansson and Ma (2021): lpdensity:
#' Local Polynomial Density Estimation and Inference.
#' *Journal of Statistical Software*, forthcoming.
#'  \item Cattaneo, Jansson and Ma (2020):
#' Simple Local Polynomial Density Estimators.
#' *Journal of the American Statistical Association* 115(531): 1449-1455.
#' }
#' @importFrom ggplot2 ggplot aes geom_line scale_x_discrete scale_y_continuous
#' @importFrom ggplot2 scale_fill_manual scale_color_manual
#' @importFrom ggplot2 scale_alpha_continuous scale_linetype_discrete
#' @importFrom ggplot2 theme_minimal .data
#' @importFrom checkmate assert_class assert_numeric
#' @importFrom lpdensity lpdensity
#' @importFrom dplyr tibble %>% mutate
#' @export
plot_weighting_continuous <- function(
  mod,
  covariate,
  alpha = 0.05,
  num_eval = 250,
  ...
) {
  checkmate::assert_class(mod, "regweight")
  checkmate::assert_numeric(covariate)

  ok <- stats::complete.cases(covariate, mod$weights)
  n <- sum(ok)
  covariate <- covariate[ok]
  wts <- mod$weights[ok]

  range <- stats::quantile(covariate, probs = c(0.05, 0.95))
  eval_pts <- seq(range[1], range[2], length = num_eval)

  wkde <- lpdensity::lpdensity(
    covariate,
    grid = eval_pts,
    Pweights = wts / sum(wts) * n,
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
    weight = rep(
      c("Implicit regression", "Nominal"),
      c(num_eval, num_eval)
    ),
    transp = rep(c(1, 0.5), c(num_eval, num_eval)),
    covariate = c(eval_pts, eval_pts),
    density = c(wkde$Estimate[, "f_p"], kde$Estimate[, "f_p"]),
    std_error = c(wkde$Estimate[, "se_q"], kde$Estimate[, "se_q"])
  )
  tbl <- tbl %>%
    dplyr::mutate(
      lwr = tbl[["density"]] -
        stats::qnorm(1 - alpha / 2) * tbl[["std_error"]],
      upr = tbl[["density"]] +
        stats::qnorm(1 - alpha / 2) * tbl[["std_error"]]
    )

  ggplot2::ggplot(tbl,
    ggplot2::aes(
      x = .data[["covariate"]],
      alpha = .data[["transp"]],
      color = .data[["weight"]],
      fill = .data[["weight"]]
    )
  ) +
    ggplot2::geom_line(ggplot2::aes(y = .data[["density"]])) +
    ggplot2::geom_line(ggplot2::aes(y = .data[["lwr"]]), linetype = "dashed") +
    ggplot2::geom_line(ggplot2::aes(y = .data[["upr"]]), linetype = "dashed") +
    ggplot2::scale_x_continuous("") +
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

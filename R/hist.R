#' Plot histogram of implicit regression weights
#'
#' This provides a simple histogram of the Aronow and Samii (2015)
#' \doi{10.1111/ajps.12185} implicit regression weights.
#'
#' @param x Weighting model object
#' @param bw Bandwidth for histogram bins. If not provided, the
#' Freedman-Diaconis rule will be used.
#' @param ... unused arguments
#' @return A `ggplot2::ggplot` object.
#' @importFrom ggplot2 ggplot aes geom_histogram theme_minimal
#' @importFrom ggplot2 scale_x_log10 expand_limits
#' @importFrom dplyr tibble group_by summarize mutate n
#' @importFrom rlang abort
#' @export
hist.regweight <- function(x, bw = NULL, ...) {
    if (is.null(bw)) {
        # Freedman-Diaconis rule
        bw <- 2 * stats::IQR(x$weights, na.rm = TRUE) / sum(!is.na(x$weights)) ^ (1 / 3)
    }
    ggplot2::ggplot(dplyr::tibble(w = x$weights), ggplot2::aes(w)) +
    ggplot2::geom_histogram(binwidth = bw) +
    ggplot2::scale_x_log10("Weight (log scale)") +
    ggplot2::scale_y_continuous("Count") +
    ggplot2::expand_limits(x = 1) +
    ggplot2::theme_minimal()
}

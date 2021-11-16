#' Plot weights in a choropleth
#'
#' This provid
#' @param mod Weighting model object
#' @param geometry A column of class `sf::sfc` with the geometry associated with
#' each observation.
#' @param ... unused arguments
#' @seealso [hist.regweight()], [plot.regweight()], [sf::sfc()]
#' @importFrom ggplot2 ggplot aes geom_line scale_x_discrete scale_y_continuous
#' @importFrom ggplot2 scale_fill_manual scale_color_manual
#' @importFrom ggplot2 scale_alpha_continuous scale_linetype_discrete
#' @importFrom ggplot2 theme_void geom_sf
#' @importFrom scales percent
#' @importFrom checkmate assert_class
#' @importFrom dplyr tibble %>%
#' @importFrom stats aggregate
#' @importFrom rlang .data check_installed
#' @export
plot_weighting_map <- function(mod, geometry, ...) {
    rlang::check_installed("sf")
    try(attachNamespace("sf"), silent = TRUE)
    checkmate::assert_class(mod, "regweight")
    checkmate::assert_class(geometry, "sfc")

    df <- dplyr::tibble(weights = mod$weights / sum(mod$weights, na.rm = TRUE), geometry = geometry) %>%
    sf::st_as_sf()

    agg_df <- stats::aggregate(df, by = df$geometry, sum)

    ggplot2::ggplot(agg_df) +
    ggplot2::geom_sf(ggplot2::aes(fill = .data$weights), color = "#bbbbbb", size = 0.05) +
    ggplot2::scale_fill_gradient(
        "Implicit regression weight",
        low = "#ffffff",
        high = "#000000",
        labels = scales::percent
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "bottom")
}

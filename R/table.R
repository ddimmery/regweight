#' Create summary statistics for implicit sample
#'
#' This provides a simple table of summary statistics for
#' the implicit sample defined by Aronow and Samii (2015)
#' \doi{10.1111/ajps.12185}.
#'
#' @param object Weighting model object
#' @param df dataframe with one column for each covariate to
#' include in the resulting table of summary statistics.
#' @param output Desired output type. Default is to return a tibble,
#' but can also select from "latex" and "html" to return a formatted
#' table for inclusion in a paper or report.
#' @param ... unused
#' @return One of three outputs depending on the requested type:
#' \itemize{
#'   \item \code{tibble}: Returns a `tibble` object (see [tibble::tibble()]).
#'   \item \code{latex}: Returns a `knit_asis` object
#' (see [knitr::asis_output()]).
#'   \item \code{html}: Returns an `html` object (see [htmltools::HTML()]).
#' }
#' @importFrom ggplot2 ggplot aes geom_histogram theme_minimal
#' @importFrom ggplot2 scale_x_log10 expand_limits
#' @importFrom dplyr %>% group_by summarize mutate n .data
#' @importFrom tidyr pivot_longer
#' @importFrom rlang abort
#' @importFrom tidyselect vars_select_helpers starts_with all_of
#' @export
summary.regweight <- function(object, df, output = "tibble", ...) {
  df$.weight <- object$weights
  few_unique <- function(.x) length(unique(.x)) < 10
  where_short <- tidyselect::vars_select_helpers$where
  df_discrete <- df %>%
    dplyr::select(
      .data[[".weight"]],
      where_short(checkmate::test_factor),
      where_short(checkmate::test_character),
      where_short(few_unique)
    )
  if (ncol(df_discrete) == 1) {
    result_discrete <- NULL
  } else {
    df_discrete <- df_discrete %>%
      tidyr::pivot_longer(
        cols = !tidyselect::all_of(".weight"),
        names_to = "covariate",
        values_transform = list(value = as.character)
      )

    result_discrete <- df_discrete %>%
      dplyr::group_by(.data[["covariate"]]) %>%
      dplyr::reframe(summary_of_discrete(dplyr::pick(dplyr::everything()))) %>%
      dplyr::ungroup()
  }

  df_cts <- df %>%
    dplyr::select(
      .data[[".weight"]],
      !(
        where_short(checkmate::test_factor) |
          where_short(checkmate::test_character) |
          where_short(few_unique)
      ) & where_short(checkmate::test_numeric)
    )

  if (ncol(df_cts) == 1) {
    result_cts <- NULL
  } else {
    df_cts <- df_cts %>%
      tidyr::pivot_longer(
        cols = dplyr::everything() & !tidyselect::all_of(".weight"),
        names_to = "covariate",
        values_transform = list(values = as.numeric)
      )

    result_cts <- df_cts %>%
      dplyr::group_by(.data[["covariate"]]) %>%
      dplyr::reframe(
        summary_of_continuous(dplyr::pick(dplyr::everything()))
      ) %>%
      dplyr::ungroup()
  }

  df_unknown <- df %>%
    dplyr::select(
      !(
        where_short(checkmate::test_factor) |
          where_short(checkmate::test_character) |
          where_short(few_unique)
      ) & !where_short(checkmate::test_numeric)
    )

  if (ncol(df_unknown) > 0) {
    cov_names <- names(df_unknown)
    rlang::abort(
      c(
        "The type of some columns cannot be determined.",
        "To make recognition easy, typecast:",
        "i" = "categorical variables to factors and",
        "i" = "continuous variables to double.",
        paste("Covariates: ", paste(cov_names, collapse = ", "))
      ),
      class = "regweight_table_type"
    )
  }

  format_result(result_discrete, result_cts, type = output)
}

#' @noRd
#' @importFrom gt gt
format_result <- function(tbl_discrete, tbl_cts, type) {

  if (type == "tibble") {
    return(dplyr::bind_rows(tbl_discrete, tbl_cts))
  } else if (type == "latex") {
    export_tbl <- gt::as_latex
  } else if (type == "html") {
    export_tbl <- gt::as_raw_html
  } else {
    rlang::abort(
      c(
        "The requested output type is unknown. Please choose one of:",
        "i" = "`'html'` for HTML output",
        "i" = "`'latex'` for LaTeX output",
        "i" = "`tibble'` for the raw output as a tibble"
      ),
      class = "regweight_table_output_type"
    )
  }

  tbl <- dplyr::bind_rows(
    tbl_discrete,
    tbl_cts
  ) %>%
    tidyr::pivot_wider(
      id_cols = tidyselect::all_of(c("covariate", "value")),
      names_from = tidyselect::all_of("sample"),
      values_from = tidyselect::all_of(c("mean", "std_dev"))
    )

  tbl <- tbl %>%
    mutate(
      covariate = dplyr::if_else(
        duplicated(.data[["covariate"]]),
        "",
        .data[["covariate"]]
      )
    ) %>%
    gt::gt(rowname_col = "covariate") %>%
    gt::tab_stubhead(label = "Covariate") %>%
    gt::tab_row_group(
      label = "Discrete variables",
      rows = which(is.na(tbl[["std_dev_Nominal"]]))
    ) %>%
    gt::tab_row_group(
      label = "Continuous variables",
      rows = which(is.na(tbl[["value"]]))
    ) %>%
    gt::row_group_order(
      groups = c("Discrete variables", "Continuous variables")
    ) %>%
    gt::tab_spanner(
      label = "Nominal",
      columns = tidyselect::all_of(c("mean_Nominal", "std_dev_Nominal"))
    ) %>%
    gt::tab_spanner(
      label = "Implicit",
      columns = tidyselect::all_of(c("mean_Implicit", "std_dev_Implicit"))
    ) %>%
    gt::fmt_number(
      columns = c(
        tidyselect::starts_with("mean"), tidyselect::starts_with("std")
      ),
      n_sigfig = 3
    ) %>%
    gt::sub_missing(
      columns = dplyr::everything(),
      missing_text = ""
    ) %>%
    gt::cols_label(
      value = "Value",
      mean_Nominal = "Mean",
      mean_Implicit = "Mean",
      std_dev_Nominal = "Std. Dev.",
      std_dev_Implicit = "Std. Dev."
    )
  export_tbl(tbl)

}

#' @noRd
summary_of_discrete <- function(df) {
  df %>%
    dplyr::group_by(.data[["value"]]) %>%
    dplyr::summarize(
      n = n(),
      sum_weight = sum(.data[[".weight"]], na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      unweighted = .data[["n"]] / sum(.data[["n"]]),
      weighted = .data[["sum_weight"]] /
        sum(.data[["sum_weight"]], na.rm = TRUE)
    ) %>%
    dplyr::group_by(.data[["value"]]) %>%
    dplyr::reframe(
      sample = c("Nominal", "Implicit"),
      mean = c(.data[["unweighted"]], .data[["weighted"]]),
      std_dev = c(NA, NA)
    ) %>%
    dplyr::select(
      tidyselect::all_of(c("value", "sample", "mean", "std_dev"))
    )
}

#' @noRd
#' @importFrom stats weighted.mean sd
summary_of_continuous <- function(df) {
  df %>%
    dplyr::ungroup() %>%
    dplyr::reframe(
      sample = c("Nominal", "Implicit"),
      mean = c(
        mean(.data[["value"]], na.rm = TRUE),
        stats::weighted.mean(
          .data[["value"]][!is.na(.data[[".weight"]])],
          .data[[".weight"]][!is.na(.data[[".weight"]])],
          na.rm = TRUE
        )
      ),
      std_dev = c(
        stats::sd(.data[["value"]], na.rm = TRUE),
        sqrt(stats::weighted.mean(
          (
            .data[["value"]][!is.na(.data[[".weight"]])] -
              stats::weighted.mean(
                .data[["value"]][!is.na(.data[[".weight"]])],
                .data[[".weight"]][!is.na(.data[[".weight"]])],
                na.rm = TRUE
              )
          ) ^ 2,
          .data[[".weight"]][!is.na(.data[[".weight"]])],
          na.rm = TRUE
        ))
      ),
      value = c(NA, NA),
    ) %>%
    dplyr::select(
      tidyselect::all_of(c("value", "sample", "mean", "std_dev"))
    )
}

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
#' @importFrom dplyr %>% group_by summarize mutate n
#' @importFrom tidyr pivot_longer
#' @importFrom rlang abort
#' @importFrom tidyselect vars_select_helpers
#' @export
summary.regweight <- function(object, df, output = "tibble", ...) {
    df$.weight <- object$weights
    few_unique <- function(.x) length(unique(.x)) < 10
    where_short <- tidyselect::vars_select_helpers$where
    df %>%
        dplyr::select(
            .weight,
            where_short(checkmate::test_factor),
            where_short(checkmate::test_character),
            where_short(few_unique)
        ) -> df_discrete
    if (ncol(df_discrete) == 1) {
        result_discrete <- NULL
    } else {
        df_discrete %>%
        tidyr::pivot_longer(
            cols = !.weight,
            names_to = "covariate",
            values_transform = list(value = as.character)
        ) -> df_discrete

        result_discrete <- df_discrete %>%
            dplyr::group_by(covariate) %>%
            dplyr::summarize(summary_of_discrete(dplyr::cur_data())) %>%
            dplyr::ungroup()
    }

    df %>%
        dplyr::select(
            .weight,
            !(
                where_short(checkmate::test_factor) |
                where_short(checkmate::test_character) |
                where_short(few_unique)
            ) & where_short(checkmate::test_numeric)
        ) -> df_cts

    if (ncol(df_cts) == 1) {
        result_cts <- NULL
    } else {
        df_cts %>%
            tidyr::pivot_longer(
                cols = dplyr::everything() & !dplyr::all_of(".weight"),
                names_to = "covariate",
                values_transform = list(values = as.numeric)
            ) -> df_cts

        result_cts <- df_cts %>%
            dplyr::group_by(covariate) %>%
            dplyr::summarize(summary_of_continuous(dplyr::cur_data())) %>%
            dplyr::ungroup()
    }

    df %>%
        dplyr::select(
            !(
                where_short(checkmate::test_factor) |
                where_short(checkmate::test_character) |
                where_short(few_unique)
            ) & !where_short(checkmate::test_numeric)
        ) -> df_unknown

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
        id_cols = c(covariate, value),
        names_from = sample,
        values_from = c(mean, std_dev)
    )

    tbl %>%
    mutate(
        covariate = dplyr::if_else(
            duplicated(covariate),
            "",
            covariate
        )
    ) %>%
    gt::gt(rowname_col = "covariate") %>%
    gt::tab_stubhead(label = "Covariate") %>%
    gt::tab_row_group(
        label = "Discrete variables",
        rows = which(is.na(tbl$std_dev_Nominal))
    ) %>%
    gt::tab_row_group(
        label = "Continuous variables",
        rows = which(is.na(tbl$value))
    ) %>%
    gt::row_group_order(
      groups = c("Discrete variables", "Continuous variables")
    ) %>%
    gt::tab_spanner(
        label = "Nominal",
        columns = c(mean_Nominal, std_dev_Nominal)
    ) %>%
    gt::tab_spanner(
        label = "Implicit",
        columns = c(mean_Implicit, std_dev_Implicit)
    ) %>%
    gt::fmt_number(
        columns = c(starts_with("mean"), starts_with("std")),
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
    ) -> tbl
    export_tbl(tbl)

}

#' @noRd
summary_of_discrete <- function(df) {
    df %>%
        dplyr::group_by(value) %>%
        dplyr::summarize(
            n = n(),
            sum_weight = sum(.weight, na.rm = TRUE)
        ) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
            unweighted = n / sum(n),
            weighted = sum_weight / sum(sum_weight, na.rm = TRUE)
        ) %>%
        dplyr::group_by(value) %>%
        dplyr::summarize(
            sample = c("Nominal", "Implicit"),
            mean = c(unweighted, weighted),
            std_dev = c(NA, NA)
        ) %>%
        dplyr::select(
            value, sample, mean, std_dev
        )
}

#' @noRd
#' @importFrom stats weighted.mean sd
summary_of_continuous <- function(df) {
    df %>%
        dplyr::ungroup() %>%
        dplyr::summarize(
            sample = c("Nominal", "Implicit"),
            mean = c(
                mean(value, na.rm = TRUE),
                stats::weighted.mean(
                    value[!is.na(.weight)],
                    .weight[!is.na(.weight)],
                    na.rm = TRUE
                )
            ),
            std_dev = c(
                stats::sd(value, na.rm = TRUE),
                sqrt(stats::weighted.mean(
                    (
                        value[!is.na(.weight)] -
                        stats::weighted.mean(
                            value[!is.na(.weight)],
                            .weight[!is.na(.weight)],
                            na.rm = TRUE
                        )
                    ) ^ 2,
                    .weight[!is.na(.weight)],
                    na.rm = TRUE
                ))
            ),
            value = c(NA, NA),
        ) %>%
        dplyr::select(
            value, sample, mean, std_dev
        )
}

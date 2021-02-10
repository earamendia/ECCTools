# This is meant to remain in the ECCTools package


# This function converts a tidy data frame into the same tidy data frame but with net trade

#' Title
#'
#' @param .tidy_iea_df
#' @param imports
#' @param exports
#' @param country
#' @param e_dot
#' @param flow
#' @param year
#' @param ledger_side
#' @param flow_aggregation_point
#' @param product
#' @param .net_imports
#'
#' @return
#' @export
#'
#' @examples
convert_to_net_trade <- function(.tidy_iea_df,
                                 imports = IEATools::interface_industries$imports,
                                 exports = IEATools::interface_industries$exports,
                                 country = IEATools::iea_cols$country,
                                 e_dot = IEATools::iea_cols$e_dot,
                                 flow = IEATools::iea_cols$flow,
                                 year = IEATools::iea_cols$year,
                                 ledger_side = IEATools::iea_cols$ledger_side,
                                 flow_aggregation_point = IEATools::iea_cols$flow_aggregation_point,
                                 product = IEATools::iea_cols$product,
                                 .net_imports = "Net_Imports"){

  net_trade_flows <- .tidy_iea_df %>%
    dplyr::filter(stringr::str_detect(.data[[flow]], imports) | stringr::str_detect(.data[[flow]], exports)) %>%
    dplyr::mutate(
      "{flow}" := dplyr::case_when(
        stringr::str_detect(.data[[flow]], imports) ~ imports,
        stringr::str_detect(.data[[flow]], exports) ~ exports,
        TRUE ~ .data[[flow]]
      )
    ) %>%
    tidyr::pivot_wider(names_from = .data[[flow]], values_from = .data[[e_dot]]) %>%
    dplyr::mutate(
      "{imports}" := tidyr::replace_na(.data[[imports]], 0),
      "{exports}" := tidyr::replace_na(.data[[exports]], 0),
      "{.net_imports}" := .data[[imports]] + .data[[exports]]
    ) %>%
    tidyr::pivot_longer(cols = c({imports}, {exports}, {.net_imports}), names_to = flow, values_to = e_dot) %>%
    dplyr::filter(.data[[flow]] == {.net_imports}) %>%
    dplyr::mutate(
      "{flow}" := dplyr::case_when(
        .data[[e_dot]] >= 0 ~ {imports},
        .data[[e_dot]] < 0 ~ {exports}
      )
    ) %>%
    dplyr::filter(.data[[e_dot]] != 0) %>%
    dplyr::mutate(
      "{flow}" := stringr::str_c(.data[[flow]], " [of ", .data[[product]], "]", sep = "")
    ) %>%
    dplyr::arrange({year}, {country}, dplyr::desc({ledger_side}), {flow_aggregation_point}, {flow})


  tidy_net_trade_df <- .tidy_iea_df %>%
    dplyr::filter(! (stringr::str_detect(.data[[flow]], imports) | stringr::str_detect(.data[[flow]], exports))) %>%
    dplyr::bind_rows(net_trade_flows) %>%
    dplyr::arrange({year}, {country}, dplyr::desc({ledger_side}), {flow_aggregation_point}, {flow})


  return(tidy_net_trade_df)
}




# This function removes Statistical differences from the tidy IEA data frame

#' Title
#'
#' @param .tidy_iea_df
#' @param flow
#' @param matnames
#' @param e_dot
#' @param stat_diffs
#' @param epsilon
#'
#' @return
#' @export
#'
#' @examples
stat_diffs_to_epsilon <- function(.tidy_iea_df,
                              flow = IEATools::iea_cols$flow,
                              matnames = "matnames",
                              e_dot = IEATools::iea_cols$e_dot,
                              stat_diffs = "Statistical differences",
                              epsilon = "Epsilon"){
  .tidy_iea_df %>%
    dplyr::mutate(
      "{matnames}" := dplyr::case_when(
        .data[[flow]] == stat_diffs ~ epsilon,
        TRUE ~ .data[[matnames]]
      ),
      "{e_dot}" := dplyr::case_when(
        .data[[flow]] == stat_diffs ~ -.data[[e_dot]],
        TRUE ~ .data[[e_dot]]
      )
    )
}



# This function removes Stock changes from the tidy IEA data frame

#' Title
#'
#' @param .tidy_iea_df
#' @param flow
#' @param matnames
#' @param e_dot
#' @param stock_changes
#' @param epsilon
#'
#' @return
#' @export
#'
#' @examples
stock_changes_to_epsilon <- function(.tidy_iea_df,
                                 flow = IEATools::iea_cols$flow,
                                 matnames = "matnames",
                                 e_dot = IEATools::iea_cols$e_dot,
                                 stock_changes = "Stock changes",
                                 epsilon = "Epsilon"){
  .tidy_iea_df %>%
    dplyr::mutate(
      "{matnames}" := dplyr::case_when(
        stringr::str_detect(.data[[flow]], stock_changes) ~ epsilon,
        TRUE ~ .data[[matnames]]
      ),
      "{e_dot}" := dplyr::case_when(
        stringr::str_detect(.data[[flow]], stock_changes) ~ -.data[[e_dot]],
        TRUE ~ .data[[e_dot]]
      )
    )
}


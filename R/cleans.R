
# This function converts a tidy data frame into the same tidy data frame but with net trade

convert_to_net_trade <- function(.tidy_iea_df,
                                 imports = IEATools::interface_industries$imports,
                                 exports = IEATools::interface_industries$exports,
                                 country = IEATools::iea_cols$country,
                                 e_dot = IEATools::iea_cols$e_dot,
                                 flow = IEATools::iea_cols$flow,
                                 year = IEATools::iea_cols$year,
                                 ledger_side = IEATools::iea_cols$ledger_side,
                                 flow_aggregation_point = IEATools::iea_cols$flow_aggregation_point,
                                 .net_imports = "Net_Imports"){

  net_trade_flows <- .tidy_iea_df %>%
    dplyr::filter(.data[[flow]] == imports | .data[[flow]] == exports) %>%
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
    dplyr::arrange({year}, {country}, dplyr::desc({ledger_side}), {flow_aggregation_point}, {flow})


  tidy_net_trade_df <- .tidy_iea_df %>%
    dplyr::filter(! .data[[flow]] %in% c({imports}, {exports})) %>%
    dplyr::bind_rows(net_trade_flows) %>%
    dplyr::arrange({year}, {country}, dplyr::desc({ledger_side}), {flow_aggregation_point}, {flow})


  return(tidy_net_trade_df)
}
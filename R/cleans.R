
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




# This function removes Statistical differences from the tidy IEA data frame

remove_stat_diffs <- function(.tidy_iea_df,
                              flow = IEATools::iea_cols$flow,
                              ledger_side = IEATools::iea_cols$ledger_side,
                              stat_diffs = "Statistical differences",
                              balancing = "Balancing"){
  .tidy_iea_df %>%
    dplyr::mutate(
      "{ledger_side}" := dplyr::case_when(
        .data[[flow]] == stat_diffs ~ balancing,
        TRUE ~ .data[[ledger_side]]
      )
    )
}



# This function removes Stock changes from the tidy IEA data frame

remove_stock_changes <- function(.tidy_iea_df,
                                 flow = IEATools::iea_cols$flow,
                                 ledger_side = IEATools::iea_cols$ledger_side,
                                 stock_changes = "Stock changes",
                                 balancing = "Balancing"){
  .tidy_iea_df %>%
    dplyr::mutate(
      "{ledger_side}" := dplyr::case_when(
        stringr::str_detect(.data[[flow]], stock_changes) ~ balancing,
        TRUE ~ .data[[ledger_side]]
      )
    )
}



# This function gathers producers and autoproducers

gather_producer_autoproducer <- function(.tidy_iea_df,
                                         flow_aggregation_point = "Flow.aggregation.point",
                                         eiou = "Energy industry own use",
                                         transformation_processes = "Transformation processes",
                                         flow = "Flow",
                                         # Industries that receive EIOU but are not in Transformation processes
                                         own_use_elect_chp_heat = "Own use in electricity, CHP and heat plants",
                                         pumped_storage = "Pumped storage plants",
                                         nuclear_industry = "Nuclear industry",
                                         e_dot = "E.dot",
                                         negzeropos = ".negzeropos",
                                         # Places where the EIOU will e reassigned
                                         main_act_producer_elect = "Main activity producer electricity plants"){

  .tidy_iea_df %>%
    dplyr::mutate(
      "{flow}" := dplyr::case_when(
        .data[[flow]] == ,
        TRUE ~ .data[[flow]]
      )
    ) %>%
    dplyr::mutate(
      "{negzeropos}" := dplyr::case_when(
        .data[[e_dot]] < 0 ~ "neg",
        .data[[e_dot]] == 0 ~ "zero",
        .data[[e_dot]] > 0 ~ "pos"
      )
    ) %>%
    # Now sum similar rows using summarise.
    # Group by everything except the energy flow rate column, "E.dot".
    matsindf::group_by_everything_except(e_dot) %>%
    dplyr::summarise(
      "{e_dot}" := sum(.data[[e_dot]])
    ) %>%
    dplyr::mutate(
      # Eliminate the column we added.
      "{negzeropos}" := NULL
    ) %>%
    dplyr::ungroup()
}



re_route_pumped_storage <- function(){

}



re_route_own_use_elect_chp_heat <- function(){

}



add_nuclear_industry <- function(){

}



re_route_non_specified_flows <- function(){

}

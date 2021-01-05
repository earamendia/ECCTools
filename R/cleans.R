
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
                                         flow_aggregation_point = IEATools::iea_cols$flow_aggregation_point,
                                         transformation_processes = "Transformation processes",
                                         flow = IEATools::iea_cols$flow,
                                         e_dot = IEATools::iea_cols$e_dot,
                                         negzeropos = ".negzeropos",
                                         # Autoproducer industries
                                         autoproducer_elect = "Autoproducer electricity plants",
                                         autoproducer_chp = "Autoproducer CHP plants",
                                         autoproducer_heat = "Autoproducer heat plants",
                                         # Main activity industries - to which autoproducer industries are re-routed
                                         main_act_producer_elect = "Main activity producer electricity plants",
                                         main_act_producer_heat = "Main activity producer heat plants",
                                         main_act_producer_chp = "Main activity producer CHP plants"){

  .tidy_iea_df %>%
    dplyr::mutate(
      "{flow}" := dplyr::case_when(
        (.data[[flow]] == autoproducer_elect & .data[[flow_aggregation_point]] == transformation_processes) ~ main_act_producer_elect,
        (.data[[flow]] == autoproducer_chp & .data[[flow_aggregation_point]] == transformation_processes) ~ main_act_producer_chp,
        (.data[[flow]] == autoproducer_heat & .data[[flow_aggregation_point]] == transformation_processes) ~ main_act_producer_heat,
        TRUE ~ .data[[flow]]
      )
    ) %>%
    # Aggregating. We need to add a pos/neg/null column to add up differently positive and negative values, otherwise we'd only get NET flows.
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



route_pumped_storage <- function(.tidy_iea_df,
                                    flow_aggregation_point = IEATools::iea_cols$flow_aggregation_point,
                                    eiou = "Energy industry own use",
                                    flow = IEATools::iea_cols$flow,
                                    # Industries that receive EIOU but are not in Transformation processes
                                    pumped_storage = "Pumped storage plants",
                                    e_dot = IEATools::iea_cols$e_dot,
                                    negzeropos = ".negzeropos",
                                    # Places where the EIOU will e reassigned
                                    main_act_producer_elect = "Main activity producer electricity plants"){

  .tidy_iea_df %>%
    dplyr::mutate(
      "{flow}" := dplyr::case_when(
        (.data[[flow]] == pumped_storage & .data[[flow_aggregation_point]] == eiou) ~ main_act_producer_elect,
        TRUE ~ .data[[flow]]
      )
    ) %>%
    # Aggregating. We need to add a pos/neg/null column to add up differently positive and negative values, otherwise we'd only get NET flows.
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


# This function re-routes the "Own use in electricity, CHP and heat plants" EIOU flow to the different main activity activities
# Using the shares of each main activity supply.

route_own_use_elect_chp_heat <- function(.tidy_iea_df,
                                         flow_aggregation_point = IEATools::iea_cols$flow_aggregation_point,
                                         ledger_side = IEATools::iea_cols$ledger_side,
                                         method = IEATools::iea_cols$method,
                                         ledger_side = IEATools::iea_cols$ledger_side,
                                         energy_tyoe = IEATools::iea_cols$energy_type,
                                         last_stage = IEATools::iea_cols$last_stage,
                                         year = IEATools::iea_cols$year,
                                         unit = IEATools::iea_cols$unit,
                                         e_dot = IEATools::iea_cols$e_dot,
                                         supply = "Supply",
                                         flow = IEATools::iea_cols$flow,
                                         eiou = "Energy industry own use",
                                         transformation_processes = "Transformation processes",
                                         own_use_elect_chp_heat = "Own use in electricity, CHP and heat plants",
                                         negzeropos = ".negzeropos",
                                         main_act_producer_elect = "Main activity producer electricity plants",
                                         main_act_producer_chp = "Main activity producer CHP plants",
                                         main_act_producer_heat = "Main activity producer heat plants"){


  # Check whether one of the three main activity elect, heat, and/or chp exist in the TP - supply.
  # If not, then route to main activity elect as the code originally did.

  n <- .tidy_iea_df %>%
    dplyr::filter((
      .data[[flow]] %in% c(main_act_producer_elect, main_act_producer_chp, main_act_producer_heat))
                  & .data[[flow_aggregation_point]] == transformation_processes
                  & .data[[ledger_side]] == supply
                  & .data[[e_dot]] > 0
      ) %>%
    dplyr::summarise(
      n = n()
    ) %>%
    dplyr::pull()


  if (n == 0){
    dplyr::mutate(
      "{flow}" := dplyr::case_when(
        .data[[flow]] == own_use_elect_chp_heat & .data[[flow_aggregation_point]] == eiou ~ main_act_producer_elect
      )
    )
  }

  total_main_activity_output <- tidy_iea_df %>%
    dplyr::filter(
      .data[[flow]] %in% c(main_act_producer_elect, main_act_producer_chp, main_act_producer_heat)
      & .data[[flow_aggregation_point]] == transformation_processes
      & .data[[ledger_side]] == supply
      & .data[[e_dot]] > 0
    ) %>%
    dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[unit]]) %>%
    dplyr::summarise(
      Total_supply_main_activity_From_Func = sum(.data[[e_dot]])
    )


  output_per_main_activity <- .tidy_iea_df %>%
    dplyr::filter(
      .data[[flow]] %in% c(main_act_producer_elect, main_act_producer_chp, main_act_producer_heat)
                  & .data[[flow_aggregation_point]] == transformation_processes
                  & .data[[ledger_side]] == supply
                  & .data[[e_dot]] > 0
      ) %>%
    dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[flow]], .data[[unit]]) %>% # adding flow
    dplyr::summarise(
      Supply_per_main_activity_From_Func = sum(.data[[e_dot]])
    )


  share_output_per_main_activity <- output_per_main_activity %>%
    dplyr::left_join(
      total_main_activity_output, by = c({country}, {method}, {energy_type}, {last_stage}, {year}, {flow}, {unit}, {ledger_side}, {flow_aggregation_point})
      ) %>%
    dplyr::mutate(
      Share_supply_per_main_activity_From_Func = Supply_per_main_activity_From_Func / Total_supply_main_activity_From_Func
    )

  routed_own_use <- .tidy_iea_df %>%
    dplyr::filter(.data[[flow]] == own_use_elect_chp_heat) %>%
    group_by(
      .data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[unit]], .data[[flow_aggregation_point]], .data[[ledger_side]]
      ) %>%
    tidyr::crossing(
      destination_flow = c(main_act_producer_elect, main_act_producer_chp, main_act_producer_heat)
      ) %>%
    dplyr::right_join(
      share_output_per_main_activity, by = c({country}, {method}, {energy_type}, {last_stage}, {year}, {flow}, {unit}, {ledger_side}, {flow_aggregation_point})
      ) %>%
    dplyr::mutate(
      "{flow}" := .data[["destination_flow"]],
      "{e_dot}" := .data[[e_dot]] * Share_supply_per_main_activity_From_Func
    ) %>%
    dplyr::select(-destination_flow, -Share_supply_per_main_activity_From_Func)

  tidy_iea_df_routed_own_use <- .tidy_iea_df %>%
    dplyr::filter(.data[[flow]] != own_use_elect_chp_heat) %>%
    dplyr::bind_rows(routed_own_use)

  return(tidy_iea_df_routed_own_use)
}



add_nuclear_industry <- function(.tidy_iea_df,
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

}



re_route_non_specified_flows <- function(.tidy_iea_df,
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

}

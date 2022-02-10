
#' Converts data to net trade data
#'
#' This function enables to convert a `.tidy_iea_df` to
#' a new `.tidy_iea_df` in which trade flows are reported as net flows.
#' So, if a given region or country both imports and exports a given product,
#' the function will report only the difference between imports and exports,
#' depending on which flow is dominant. See Details for uses.
#'
#' The function may be use after aggregating regions with the
#' `IEATools::aggregate_regions()` function, or may be applied to IEA data for
#' aggregated regions (such as World) for which both imports and exports are reported
#' for a given product.
#'
#' @param .tidy_iea_df The `.tidy_iea_df` which trade flows need to be converted to net trade flows.
#' @param imports The name of the Imports flows in the `.tidy_iea_df`.
#'                Default is `IEATools::interface_industries$imports`.
#' @param exports The name of the Exports flows in the `.tidy_iea_df`.
#'                Default is `IEATools::interface_industries$exports`.
#' @param country,e_dot,flow,year,ledger_side,flow_aggregation_point,product See `IEATools::iea_cols`.
#' @param .net_imports A temporary column that calculates net imports as the difference between imports and exports.
#'                     Default is "Net_Imports".
#'
#' @return A `.tidy_iea_df` for which trade flows are converted to net trade.
#' @importFrom rlang :=
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' # In this example, we gather all flows for countries A and B
#' # in a new "AB" region, for which both imports and exports are reported for some products.
#' tidy_AB_data %>%
#' dplyr::mutate(Country = "AB") %>%
#' dplyr::filter(stringr::str_detect(Flow, "Imports") | stringr::str_detect(Flow, "Exports")) %>%
#' print()
#' # After running the function, only either imports or exports are reported for each product.
#' tidy_AB_data %>%
#' dplyr::mutate(Country = "AB") %>%
#' convert_to_net_trade() %>%
#' dplyr::filter(stringr::str_detect(Flow, "Imports") | stringr::str_detect(Flow, "Exports")) %>%
#' print()
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
    # this line below is added:
    dplyr::filter(! (stringr::str_detect(.data[[flow]], IEATools::tpes_flows$exports_to_world_marine_bunkers) |
                       stringr::str_detect(.data[[flow]], IEATools::tpes_flows$exports_to_world_aviation_bunkers))) %>%
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
    #dplyr::filter(! (stringr::str_detect(.data[[flow]], imports) | stringr::str_detect(.data[[flow]], exports))) %>%
    dplyr::filter(! (stringr::str_detect(.data[[flow]], imports) | stringr::str_detect(.data[[flow]], exports)) |
                    stringr::str_detect(.data[[flow]], IEATools::tpes_flows$exports_to_world_aviation_bunkers) |
                    stringr::str_detect(.data[[flow]], IEATools::tpes_flows$exports_to_world_marine_bunkers)) %>%
    dplyr::bind_rows(net_trade_flows) %>%
    dplyr::arrange({year}, {country}, dplyr::desc({ledger_side}), {flow_aggregation_point}, {flow})


  return(tidy_net_trade_df)
}





#' Moving statistical differences flows to Balancing matrix
#'
#' This function sends statistical differences flows to a balancing matrix Balancing.
#' The Balancing matrix is akin to an additional final demand matrix,
#' meaning that flows akin to final demand will be positive,
#' while flows akin to supply will be negative.
#'
#' See the Balancing matrix vignette for more information.
#' Note: one needs to add the column containing matrices names first,
#' most likely using the `IEATools::add_psut_matnames()` function.
#'
#' @param .tidy_iea_df The `.tidy_iea_df` for which statistical differences flows need to be sent to the Balancing matrix.
#' @param flow,e_dot See `IEATools::iea_cols`.
#' @param matnames The column name of the column having matrices names.
#'                 Default is `IEATools::mat_meta_cols$matnames`.
#' @param stat_diffs The name of the statistical differences flows.
#'                   Default is "Statistical differences".
#' @param balancing_matrix The name of the Balancing matrix.
#'                Default is "B".
#'
#' @return A `.tidy_iea_df` for which statistical differences flows have been send to the Balancing matrix.
#' @export
#'
#' @examples
#' tidy_AB_data %>%
#' tibble::add_row(
#' Country = "A",
#' Method = "PCM",
#' Energy.type = "Energy",
#' Last.stage = "Final",
#' Year = 2018,
#' Ledger.side = "Supply",
#' Flow.aggregation.point = "TFC compare",
#' Flow = "Statistical differences",
#' Product = "Crude oil",
#' Unit = "ktoe",
#' E.dot = 10
#' ) %>%
#' IEATools::add_psut_matnames() %>%
#' stat_diffs_to_balancing() %>%
#' dplyr::filter(stringr::str_detect(Flow, "Statistical differences")) %>%
#' print()
stat_diffs_to_balancing <- function(.tidy_iea_df,
                              flow = IEATools::iea_cols$flow,
                              matnames = IEATools::mat_meta_cols$matnames,
                              e_dot = IEATools::iea_cols$e_dot,
                              stat_diffs = "Statistical differences",
                              balancing_matrix = "B"){
  .tidy_iea_df %>%
    dplyr::mutate(
      "{matnames}" := dplyr::case_when(
        .data[[flow]] == stat_diffs ~ balancing_matrix,
        TRUE ~ .data[[matnames]]
      ),
      "{e_dot}" := dplyr::case_when(
        .data[[flow]] == stat_diffs ~ -.data[[e_dot]],
        TRUE ~ .data[[e_dot]]
      )
    )
}




#' Moving stock changes flows to Balancing matrix
#'
#' This function sends stock changes flows to a balancing matrix B.
#' The Balancing matrix is akin to an additional final demand matrix,
#' meaning that flows akin to final demand (i.e. where stocks increase) will be positive,
#' while flows akin to supply (i.e. where stocks decrease) will be negative.
#'
#' See the Balancing matrix vignette for more information.
#' Note: one needs to add the column containing matrices names first,
#' most likely using the `IEATools::add_psut_matnames()` function.
#'
#' @param .tidy_iea_df The `.tidy_iea_df` for which Stock changes flows need to be sent to the Balancing matrix.
#' @param flow,e_dot See `IEATools::iea_cols`.
#' @param matnames The column name of the column having matrices names.
#'                 Default is `IEATools::mat_meta_cols$matnames`.
#' @param stock_changes The name of the Stock changes flows.
#'                      Default is "Stock changes".
#' @param balancing_matrix The name of the Balancing matrix.
#'                Default is "B".
#'
#' @return A `.tidy_iea_df` with Stock changes flows sent to the Balancing matrix.
#' @export
#'
#' @examples
#' tidy_AB_data %>%
#' tibble::add_row(
#' Country = "A",
#' Method = "PCM",
#' Energy.type = "Energy",
#' Last.stage = "Final",
#' Year = 2018,
#' Ledger.side = "Supply",
#' Flow.aggregation.point = "Supply",
#' Flow = "Stock changes",
#' Product = "Crude oil",
#' Unit = "ktoe",
#' E.dot = 10
#' ) %>%
#' IEATools::add_psut_matnames() %>%
#' stock_changes_to_balancing() %>%
#' dplyr::filter(stringr::str_detect(Flow, "Statistical differences")) %>%
#' print()
stock_changes_to_balancing <- function(.tidy_iea_df,
                                 flow = IEATools::iea_cols$flow,
                                 matnames = IEATools::mat_meta_cols$matnames,
                                 e_dot = IEATools::iea_cols$e_dot,
                                 stock_changes = IEATools::interface_industries$stock_changes,
                                 balancing_matrix = "B"){
  .tidy_iea_df %>%
    dplyr::mutate(
      "{matnames}" := dplyr::case_when(
        stringr::str_detect(.data[[flow]], stock_changes) ~ balancing_matrix,
        TRUE ~ .data[[matnames]]
      ),
      "{e_dot}" := dplyr::case_when(
        stringr::str_detect(.data[[flow]], stock_changes) ~ -.data[[e_dot]],
        TRUE ~ .data[[e_dot]]
      )
    )
}



#' Moves international bunkers flows to the Balancing matrix
#'
#' This function moves international bunkers flows to the Balancing matrix.
#' The Balancing matrix is akin to an additional final demand matrix,
#' meaning that flows akin to final demand (i.e. where stocks increase) will be positive,
#' while flows akin to supply (i.e. where stocks decrease) will be negative.
#'
#' See the Balancing matrix vignette for more information.
#' Note: one needs to add the column containing matrices names first,
#' most likely using the `IEATools::add_psut_matnames()` function.
#'
#' @param .tidy_iea_df The `.tidy_iea_df` for which Stock changes flows need to be sent to the Balancing matrix.
#' @param flow,e_dot See `IEATools::iea_cols`.
#' @param matnames The column name of the column having matrices names.
#'                 Default is `IEATools::mat_meta_cols$matnames`.
#' @param international_marine_bunkers The name of the international marine bunkers flows in the `.tidy_iea_df`.
#'                                     Default is `IEATools::interface_industries$international_marine_bunkers`.
#' @param international_aviation_bunkers The name of the international aviation bunkers flows in the `.tidy_iea_df`.
#'                                     Default is `IEATools::interface_industries$international_aviation_bunkers`.
#' @param balancing_matrix The name of the Balancing matrix.
#'                Default is "B".
#'
#' @return The new `.tidy_iea_df` with international bunkers ascribed to the Balancing matrix.
#' @export
international_bunkers_to_balancing <- function(.tidy_iea_df,
                                             flow = IEATools::iea_cols$flow,
                                             matnames = IEATools::mat_meta_cols$matnames,
                                             e_dot = IEATools::iea_cols$e_dot,
                                             international_marine_bunkers = "International marine bunkers",
                                             international_aviation_bunkers = "International aviation bunkers",
                                             balancing_matrix = "B"){

  .tidy_iea_df %>%
    dplyr::mutate(
      "{matnames}" := dplyr::case_when(
        (stringr::str_detect(.data[[flow]], international_marine_bunkers) | stringr::str_detect(.data[[flow]], international_aviation_bunkers)) ~ balancing_matrix,
        TRUE ~ .data[[matnames]]
      ),
      "{e_dot}" := dplyr::case_when(
        (stringr::str_detect(.data[[flow]], international_marine_bunkers) | stringr::str_detect(.data[[flow]], international_aviation_bunkers)) ~ -.data[[e_dot]],
        TRUE ~ .data[[e_dot]]
      )
    )
}


#' Converts jet fuel type gasoline into motor gasoline
#'
#' This function converts flows of the "Gasoline type jet fuel" product into flows of "Motor gasoline excl. biofuels".
#' The function then gathers those flows into a single new flow. Basically, it aggregates those two products and keeps the
#' "Motor gasoline excl. biofuels" product name.
#'
#' @param .tidy_iea_df The `.tidy_iea_df` for which jet fuel type gasoline needs to be converted into motor gasoline.
#' @param product,e_dot,flow See `IEATools::iea_cols`.
#'
#' @return A `.tidy_iea_df` for which jet fuel type gasoline is converted into motor gasoline.
#' @export
#'
#' @examples
#' # Here we add a flow of "Gasoline type jet fuel":
#' tidy_AB_data %>%
#'tibble::add_row(
#'  Country = "A",
#'  Method = "PCM",
#'  Energy.type = "E",
#'  Last.stage = "Final",
#'  Year = 2018,
#'  Product = "Gasoline type jet fuel",
#'  Ledger.side = "Consumption",
#'  Flow.aggregation.point = "Industry",
#'  Flow = "Iron and steel",
#'  Unit = "ktoe",
#'  E.dot = 20
#') %>%
#'  dplyr::filter(Country == "A" & stringr::str_detect(Product, "(G|g)asoline")) %>%
#'  print()
#' # Then we gather both flows:
#' tidy_AB_data %>%
#'tibble::add_row(
#'  Country = "A",
#'  Method = "PCM",
#'  Energy.type = "E",
#'  Last.stage = "Final",
#'  Year = 2018,
#'  Product = "Gasoline type jet fuel",
#'  Ledger.side = "Consumption",
#'  Flow.aggregation.point = "Industry",
#'  Flow = "Iron and steel",
#'  Unit = "ktoe",
#'  E.dot = 20
#') %>%
#'  dplyr::filter(Country == "A" & stringr::str_detect(Product, "(G|g)asoline")) %>%
#'  convert_fuel_gasoline_into_motor_gasoline() %>%
#'  print()
convert_fuel_gasoline_into_motor_gasoline <- function(.tidy_iea_df,
                                                      product = IEATools::iea_cols$product,
                                                      flow = IEATools::iea_cols$flow,
                                                      e_dot = IEATools::iea_cols$e_dot){

  .tidy_iea_df %>%
    dplyr::mutate(
      "{product}" := dplyr::case_when(
        .data[[product]] == "Gasoline type jet fuel" ~ "Motor gasoline excl. biofuels",
        TRUE ~ .data[[product]]
      ),
      "{flow}" := dplyr::case_when(
        stringr::str_detect(.data[[flow]], "Gasoline type jet fuel") ~ stringr::str_replace(.data[[flow]], "Gasoline type jet fuel", "Motor gasoline excl. biofuels"),
        TRUE ~ .data[[flow]]
      )
    ) %>%
    matsindf::group_by_everything_except(e_dot) %>%
    dplyr::summarise(
      "{e_dot}" := sum(.data[[e_dot]])
    )
}


#' Adds a balancing vector to the Balancing matrix
#'
#' This function adds a balancing vector to the `.tidy_iea_df`
#' in the Balancing matrix when the data frame is not balanced.
#'
#' The balancing flow is added by adding a single flow for each product
#' for which consumption and supply are not balanced. So, a single vector is added,
#' but a few flows may be added, depending the number of products for which flows do not balance.
#' The ledger side of balancing flows is "balancing", as these are neither actual
#' supply nor consumption flows, but unbalances that are somehow present in IEA data.
#' Note: one needs to add the column containing matrices names first,
#' most likely using the `IEATools::add_psut_matnames()` function.
#'
#' @param .tidy_iea_df The `.tidy_iea_df` for which a balancing vector needs being added.
#' @param country,method,energy_type,last_stage,year,ledger_side,flow_aggregation_point,flow,product,e_dot,unit See `IEATools::iea_cols`.
#' @param supply The name of the supply ledger side.
#'               Default is `IEATools::ledger_sides$supply`.
#' @param consumption The name of the consumption ledger side.
#'                    Default is `IEATools::ledger_sides$consumption`.
#' @param matnames The column name of the column having matrices names.
#'                 Default is `IEATools::mat_meta_cols$matnames`.
#' @param balancing The name of the balancing ledger side.
#'                  Default is "balancing".
#' @param supply_sum The name of the supply sum column (total supply for each product).
#'                   Default is "supply_sum".
#' @param consumption_sum The name of the consumption sum column (total consumption for each product).
#'                        Default is "consumption_sum".
#' @param supply_minus_consumption The name of the column calculating the difference between total supply and consumption for each product.
#'                                 Default is "supply_minus_consumption".
#' @param balance_OK A boolean stating whether flows for each product are balanced or not.
#'                   Default is "balance_OK".
#' @param err The numerical error observed when calculating the balance. Balanced flows will return 0
#'            Default is "err".
#' @param balancing_matrix The name of the Balancing matrix.
#'                Default is "B".
#'
#' @return The `.tidy_iea_df` with balancing flows added when needed.
#' @export
#'
#' @examples
#' # When we build the MR-PSUT with the AB example, we get unbalanced products.
#' # Note that this is done on purpose to reflect reality of IEA data.
#' tidy_AB_data %>%
#' IEATools::add_psut_matnames() %>%
#'  transform_to_gma() %>%
#'  IEATools::calc_tidy_iea_df_balances() %>%
#'  dplyr::filter(balance_OK == FALSE) %>%
#'  print()
#' # Now, if we add the balancing vector, we obtain balanced flows:
#' tidy_AB_data %>%
#' IEATools::add_psut_matnames() %>%
#'  transform_to_gma() %>%
#'  add_balancing_vector() %>%
#'  IEATools::calc_tidy_iea_df_balances() %>%
#'  dplyr::filter(balance_OK == FALSE) %>%
#'  print()
#' # Let's have a look to balancing flows, which are all ascribed to the Balancing matrix:
#' tidy_AB_data %>%
#' IEATools::add_psut_matnames() %>%
#'  transform_to_gma() %>%
#'  add_balancing_vector() %>%
#'  dplyr::filter(matnames == "B") %>%
#'  print()
add_balancing_vector <- function(.tidy_iea_df,
                                 # Input column names
                                 country = IEATools::iea_cols$country,
                                 method = IEATools::iea_cols$method,
                                 energy_type = IEATools::iea_cols$energy_type,
                                 last_stage = IEATools::iea_cols$last_stage,
                                 year = IEATools::iea_cols$year,
                                 ledger_side = IEATools::iea_cols$ledger_side,
                                 flow_aggregation_point = IEATools::iea_cols$flow_aggregation_point,
                                 flow = IEATools::iea_cols$flow,
                                 product = IEATools::iea_cols$product,
                                 e_dot = IEATools::iea_cols$e_dot,
                                 unit = IEATools::iea_cols$unit,
                                 supply = IEATools::ledger_sides$supply,
                                 consumption = IEATools::ledger_sides$consumption,
                                 matnames = IEATools::mat_meta_cols$matnames,
                                 balancing = "balancing",
                                 # Output column names
                                 supply_sum = "supply_sum",
                                 consumption_sum = "consumption_sum",
                                 supply_minus_consumption = "supply_minus_consumption",
                                 balance_OK = "balance_OK",
                                 err = "err",
                                 balancing_matrix = "B"){

  # Check for which products the flows are unbalanced
  balances <- .tidy_iea_df %>%
    IEATools::calc_tidy_iea_df_balances()

  balancing_vector <- balances %>%
    dplyr::filter(balance_OK == FALSE) %>%
    dplyr::select(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[product]], .data[[unit]], .data[[err]]) %>%
    dplyr::mutate(
      "{ledger_side}" := balancing,
      "{flow_aggregation_point}" := balancing,
      "{flow}" := balancing,
      "{matnames}" := balancing_matrix
    ) %>%
    dplyr::rename(
      "{e_dot}" := .data[[err]]
    )

  .tidy_iea_df %>%
    dplyr::bind_rows(balancing_vector) %>%
    return()
}




#' Relocates export flows in the Balancing matrix
#'
#' This function relocates exports to the balancing matrix B.
#' The Balancing matrix is akin to an additional final demand matrix.
#'
#' See the Balancing matrix vignette for more information.
#' Note: one needs to add the column containing matrices names first,
#' most likely using the `IEATools::add_psut_matnames()` function.
#'
#' @param .tidy_iea_df The `.tidy_iea_df` for which export flows need to be relocated in the Balancing matrix.
#' @param flow,e_dot See `IEATools::iea_cols`.
#' @param matnames The column name of the column containing matrices names.
#'                 Default is `IEATools::mat_meta_cols$matnames`.
#' @param exports The string that identifies export flows.
#'                Default is `IEATools::interface_industries$exports`.
#' @param balancing_matrix The name of the Balancing matrix.
#'                         Default is "B".
#'
#' @return A `.tidy_iea_df` with Stock changes flows relocated in the Balancing matrix.
#' @export
#'
#' @examples
#' tidy_AB_data %>%
#' IEATools::add_psut_matnames() %>%
#' dplyr::filter(stringr::str_detect(Flow, "Exports")) %>%
#' exports_to_balancing() %>%
#' print()
exports_to_balancing <- function(.tidy_iea_df,
                                 flow = IEATools::iea_cols$flow,
                                 matnames = IEATools::mat_meta_cols$matnames,
                                 e_dot = IEATools::iea_cols$e_dot,
                                 exports = IEATools::interface_industries$exports,
                                 balancing_matrix = "B"){
  .tidy_iea_df %>%
    dplyr::mutate(
      "{matnames}" := dplyr::case_when(
        stringr::str_detect(.data[[flow]], exports) ~ balancing_matrix,
        TRUE ~ .data[[matnames]]
      )
    )
}


#' Relocates losses in the Balancing matrix
#'
#' This function relocates losses to the balancing matrix B.
#' The Balancing matrix is akin to an additional final demand matrix.
#'
#' See the Balancing matrix vignette for more information.
#' Note: one needs to add the column containing matrices names first,
#' most likely using the `IEATools::add_psut_matnames()` function.
#'
#' Importantly, we change the sign of losses flows when moving them to the balancing matrix, so that they stay akin to
#' final demand flows. And we change their ledger side to "Consumption" so that the energy balance is respected.
#'
#' @param .tidy_iea_df The `.tidy_iea_df` for which losses need to be relocated in the Balancing matrix.
#' @param flow,e_dot,ledger_side See `IEATools::iea_cols`.
#' @param matnames The column name of the column containing matrices names.
#'                 Default is `IEATools::mat_meta_cols$matnames`.
#' @param losses The name of losses flows.
#'               Default is `IEATools::tfc_compare_flows$losses`.
#' @param balancing_matrix The name of the Balancing matrix.
#'                         Default is "B".
#'
#' @return A `.tidy_iea_df` with losses relocated in the Balancing matrix.
#' @export
#'
#' @examples
#' tidy_AB_data %>%
#' tibble::add_row(
#'  Country = "A",
#'  Methpd = "PCM",
#'  Energy.type = "E",
#'  Last.stage = "Final",
#'  Year = 2018,
#'  Ledger.side = "Supply",
#'  Flow.aggregation.point = "TFC compare",
#'  Flow = "Losses",
#'  Product = "Electricity",
#'  Unit = "ktoe",
#'  E.dot = 1,
#' ) %>%
#'  IEATools::add_psut_matnames() %>%
#'  dplyr::filter(matnames == "B")
losses_to_balancing <- function(.tidy_iea_df,
                                flow = IEATools::iea_cols$flow,
                                matnames = IEATools::mat_meta_cols$matnames,
                                e_dot = IEATools::iea_cols$e_dot,
                                ledger_side = IEATools::iea_cols$ledger_side,
                                losses = IEATools::tfc_compare_flows$losses,
                                balancing_matrix = "B"){

  .tidy_iea_df %>%
    dplyr::mutate(
      "{matnames}" := dplyr::case_when(
        .data[[flow]] == losses ~ balancing_matrix,
        TRUE ~ .data[[matnames]]
      ),
      "{e_dot}" := dplyr::case_when(
        .data[[flow]] == losses ~ abs(.data[[e_dot]]),
        TRUE ~ .data[[e_dot]]
      ),
      "{ledger_side}" := dplyr::case_when(
        .data[[flow]] == losses ~ "Consumption",
        TRUE ~ .data[[ledger_side]]
      )
    )
}


#' Relocates non-energy uses flows in the balancing matrix
#'
#' This function relocates non-energy uses to the balancing matrix B.
#' The Balancing matrix is akin to an additional final demand matrix.
#'
#' See the Balancing matrix vignette for more information.
#' Note: one needs to add the column containing matrices names first,
#' most likely using the `IEATools::add_psut_matnames()` function.
#'
#' @param .tidy_iea_df The `.tidy_iea_df` for which non-energy use flows need to be relocated in the Balancing matrix.
#' @param flow,e_dot See `IEATools::iea_cols`.
#' @param matnames The column name of the column having matrices names.
#'                 Default is `IEATools::mat_meta_cols$matnames`.
#' @param non_energy_uses The string that identifies non-energy uses.
#'                        Default is `IEATools::tfc_flows$non_energy_use`.
#' @param balancing_matrix The name of the Balancing matrix.
#'                         Default is "B".
#'
#' @return A `.tidy_iea_df` with non-energy uses flows relocated in the Balancing matrix.
#' @export
#'
#' @examples
#' tidy_AB_data %>%
#' tibble::add_row(
#'  Country = "A",
#'  Method = "PCM",
#'  Energy.type = "E",
#'  Last.stage = "Final",
#'  Year = 2018,
#'  Ledger.side = "Supply",
#'  Flow.aggregation.point = "Consumption",
#'  Flow = "Non-energy use in whatever sector",
#'  Product = "Electricity",
#'  Unit = "ktoe",
#'  E.dot = 10,
#' ) %>%
#' IEATools::add_psut_matnames() %>%
#' dplyr::filter(matnames == "B")
non_energy_uses_to_balancing <- function(.tidy_iea_df,
                                         flow = IEATools::iea_cols$flow,
                                         matnames = IEATools::mat_meta_cols$matnames,
                                         e_dot = IEATools::iea_cols$e_dot,
                                         non_energy_uses = IEATools::tfc_flows$non_energy_use,
                                         balancing_matrix = "B"){
  .tidy_iea_df %>%
    dplyr::mutate(
      "{matnames}" := dplyr::case_when(
        stringr::str_detect(.data[[flow]], non_energy_uses) ~ balancing_matrix,
        TRUE ~ .data[[matnames]]
      )
    )
}


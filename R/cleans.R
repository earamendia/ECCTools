
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
#' @return A `.tidy_iea_df` for which trade flows are converte to net trade.
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





#' Moving statistical differences flows to Balancing matrix
#'
#' This function sends statistical differences flows to a balancing matrix Balancing.
#' The Balancing matrix is akin to an additional final demand matrix,
#' meaning that flows akin to final demand will be positive,
#' while flows akin to supply will be negative.
#'
#' See the Balacing matrix vignette for more information.
#' Note: one needs to add the column containing matrices names first,
#' most likely using the `IEATools::add_psut_matnames()` function.
#'
#' @param .tidy_iea_df The `.tidy_iea_df` for which statistical differences flows need to be sent to the Balancingmatrix.
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
#' stat_diffs_to_balancing_mat() %>%
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
#' See the Balacing matrix vignette for more information.
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
        stringr::str_detect(.data[[flow]], stock_changes) ~ balancng_matrix,
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
#' See the Balacing matrix vignette for more information.
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
#' @param epsilon The name of the Balancing matrix.
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

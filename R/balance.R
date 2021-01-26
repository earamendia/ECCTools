
# This function copies IEATools function but also includes the new "balancing" ledger side in the calculation.

calc_tidy_iea_df_balances_balancing <- function(.tidy_iea_df,
                                      # Input column names
                                      ledger_side = IEATools::iea_cols$ledger_side,
                                      flow_aggregation_point = IEATools::iea_cols$flow_aggregation_point,
                                      flow = IEATools::iea_cols$flow,
                                      product = IEATools::iea_cols$product,
                                      e_dot = IEATools::iea_cols$e_dot,
                                      unit = IEATools::iea_cols$unit,
                                      supply = IEATools::ledger_sides$supply,
                                      consumption = IEATools::ledger_sides$consumption,
                                      matnames = IEATools::iea_cols$matnames,
                                      balancing = "balancing",
                                      # Output column names
                                      supply_sum = "supply_sum",
                                      consumption_sum = "consumption_sum",
                                      supply_minus_consumption = "supply_minus_consumption",
                                      balance_OK = "balance_OK",
                                      err = "err",
                                      tol = 1e-6){
  # Calculate sums on a per-group basis
  grouping_names <- matsindf::everything_except(.tidy_iea_df, ledger_side, flow_aggregation_point, flow, e_dot, matnames)
  grouping_strings <- matsindf::everything_except(.tidy_iea_df, ledger_side, flow_aggregation_point, flow, e_dot, matnames, .symbols = FALSE)

  Tidy <- .tidy_iea_df %>%
    dplyr::group_by(!!!grouping_names)
  SupplySum <- Tidy %>%
    dplyr::filter(.data[[ledger_side]] == supply) %>%
    dplyr::summarise("{supply_sum}" := sum(.data[[e_dot]]))
  # Calculate the consumption sum on a per-group basis
  ConsumptionSum <- Tidy %>%
    dplyr::filter(.data[[ledger_side]] == consumption | .data[[ledger_side]] == balancing) %>%
    dplyr::summarise("{consumption_sum}" := sum(.data[[e_dot]]))
  # Return the difference between supply and consumption
  dplyr::full_join(SupplySum, ConsumptionSum, by = grouping_strings) %>%
    dplyr::mutate(
      "{supply_minus_consumption}" := .data[[supply_sum]] - .data[[consumption_sum]],
      "{balance_OK}" := dplyr::case_when(
        is.na(.data[[consumption_sum]]) ~ abs(.data[[supply_sum]]) <= tol,
        TRUE ~ abs(.data[[supply_sum]] - .data[[consumption_sum]]) <= tol
      ),
      "{err}" := dplyr::case_when(
        is.na(.data[[consumption_sum]]) ~ .data[[supply_sum]],
        TRUE ~ .data[[supply_minus_consumption]]
      )
    ) %>%
    dplyr::ungroup()
}


# This function adds a balancing vector, with positive entries where akin to consumption, and negative entries when akin to supply.
# Ledger side is "balancing".

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
                                 matnames = IEATools::iea_cols$matnames,
                                 balancing = "balancing",
                                 # Output column names
                                 supply_sum = "supply_sum",
                                 consumption_sum = "consumption_sum",
                                 supply_minus_consumption = "supply_minus_consumption",
                                 balance_OK = "balance_OK",
                                 err = "err",
                                 epsilon = "Epsilon",
                                 tol = 1e-6){

  # Check for which products the flows are unbalanced
  balances <- .tidy_iea_df %>%
    calc_tidy_iea_df_balances_balancing()

  balancing_vector <- balances %>%
    dplyr::filter(balance_OK == FALSE) %>%
    dplyr::select(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[product]], .data[[unit]], .data[[err]]) %>%
    dplyr::mutate(
      "{ledger_side}" := balancing,
      "{flow_aggregation_point}" := balancing,
      "{flow}" := balancing,
      "{matnames}" := epsilon
    ) %>%
    dplyr::rename(
      "{e_dot}" := .data[[err]]
    )

  .tidy_iea_df %>%
    dplyr::bind_rows(balancing_vector) %>%
    return()
}




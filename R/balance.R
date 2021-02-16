
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
                                 matnames = IEATools::mat_meta_cols$matnames,
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




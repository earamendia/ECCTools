
#' Adds a balancing vector to the Epsilon matrix
#'
#' This function adds a balancing vector to the `.tidy_iea_df`
#' in the Epsilon matrix when the data frame is not balanced.
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
#' @param epsilon The name of the Epsilon matrix.
#'                Default is "Epsilon".
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
#' # Let's have a look to balancing flows, which are all ascribed to the Epsilon matrix:
#' tidy_AB_data %>%
#' IEATools::add_psut_matnames() %>%
#'  transform_to_gma() %>%
#'  add_balancing_vector() %>%
#'  dplyr::filter(matnames == "Epsilon") %>%
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
                                 epsilon = "Epsilon"){

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
      "{matnames}" := epsilon
    ) %>%
    dplyr::rename(
      "{e_dot}" := .data[[err]]
    )

  .tidy_iea_df %>%
    dplyr::bind_rows(balancing_vector) %>%
    return()
}

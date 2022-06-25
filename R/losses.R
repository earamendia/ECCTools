
#' Specifies losses as a transformation industry
#'
#' Losses are originally treated as a final demand flow in the IEA WEEB balances. This function allows analysts to redefine losses
#' as a transformation industry, which takes as input "Product (before Losses)" and provides "Product".
#' Hence, final demand sectors still consume "Product", but where relevant, products go first through a "Losses (of Product)" industry.
#'
#' TO DO:!!! Importantly, we need to check that values do not become negative...!!!
#'
#' @param .tidy_iea_df The `.tidy__iea_df` for which losses need to be specified as transformation industry.
#' @param country,method,energy_type,last_stage,ledger_side,flow_aggregation_point,year,flow,product,unit,e_dot See `IEATools::iea_cols`.
#' @param matnames The name of the matrix name column in the input data frame.
#' @param losses The name of the "Losses" flows in the input data frame.
#'               Default is `IEATools::tfc_compare_flows$losses`.
#' @param imports The name of "Imports" flows in the input data frame.
#'                Default is `IEATools::interface_industries$imports`.
#' @param stat_diffs The name of "Statistical differences" flows in the input data frame.
#'                   Default is `IEATools::tfc_compare_flows$statistical_differences`.
#' @param transformation_processes The name of transformation processes in the flow aggregation point column.
#'                                 Default is `IEATools::tfc_compare_flows$transformation_processes`.
#' @param supply The name of the supply ledger side.
#'               Default is `IEATools::ledger_sides$supply`.
#' @param observation_string The name of a temporary column identifying the observation.
#'                           Default is "Observation_String".
#'
#' @return A `.tidy_iea_df` with losses specified as a transformation industry.
#' @export
#'
#' @examples
specify_losses_as_industry <- function(.tidy_iea_df,
                                       country = IEATools::iea_cols$country,
                                       method = IEATools::iea_cols$method,
                                       energy_type = IEATools::iea_cols$energy_type,
                                       last_stage = IEATools::iea_cols$last_stage,
                                       year = IEATools::iea_cols$year,
                                       ledger_side = IEATools::iea_cols$ledger_side,
                                       flow_aggregation_point = IEATools::iea_cols$flow_aggregation_point,
                                       flow = IEATools::iea_cols$flow,
                                       product = IEATools::iea_cols$product,
                                       unit = IEATools::iea_cols$unit,
                                       e_dot = IEATools::iea_cols$e_dot,
                                       matnames = IEATools::mat_meta_cols$matnames,
                                       losses = IEATools::tfc_compare_flows$losses,
                                       imports = IEATools::interface_industries$imports,
                                       stat_diffs = IEATools::tfc_compare_flows$statistical_differences,
                                       transformation_processes = IEATools::tfc_compare_flows$transformation_processes,
                                       supply = IEATools::ledger_sides$supply,
                                       observation_string = "Observation_String"){

  # General comment: we exclude Imports and Statistical differences from changes in supply
  # Otherwise for imports it will be a mess when building the MR-PSUT, and for statistical differences they don't make sense anyway.

  # Finding out, for each region and year, the products for which losses ought to be modelled
  products_losses_observations <- .tidy_iea_df %>%
    dplyr::filter(.data[[flow]] == losses) %>%
    dplyr::select(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[flow]],
                  .data[[product]], .data[[unit]]) %>%
    dplyr::mutate(
      "{observation_string}" := stringr::str_c(.data[[country]], "_", .data[[method]], "_", .data[[energy_type]], "_",
                                               .data[[last_stage]], "_", .data[[year]], "_", .data[[product]], "_")
    ) %>%
    dplyr::select(.data[[observation_string]]) %>%
    dplyr::pull()

  # Modifying the new supply flows so that they now supply "Product [before Losses]"
  modified_supply_excluding_losses <- .tidy_iea_df %>%
    dplyr::filter(
      stringr::str_c(.data[[country]], "_", .data[[method]], "_", .data[[energy_type]], "_", .data[[last_stage]], "_",
                     .data[[year]], "_", .data[[product]], "_") %in% products_losses_observations
    ) %>%
    dplyr::filter(.data[[matnames]] == "V") %>%
    dplyr::filter(! stringr::str_detect(.data[[flow]], imports)) %>%
    dplyr::filter(! stringr::str_detect(.data[[flow]], stat_diffs)) %>%
    dplyr::mutate(
      "{product}" := stringr::str_c(.data[[product]], " [before Losses]")
    )

  # Building energy flows supplied by the new losses industries
  supply_industry_losses_df <- .tidy_iea_df %>%
    dplyr::filter(
      stringr::str_c(.data[[country]], "_", .data[[method]], "_", .data[[energy_type]], "_", .data[[last_stage]], "_",
                     .data[[year]], "_", .data[[product]], "_") %in% products_losses_observations
    ) %>%
    dplyr::filter(.data[[matnames]] == "V" | stringr::str_detect(.data[[flow]], losses)) %>%
    dplyr::filter(! stringr::str_detect(.data[[flow]], imports)) %>%
    dplyr::filter(! stringr::str_detect(.data[[flow]], stat_diffs)) %>%
    dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[product]], .data[[unit]]) %>%
    dplyr::summarise(
      "{e_dot}" := sum(.data[[e_dot]]) # losses have negative sign so we can do like this.
    ) %>%
    dplyr::mutate(
      "{ledger_side}" := supply,
      "{flow_aggregation_point}" := transformation_processes,
      "{flow}" := stringr::str_c("Losses [of ", .data[[product]], "]"),
      "{matnames}" := "V"
    )

  # Building energy flows consumed by the new losses industries
  inputs_to_losses_industry_df <- .tidy_iea_df %>%
    dplyr::filter(
      stringr::str_c(.data[[country]], "_", .data[[method]], "_", .data[[energy_type]], "_", .data[[last_stage]], "_",
                     .data[[year]], "_", .data[[product]], "_") %in% products_losses_observations
    ) %>%
    dplyr::filter(.data[[matnames]] == "V") %>%#
    dplyr::filter(! stringr::str_detect(.data[[flow]], imports)) %>%
    dplyr::filter(! stringr::str_detect(.data[[flow]], stat_diffs)) %>%
    dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[product]], .data[[unit]]) %>%
    dplyr::summarise(
      "{e_dot}" := sum(.data[[e_dot]])
    ) %>%
    dplyr::mutate(
      "{ledger_side}" := supply,
      "{flow_aggregation_point}" := transformation_processes,
      "{flow}" := stringr::str_c("Losses [of ", .data[[product]], "]"),
      "{product}" := stringr::str_c(.data[[product]], " [before Losses]"),
      "{matnames}" := "U_feed"
    )

  # Need to create data frames with imports and statistical differences located in V matrix
  imports_stat_diffs_V_df <- .tidy_iea_df %>%
    dplyr::filter(
      stringr::str_c(.data[[country]], "_", .data[[method]], "_", .data[[energy_type]], "_", .data[[last_stage]], "_",
                     .data[[year]], "_", .data[[product]], "_") %in% products_losses_observations
    ) %>%
    dplyr::filter(matnames == "V" & (stringr::str_detect(.data[[flow]], imports) | stringr::str_detect(.data[[flow]], stat_diffs)))

  # Check that all flows supplied by losses remain positive. Else there is an issue.
  supply_industry_losses_df %>%
    dplyr::filter(.data[[e_dot]] < 0) %>%
    nrow() %>%
    testthat::expect_equal(0)

  # CAREFUL HERE ON HOW TO DEFINE THIS BIT TO AVOID DOUBLE ACCOUNTING FLOWS!!!!
  tidy_iea_df_with_losses_specified <- .tidy_iea_df %>%
    dplyr::filter(
      ! ((stringr::str_c(.data[[country]], "_", .data[[method]], "_", .data[[energy_type]], "_", .data[[last_stage]], "_",
                       .data[[year]], "_", .data[[product]], "_") %in% products_losses_observations) && (.data[[matnames == "V"]]))
    ) %>%
    dplyr::bind_rows(
      modified_supply_excluding_losses,
      supply_industry_losses_df,
      inputs_to_losses_industry_df,
      imports_stat_diffs_V_df
    )

  return(tidy_iea_df_with_losses_specified)
}



specify_losses_as_industry <- function(.tidy_iea_df,
                                       country = IEATools::iea_cols$country,
                                       method = IEATools::iea_cols$method,
                                       energy_type = IEATools::iea_cols$energy_type,
                                       last_stage = IEATools::iea_cols$last_stage,
                                       year = IEATools::iea_cols$year,
                                       flow = IEATools::iea_cols$flow,
                                       product = IEATools::iea_cols$product,
                                       unit = IEATools::iea_cols$unit,
                                       e_dot = IEATools::iea_cols$e_dot,
                                       matnames = IEATools::mat_meta_cols,
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
      "{Observation_String}" := stringr::str_c(.data[[country]], "_", .data[[method]], "_", .data[[energy_type]], "_",
                                               .data[[last_stage]], "_", .data[[year]], "_", .data[[product]], "_")
    ) %>%
    dplyr::select(.data[[observation_string]]) %>%
    dplyr::pull()

  # Modifying the new supply flows so that they now supply "Product [before Losses]"
  modified_supply_excluding_losses <- .tidy_iea_df %>%
    dplyr::filter(
      stringr::str_c(.data[[country]], "_", .data[[method]], "_", .data[[energy_type]], "_", .data[[last_stage]], "_",
                     .data[[year]], "_", .data[[product]], "_") %in% observations_losses
    ) %>%
    dplyr::filter(.data[[matnames]] == "V") %>%
    dplyr::filter(! stringr::str_detect(.data[[flow]], imports)) %>%
    dplyr::filter(! stringr::str_detect(.data[[flow]], stat_diffs)) %>%
    dplyr::mutate(
      "{product}" := stringr::str_c(.data[[product]], " [before Losses]")
    )

  # Building energy flows supplied by the new losses industries
  supply_industry_losses_df <- specified_world_iea_data_2019 %>%
    dplyr::filter(
      stringr::str_c(.data[[country]], "_", .data[[method]], "_", .data[[energy_type]], "_", .data[[last_stage]], "_",
                     .data[[year]], "_", .data[[product]], "_") %in% observations_losses
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
      "{flow}" := stringr::str_c("Losses [of ", .data[[flow]], "]"),
      "{matnames}" := "V"
    )

  # Building energy flows consumed by the new losses industries
  inputs_to_losses_industry_df <- specified_world_iea_data_2019 %>%
    dplyr::filter(
      stringr::str_c(.data[[country]], "_", .data[[method]], "_", .data[[energy_type]], "_", .data[[last_stage]], "_",
                     .data[[year]], "_", .data[[product]], "_") %in% observations_losses
    ) %>%
    dplyr::filter(.data[[matnames]] == "V") %>%#
    dplyr::filter(! stringr::str_detect(.data[[flow]], imports)) %>%
    dplyr::filter(! stringr::str_detect(.data[[flow]], stat_diffs)) %>%
    dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[product]], .data[[unit]]) %>%
    dplyr::summarise(
      "{e_dot}" := sum(.data[[e_dot]]) # losses have negative sign so we can do like this.
    ) %>%
    dplyr::mutate(
      "{ledger_side}" := supply,
      "{flow_aggregation_point}" := transformation_processes,
      "{fow}" := stringr::str_c("Losses [of ", .data[[product]], "]"),
      "{product}" := stringr::str_c(.data[[product]], " [before Losses]"),
      "{matnames}" := "U_feed"
    )


  # CAREFUL HERE ON HOW TO DEFINE THIS BIT TO AVOID DOUBLE ACCOUNTING FLOWS!!!!
  tidy_iea_df_with_losses_specified <- .tidy_iea_df

  return(tidy_iea_df_with_losses_specified)
}


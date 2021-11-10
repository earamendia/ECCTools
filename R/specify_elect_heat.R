
# NEW WORK HERE:

# First, specifying elec/heat flows from renewables:

specify_elect_heat_renewables <- function(.tidy_iea_df,
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
                                          # Flow aggregation points
                                          transformation_processes = IEATools::aggregation_flows$transformation_processes,
                                          eiou_flows = IEATools::aggregation_flows$energy_industry_own_use,
                                          # Elec and heat producing industries
                                          main_act_prod_elect = IEATools::main_act_plants$main_act_prod_elect_plants,
                                          main_act_prod_chp = IEATools::main_act_plants$main_act_prod_chp_plants,
                                          main_act_prod_heat = IEATools::main_act_plants$main_act_prod_heat_plants,
                                          autoprod_elect = "Autoproducer electricity plants",
                                          autoprod_chp = "Autoproducer CHP plants",
                                          autoprod_heat = "Autoproducer heat plants",
                                          # Energy product names
                                          hydro = "Hydro",
                                          geothermal = "Geothermal",
                                          solar_pv = "Solar photovoltaics",
                                          solar_th = "Solar thermal",
                                          tide_wave_ocean = "Tide, wave and ocean",
                                          wind = "Wind",
                                          electricity = "Electricity",
                                          heat = "Heat",
                                          # Constants - see WEEB documentation
                                          ratio_hydro_to_input = 1,
                                          ratio_solar_PV_to_input = 1,
                                          ratio_solar_thermal_elect_to_input = 0.33,
                                          ratio_solar_thermal_heat_to_input = 1,
                                          ratio_wind_to_input = 1,
                                          ratio_geothermal_elect_to_input = 0.1,
                                          ratio_geothermal_heat_to_input = 0.5,
                                          ratio_tidal_wave_to_input = 1){

  # Empty tibble with energy product names:
  products_tibble <- tibble::tibble("{hydro}" := NA,
                                    "{geothermal}" := NA,
                                    "{solar_pv}" := NA,
                                    "{solar_th}" := NA,
                                    "{tide_wave_ocean}" := NA,
                                    "{wind}" := NA,
                                    "{electricity}" := NA,
                                    "{heat}" := NA)

  # Defining product list:
  renewable_product_list <- c(hydro, geothermal, solar_pv, solar_th, tide_wave_ocean, wind)
  products_list <- c(hydro, geothermal, solar_pv, solar_th, tide_wave_ocean, wind, electricity, heat)

  # Defining list of elect and heat producer activities:
  elect_heat_producer_industries <- c(main_act_prod_elect, main_act_prod_chp, main_act_prod_heat, autoprod_elect, autoprod_chp, autoprod_heat)

  # Here we keep only the flows that we are going to modify:
  intermediary_modified_flows <- .tidy_iea_df %>%
    dplyr::filter(
      .data[[flow_aggregation_point]] == transformation_processes &
        ((.data[[flow]] %in% elect_heat_producer_industries & .data[[product]] %in% products_list))
    ) %>%
    tidyr::pivot_wider(names_from = .data[[product]], values_from = .data[[e_dot]]) %>%
    dplyr::select(-tidyselect::any_of({e_dot}))

  # Storing the names
  names_intermediary_modified_flows <- names(intermediary_modified_flows)


  # Modifying output and input flows:
  modified_flows_inputs_outputs <- intermediary_modified_flows %>%
    # Making sure all products in products_tibble exist as columns prior to manipulating the tibble:
    tibble::add_column(!!products_tibble[! names(products_tibble) %in% names_intermediary_modified_flows]) %>%
    # Putting zeros where we have NAs
    dplyr::mutate(
      "{hydro}" := tidyr::replace_na(.data[[hydro]], 0),
      "{geothermal}" := tidyr::replace_na(.data[[geothermal]], 0),
      "{solar_pv}" := tidyr::replace_na(.data[[solar_pv]], 0),
      "{solar_th}" := tidyr::replace_na(.data[[solar_th]], 0),
      "{tide_wave_ocean}" := tidyr::replace_na(.data[[tide_wave_ocean]], 0),
      "{wind}" := tidyr::replace_na(.data[[wind]], 0),
      "{electricity}" := tidyr::replace_na(.data[[electricity]], 0),
      "{heat}" := tidyr::replace_na(.data[[heat]], 0)
    ) %>%
    # Now come the interesting bit, modifying flows output flows
    dplyr::mutate(
      # Determining the share of electricity against heat, when heat is involved:
      "{share_elect_output_From_Func}" := .data[[electricity]] / (.data[[electricity]] + .data[[heat]]),
      # Modifying input flows:
      "{electricity}" := dplyr::case_when(
        (.data[[flow]] == main_act_prod_elect | .data[[flow]] == autoprod_elect) ~ .data[[electricity]] + .data[[hydro]] * ratio_hydro_to_input +
          .data[[geothermal]] * ratio_geothermal_elect_to_input + .data[[solar_pv]] * ratio_solar_PV_to_input +
          .data[[solar_th]] * ratio_solar_thermal_elect_to_input + .data[[tide_wave_ocean]] * ratio_tidal_wave_to_input +
          .data[[wind]] * ratio_wind_to_input,
        (.data[[flow]] == main_act_prod_chp | .data[[flow]] == autoprod_chp) ~ .data[[electricity]] +
          .data[[geothermal]] * ratio_geothermal_elect_to_input * .data[[share_elect_output_From_Func]],
        (.data[[flow]] == main_act_prod_heat | .data[[flow]] == autoprod_heat) ~ .data[[electricity]]
      ),
      "{heat}" := dplyr::case_when(
        (.data[[flow]] == main_act_prod_elect | .data[[flow]] == autoprod_elect) ~ .data[[heat]],
        (.data[[flow]] == main_act_prod_chp | .data[[flow]] == autoprod_chp) ~ .data[[heat]] +
          .data[[geothermal]] * ratio_geothermal_heat_to_input * (1 - .data[[share_elect_output_From_Func]]),
        (.data[[flow]] == main_act_prod_heat | .data[[flow]] == autoprod_heat) ~ .data[[heat]] + .data[[geothermal]] * ratio_geothermal_heat_to_input +
          .data[[solar_th]] * ratio_solar_thermal_heat_to_input
      ),
      # Modifying output flows:
      Electricity_Renewables = dplyr::case_when(
        (.data[[flow]] == main_act_prod_elect | .data[[flow]] == autoprod_elect) ~ - (.data[[hydro]] * ratio_hydro_to_input +
                                                                                        .data[[geothermal]] * ratio_geothermal_elect_to_input + .data[[solar_pv]] * ratio_solar_PV_to_input + .data[[solar_th]] * ratio_solar_thermal_elect_to_input +
                                                                                        .data[[tide_wave_ocean]] * ratio_tidal_wave_to_input + .data[[wind]] * ratio_wind_to_input),
        (.data[[flow]] == main_act_prod_chp | .data[[flow]] == autoprod_chp) ~ - .data[[geothermal]] * ratio_geothermal_elect_to_input * .data[[share_elect_output_From_Func]],

        (.data[[flow]] == main_act_prod_heat | .data[[flow]] == autoprod_heat) ~ 0
      ),
      Heat_Renewables = dplyr::case_when(
        (.data[[flow]] == main_act_prod_elect | .data[[flow]] == autoprod_elect) ~ 0,
        (.data[[flow]] == main_act_prod_chp | .data[[flow]] == autoprod_chp) ~ - .data[[geothermal]] * ratio_geothermal_heat_to_input * (1 - .data[[share_elect_output_From_Func]]),
        (.data[[flow]] == main_act_prod_heat | .data[[flow]] == autoprod_heat) ~ - (.data[[geothermal]] * ratio_geothermal_heat_to_input + .data[[solar_th]] * ratio_solar_thermal_heat_to_input)
      ),
    ) %>%
    dplyr::select(-.data[[share_elect_output_From_Func]]) %>%
    tidyr::pivot_longer(cols = c({electricity}, {heat}, {hydro}, {geothermal}, {solar_pv}, {solar_th}, {tide_wave_ocean}, {wind}, "Electricity_Renewables", "Heat_Renewables")
                        , values_to = {e_dot}, names_to = {product}) %>%
    dplyr::filter(.data[[e_dot]] != 0) %>%
    dplyr::mutate(
      "{flow}" := dplyr::case_when(
        stringr::str_detect(.data[[product]], "Renewables") ~ "Renewable energy plant",
        .data[[product]] %in% renewable_product_list ~ "Renewable energy plant",
        TRUE ~ .data[[flow]]
      )
    )

  # Determining share of output due to renewables versus other energy products
  share_output_renewables <- .tidy_iea_df %>%
    dplyr::filter(
      .data[[flow_aggregation_point]] == transformation_processes &
        ((.data[[flow]] %in% elect_heat_producer_industries & .data[[product]] %in% products_list))
    ) %>%
    tidyr::pivot_wider(names_from = .data[[product]], values_from = .data[[e_dot]]) %>%
    dplyr::select(-tidyselect::any_of({e_dot})) %>%
    tibble::add_column(!!products_tibble[! names(products_tibble) %in% names_intermediary_modified_flows]) %>%
    # Putting zeros where we have NAs
    dplyr::mutate(
      "{hydro}" := tidyr::replace_na(.data[[hydro]], 0),
      "{geothermal}" := tidyr::replace_na(.data[[geothermal]], 0),
      "{solar_pv}" := tidyr::replace_na(.data[[solar_pv]], 0),
      "{solar_th}" := tidyr::replace_na(.data[[solar_th]], 0),
      "{tide_wave_ocean}" := tidyr::replace_na(.data[[tide_wave_ocean]], 0),
      "{wind}" := tidyr::replace_na(.data[[wind]], 0),
      "{electricity}" := tidyr::replace_na(.data[[electricity]], 0),
      "{heat}" := tidyr::replace_na(.data[[heat]], 0)
    ) %>%
    dplyr::mutate(
      "{share_elect_output_From_Func}" := .data[[electricity]] / (.data[[electricity]] + .data[[heat]]),
      Share_Renewables = dplyr::case_when(
        (.data[[flow]] == main_act_prod_elect | .data[[flow]] == autoprod_elect) ~ - (.data[[hydro]] * ratio_hydro_to_input + .data[[geothermal]] * ratio_geothermal_elect_to_input +
                                                                                        .data[[solar_pv]] * ratio_solar_PV_to_input + .data[[solar_th]] * ratio_solar_thermal_elect_to_input +
                                                                                        .data[[tide_wave_ocean]] * ratio_tidal_wave_to_input + .data[[wind]] * ratio_wind_to_input) / .data[[electricity]] ,
        (.data[[flow]] == main_act_prod_chp | .data[[flow]] == autoprod_chp) ~ - (.data[[geothermal]] * ratio_geothermal_elect_to_input * .data[[share_elect_output_From_Func]] +
                                                                                    .data[[geothermal]] * ratio_geothermal_heat_to_input * (1 - .data[[share_elect_output_From_Func]])) / (.data[[electricity]] + .data[[heat]])
        (.data[[flow]] == main_act_prod_heat | .data[[flow]] == autoprod_heat) ~ - (.data[[geothermal]] * ratio_geothermal_heat_to_input +
                                                                                      .data[[solar_th]] * ratio_solar_thermal_heat_to_input) / .data[[heat]]
      )
    ) %>%
    dplyr::select(-tidyselect::any_of({hydro}, {geothermal}, {solar_pv}, {solar_th}, {tide_wave_ocean}, {wind}, {electricity}, {heat}))


  # Modifying EIOU flows
  modified_eiou_flows <- .tidy_iea_df %>%
    dplyr::filter((.data[[flow_aggregation_point]] == eiou_flows) & (.data[[flow]] %in% elect_heat_producer_industries)) %>%
    dplyr::left_join(share_output_renewables, by = c({country}, {year}, {last_stage}, {energy_type})) %>%
    dplyr::mutate(
      "{e_dot_renewables}" := .data[[e_dot]] * Share_Renewables,
      "{e_dot_rest}" := .data[[e_dot]] * (1 - Share_Renewables)
    ) %>%
    dplyr::select(-tidyselect::any_of({e_dot})) %>%
    dplyr::select(-Share_Renewables) %>%
    tidyr::pivot_longer(cols = c({e_dot_renewables}, {e_dot_rest}), names_to = "Renewables", values_to = {e_dot}) %>%
    dplyr::mutate(
      "{flow}" := dplyr::case_when(
        .data[["Renewables"]] == {e_dot_renewables} ~ "Renewable energy plant",
        TRUE ~ .data[[flow]]
      )
    ) %>%
    dplyr::select(-Renewables)


  to_return <- .tidy_iea_df %>%
    # FILTER OUT FIRST
    dplyr::filter(
      ! (.data[[flow_aggregation_point]] == transformation_processes &
        ((.data[[flow]] %in% elect_heat_producer_industries & .data[[product]] %in% products_list)))
    ) %>%
    dplyr::filter(
      ! ((.data[[flow_aggregation_point]] == eiou_flows) & (.data[[flow]] %in% elect_heat_producer_industries))
    ) %>%
    # THEN BIND ROWS
    dplyr::bind_rows(modified_flows_inputs_outputs) %>%
    dplyr::bind_rows(modified_eiou_flows) %>%
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
      #Eliminate the column we added.
      "{negzeropos}" := NULL
    ) %>%
    dplyr::ungroup()

  # Returning modified data frame
  return(to_return)
}




# Second, specifying elec/heat flows from oil products, natural gas, and coal products

# specify_elect_heat_fossil_fuels <- function(.tidy_iea_df){
#
# }


# Third, specifying all elect/heat flows:


# specify_elec_heat_chp_plants <- function(.tidy_iea_df){
#
#   .tidy_iea_df %>%
#     # First, specifying PV, wind, and ...
#     specify_elect_heat_renewables() %>%
#     # Second, specifying elec, heat and CHP from oil products, natural gas, and coal products
#     specify_elect_heat_fossil_fuels() %>%
#     # Third, specifying elec, heat, and CHP from all other products
#     specify_elect_heat_other_products() %>%
#     # Returning modified data frame
#     return()
# }




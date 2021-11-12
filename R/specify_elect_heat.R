
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
                                          ratio_tidal_wave_to_input = 1,
                                          # Helper column names - removed at the end
                                          share_elect_output_From_Func = "Share_Output_Elect_From_Func",
                                          share_renewables_From_Func = "Share_Renewables_From_Func",
                                          e_dot_renewables = "E_dot_Renewables",
                                          e_dot_rest = "E_dot_Rest",
                                          negzeropos = ".netzeropos"){

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
  elect_heat_list <- c(electricity, heat)

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
      `Electricity [from Renewables]` = dplyr::case_when(
        (.data[[flow]] == main_act_prod_elect | .data[[flow]] == autoprod_elect) ~ - (.data[[hydro]] * ratio_hydro_to_input +
                                                                                        .data[[geothermal]] * ratio_geothermal_elect_to_input + .data[[solar_pv]] * ratio_solar_PV_to_input + .data[[solar_th]] * ratio_solar_thermal_elect_to_input +
                                                                                        .data[[tide_wave_ocean]] * ratio_tidal_wave_to_input + .data[[wind]] * ratio_wind_to_input),
        (.data[[flow]] == main_act_prod_chp | .data[[flow]] == autoprod_chp) ~ - .data[[geothermal]] * ratio_geothermal_elect_to_input * .data[[share_elect_output_From_Func]],

        (.data[[flow]] == main_act_prod_heat | .data[[flow]] == autoprod_heat) ~ 0
      ),
      `Heat [from Renewables]` = dplyr::case_when(
        (.data[[flow]] == main_act_prod_elect | .data[[flow]] == autoprod_elect) ~ 0,
        (.data[[flow]] == main_act_prod_chp | .data[[flow]] == autoprod_chp) ~ - .data[[geothermal]] * ratio_geothermal_heat_to_input * (1 - .data[[share_elect_output_From_Func]]),
        (.data[[flow]] == main_act_prod_heat | .data[[flow]] == autoprod_heat) ~ - (.data[[geothermal]] * ratio_geothermal_heat_to_input + .data[[solar_th]] * ratio_solar_thermal_heat_to_input)
      ),
    ) %>%
    dplyr::select(-.data[[share_elect_output_From_Func]]) %>%
    tidyr::pivot_longer(cols = c({electricity}, {heat}, {hydro}, {geothermal}, {solar_pv}, {solar_th}, {tide_wave_ocean}, {wind}, "Electricity [from Renewables]", "Heat [from Renewables]")
                        , values_to = {e_dot}, names_to = {product}) %>%
    dplyr::filter(.data[[e_dot]] != 0) %>%
    dplyr::mutate(
      "{flow}" := dplyr::case_when(
        stringr::str_detect(.data[[product]], "from Renewables") ~ "Renewable energy plants",
        .data[[product]] %in% renewable_product_list ~ "Renewable energy plants",
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
      "{share_renewables_From_Func}" := dplyr::case_when(
        (.data[[flow]] == main_act_prod_elect | .data[[flow]] == autoprod_elect) ~ - (.data[[hydro]] * ratio_hydro_to_input + .data[[geothermal]] * ratio_geothermal_elect_to_input +
                                                                                        .data[[solar_pv]] * ratio_solar_PV_to_input + .data[[solar_th]] * ratio_solar_thermal_elect_to_input +
                                                                                        .data[[tide_wave_ocean]] * ratio_tidal_wave_to_input + .data[[wind]] * ratio_wind_to_input) / .data[[electricity]] ,
        (.data[[flow]] == main_act_prod_chp | .data[[flow]] == autoprod_chp) ~ - (.data[[geothermal]] * ratio_geothermal_elect_to_input * .data[[share_elect_output_From_Func]] +
                                                                                    .data[[geothermal]] * ratio_geothermal_heat_to_input * (1 - .data[[share_elect_output_From_Func]])) / (.data[[electricity]] + .data[[heat]]),
        (.data[[flow]] == main_act_prod_heat | .data[[flow]] == autoprod_heat) ~ - (.data[[geothermal]] * ratio_geothermal_heat_to_input +
                                                                                      .data[[solar_th]] * ratio_solar_thermal_heat_to_input) / .data[[heat]],
      )
    ) %>%
    dplyr::select(-.data[[hydro]], -.data[[geothermal]], -.data[[solar_pv]], -.data[[solar_th]], -.data[[tide_wave_ocean]], -.data[[wind]],
                  -.data[[electricity]], -.data[[heat]], -.data[[flow_aggregation_point]], -.data[[share_elect_output_From_Func]])


  # Modifying EIOU flows to change the industry consuming (-> "Renewable energy plants")
  modified_eiou_flows <- .tidy_iea_df %>%
    dplyr::filter((.data[[flow_aggregation_point]] == eiou_flows) & (.data[[flow]] %in% elect_heat_producer_industries)) %>%
    dplyr::left_join(share_output_renewables, by = c({country}, {year}, {last_stage}, {energy_type}, {method}, {unit}, {ledger_side}, {flow})) %>%
    dplyr::mutate(
      "{e_dot_renewables}" := .data[[e_dot]] * .data[[share_renewables_From_Func]],
      "{e_dot_rest}" := .data[[e_dot]] * (1 - .data[[share_renewables_From_Func]])
    ) %>%
    dplyr::select(-.data[[e_dot]], -.data[[share_renewables_From_Func]]) %>%
    tidyr::pivot_longer(cols = c({e_dot_renewables}, {e_dot_rest}), names_to = "Renewables", values_to = {e_dot}) %>%
    dplyr::mutate(
      "{flow}" := dplyr::case_when(
        .data[["Renewables"]] == e_dot_renewables ~ "Renewable energy plants",
        TRUE ~ .data[[flow]]
      )
    ) %>%
    dplyr::select(-.data[["Renewables"]])


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
    dplyr::filter(.data[[e_dot]] != 0) %>%
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

specify_elect_heat_fossil_fuels <- function(.tidy_iea_df,
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
                                            # Helper product type names
                                            oil_products = "Oil products",
                                            coal_products = "Coal products",
                                            natural_gas = "Natural gas",
                                            other_products = "Other products",
                                            # Helper column names
                                            product_type = "Product type",
                                            share_inputs_from_Func = "Share_inputs_from_Func",
                                            negzeropos = ".negzeropos"){

  # Defining list of industries
  elect_heat_producer_industries <- c(main_act_prod_elect, main_act_prod_chp, main_act_prod_heat, autoprod_elect, autoprod_chp, autoprod_heat)

  # First step; figuring out input shares for each elect, heat, and CHP plants, by product type:
  share_inputs_intermediary_data <- .tidy_iea_df %>%
    dplyr::filter(
      .data[[flow_aggregation_point]] == transformation_processes & (.data[[flow]] %in% elect_heat_producer_industries)
    ) %>%
    dplyr::filter(.data[[e_dot]] < 0) %>%
    dplyr::mutate(
      "{product_type}" := dplyr::case_when(
        .data[[product]] %in% IEATools::oil_and_oil_products ~ oil_products,
        .data[[product]] %in% IEATools::coal_and_coal_products ~ coal_products,
        .data[[product]] == natural_gas ~ natural_gas,
        TRUE ~ other_products
      )
    ) %>%
    dplyr::group_by(.data[[country]], .data[[year]], .data[[last_stage]], .data[[energy_type]], .data[[method]], .data[[flow]], .data[[product_type]]) %>%
    dplyr::summarise(
      "{e_dot}" := sum(.data[[e_dot]])
    ) %>%
    dplyr::group_by(.data[[country]], .data[[year]], .data[[last_stage]], .data[[energy_type]], .data[[method]], .data[[flow]]) %>%
    dplyr::mutate(
      "{share_inputs_from_Func}" := .data[[e_dot]] / sum(.data[[e_dot]])
    ) %>%
    dplyr::select(-.data[[e_dot]])

  # Second step, changing input flows so they now flow to the appropriate industry
  input_flows_modified <- .tidy_iea_df %>%
    dplyr::filter(
      .data[[flow_aggregation_point]] == transformation_processes & .data[[flow]] %in% elect_heat_producer_industries
    ) %>%
    dplyr::filter(.data[[e_dot]] < 0) %>%
    dplyr::mutate(
      "{flow}" := dplyr::case_when(
        .data[[product]] %in% IEATools::oil_and_oil_products ~ stringr::str_c(.data[[flow]], " [from Oil products]"),
        .data[[product]] %in% IEATools::coal_and_coal_products ~ stringr::str_c(.data[[flow]], " [from Coal products]"),
        .data[[product]] == natural_gas ~ stringr::str_c(.data[[flow]], " [from Natural gas]"),
        TRUE ~ .data[[flow]]
      )
    )

  # Third step, changing output flows as function of input shares
  output_flows_modified <- .tidy_iea_df %>%
    dplyr::filter(
      .data[[flow_aggregation_point]] == transformation_processes & .data[[flow]] %in% elect_heat_producer_industries
    ) %>%
    dplyr::filter(.data[[e_dot]] > 0) %>%
    dplyr::left_join(share_inputs_intermediary_data, by = c({country}, {year}, {last_stage}, {method}, {energy_type}, {flow})) %>%
    dplyr::mutate(
      "{e_dot}" := .data[[e_dot]] * .data[[share_inputs_from_Func]],
      "{flow}" := dplyr::case_when(
        .data[[product_type]] == oil_products ~ stringr::str_c(.data[[flow]], " [from Oil products]"),
        .data[[product_type]] == coal_products ~ stringr::str_c(.data[[flow]], " [from Coal products]"),
        .data[[product_type]] == natural_gas ~ stringr::str_c(.data[[flow]], " [from Natural gas]"),
        TRUE ~ .data[[flow]]
      ),
      "{product}" := dplyr::case_when(
        .data[[product_type]] == oil_products ~ stringr::str_c(.data[[product]], " [from Oil products]"),
        .data[[product_type]] == coal_products ~ stringr::str_c(.data[[product]], " [from Coal products]"),
        .data[[product_type]] == natural_gas ~ stringr::str_c(.data[[product]], " [from Natural gas]"),
        TRUE ~ .data[[product]]
      )
    ) %>%
    dplyr::select(-.data[[share_inputs_from_Func]])


  # Fourth step, changing EIOU flows as function of input shares
  eiou_flows_modified <- .tidy_iea_df %>%
    dplyr::filter(
      .data[[flow_aggregation_point]] == eiou_flows & .data[[flow]] %in% elect_heat_producer_industries
    ) %>%
    dplyr::left_join(share_inputs_intermediary_data, by = c({country}, {year}, {last_stage}, {method}, {energy_type}, {flow})) %>%
    dplyr::mutate(
      "{e_dot}" := .data[[e_dot]] * .data[[share_inputs_from_Func]],
      "{flow}" := dplyr::case_when(
        .data[[product_type]] == oil_products ~ stringr::str_c(.data[[flow]], " [from Oil products]"),
        .data[[product_type]] == coal_products ~ stringr::str_c(.data[[flow]], " [from Coal products]"),
        .data[[product_type]] == natural_gas ~ stringr::str_c(.data[[flow]], " [from Natural gas]"),
        TRUE ~ .data[[flow]]
      )
    ) %>%
    dplyr::select(-.data[[share_inputs_from_Func]])


  # Fifth, filtering out, binding, and returning modified data frame
  .tidy_iea_df %>%
    # Taking out input and output flows from relevant TPs
    dplyr::filter(
      ! (.data[[flow_aggregation_point]] == transformation_processes & .data[[flow]] %in% elect_heat_producer_industries)
    ) %>%
    # Taking out eiou flows from relevant TPs
    dplyr::filter(
      ! (.data[[flow_aggregation_point]] == eiou_flows & .data[[flow]] %in% elect_heat_producer_industries)
    ) %>%
    # Then binding modified rows
    dplyr::bind_rows(input_flows_modified) %>%
    dplyr::bind_rows(output_flows_modified) %>%
    dplyr::bind_rows(eiou_flows_modified) %>%
    dplyr::select(-.data[[product_type]]) %>%
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
    dplyr::ungroup() %>%
    return()
}


# Third, specifying  elect and heat EIOU flows

specify_elect_heat_eiou_flows <- function(.tidy_iea_df,
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
                                          # Elect and heat producing industries
                                          main_act_prod_elect = IEATools::main_act_plants$main_act_prod_elect_plants,
                                          main_act_prod_chp = IEATools::main_act_plants$main_act_prod_chp_plants,
                                          main_act_prod_heat = IEATools::main_act_plants$main_act_prod_heat_plants,
                                          autoprod_elect = "Autoproducer electricity plants",
                                          autoprod_chp = "Autoproducer CHP plants",
                                          autoprod_heat = "Autoproducer heat plants",
                                          # Helper column names, removed after:
                                          product_origin = "Product_origin",
                                          share_electricity_by_origin = "Share_electricity_by_origin",
                                          share_heat_by_origin = "Share_heat_by_origin",
                                          negzeropos = ".negzeropos"){

  # Defining industries lists:
  elect_heat_prod_industries <- c(main_act_prod_elect, main_act_prod_chp, main_act_prod_heat, autoprod_elect, autoprod_chp, autoprod_heat)
  electricity_prod_industries <- c(main_act_prod_elect, autoprod_elect, main_act_prod_chp, autoprod_chp)
  heat_prod_industries <- c(main_act_prod_chp, autoprod_chp, main_act_prod_heat, autoprod_heat)

  # Figuring out share of each energy source type electricity output
  share_electricity_output <- .tidy_iea_df %>%
    dplyr::filter(
      .data[[flow_aggregation_point]] == transformation_processes & .data[[flow]] %in% elect_heat_prod_industries
    ) %>%
    dplyr::filter(
      stringr::str_detect(.data[[product]], "Electricity")
    ) %>%
    dplyr::filter(.data[[e_dot]] > 0) %>%
    dplyr::mutate(
      "{product_origin}" := dplyr::case_when(
        .data[[product]] == "Electricity [from Oil products]" ~ "Oil products",
        .data[[product]] == "Electricity [from Coal products]" ~ "Coal products",
        .data[[product]] == "Electricity [from Natural gas]" ~ "Natural gas",
        .data[[product]] == "Electricity [from Renewables]" ~ "Renewables",
        TRUE ~ .data[[product]]
      )
    ) %>%
    dplyr::group_by(.data[[country]], .data[[year]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[product_origin]]) %>%
    dplyr::summarise(
      "{e_dot}" := sum(.data[[e_dot]])
    ) %>%
    dplyr::group_by(.data[[country]], .data[[year]], .data[[method]], .data[[energy_type]], .data[[last_stage]]) %>%
    dplyr::mutate(
      "{share_electricity_by_origin}" := .data[[e_dot]] / sum(.data[[e_dot]])
    ) %>%
    dplyr::select(-.data[[e_dot]])


  # Figuring out share of each energy source type heat output
  share_heat_output <- .tidy_iea_df %>%
    dplyr::filter(
      .data[[flow_aggregation_point]] == transformation_processes & .data[[flow]] %in% elect_heat_prod_industries
    ) %>%
    dplyr::filter(
      stringr::str_detect(.data[[product]], "Heat")
    ) %>%
    dplyr::filter(.data[[e_dot]] > 0) %>%
    dplyr::mutate(
      "{product_origin}" := dplyr::case_when(
        .data[[product]] == "Heat [from Oil products]" ~ "Oil products",
        .data[[product]] == "Heat [from Coal products]" ~ "Coal products",
        .data[[product]] == "Heat [from Natural gas]" ~ "Natural gas",
        .data[[product]] == "Heat [from Renewables]" ~ "Renewables",
        TRUE ~ .data[[product]]
      )
    ) %>%
    dplyr::group_by(.data[[country]], .data[[year]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[product_origin]]) %>%
    dplyr::summarise(
      "{e_dot}" := sum(.data[[e_dot]])
    ) %>%
    dplyr::group_by(.data[[country]], .data[[year]], .data[[method]], .data[[energy_type]], .data[[last_stage]]) %>%
    dplyr::mutate(
      "{share_heat_by_origin}" := .data[[e_dot]] / sum(.data[[e_dot]])
    ) %>%
    dplyr::select(-.data[[e_dot]])

  # Modifying electricity EIOU flows:
  modified_elec_eiou_flows <- .tidy_iea_df %>%
    dplyr::filter(
      (.data[[flow_aggregation_point]] == eiou_flows) & (.data[[product]] == "Electricity")
    ) %>%
    dplyr::left_join(
      share_electricity_output, by = c({country}, {year}, {method}, {last_stage}, {energy_type})
    ) %>%
    dplyr::mutate(
      "{e_dot}" := .data[[e_dot]] * .data[[share_electricity_by_origin]],
      "{product}" := dplyr::case_when(
        .data[[product_origin]] == "Oil products" ~ stringr::str_c(.data[[product]], " [from Oil products]"),
        .data[[product_origin]] == "Coal products" ~ stringr::str_c(.data[[product]], " [from Coal products]"),
        .data[[product_origin]] == "Natural gas" ~ stringr::str_c(.data[[product]], " [from Natural gas]"),
        .data[[product_origin]] == "Renewables" ~ stringr::str_c(.data[[product]], " [from Renewables]"),
        TRUE ~ .data[[product]]
      )
    ) %>%
    dplyr::select(-.data[[product_origin]], -.data[[share_electricity_by_origin]])

  # Modifying heat EIOU flows:
  modified_heat_eiou_flows <- .tidy_iea_df %>%
    dplyr::filter(
      (.data[[flow_aggregation_point]] == eiou_flows) & (.data[[product]] == "Heat")
    ) %>%
    dplyr::left_join(
      share_heat_output, by = c({country}, {year}, {method}, {last_stage}, {energy_type})
    ) %>%
    dplyr::mutate(
      "{e_dot}" := .data[[e_dot]] * .data[[share_heat_by_origin]],
      "{product}" := dplyr::case_when(
        .data[[product_origin]] == "Oil products" ~ stringr::str_c(.data[[product]], " [from Oil products]"),
        .data[[product_origin]] == "Coal products" ~ stringr::str_c(.data[[product]], " [from Coal products]"),
        .data[[product_origin]] == "Natural gas" ~ stringr::str_c(.data[[product]], " [from Natural gas]"),
        .data[[product_origin]] == "Renewables" ~ stringr::str_c(.data[[product]], " [from Renewables]"),
        TRUE ~ .data[[product]]
      )
    ) %>%
    dplyr::select(-.data[[product_origin]], -.data[[share_heat_by_origin]])


  # Filter out relevant flows, binding modified flows, and doing the negzeropos trick:
  .tidy_iea_df %>%
    # FILTER OUT MODIFIED FLOWS
    dplyr::filter(
      ! ((.data[[flow_aggregation_point]] == eiou_flows) & (.data[[product]] == "Electricity"))
    ) %>%
    dplyr::filter(
      ! ((.data[[flow_aggregation_point]] == eiou_flows) & (.data[[product]] == "Heat"))
    ) %>%
    # BIND NEWLY MODIFIED FLOWS
    dplyr::bind_rows(modified_elec_eiou_flows) %>%
    dplyr::bind_rows(modified_heat_eiou_flows) %>%
    # NEGZEROPOS TRICK
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
    dplyr::ungroup() %>%
    return()
}


# Fourth, specifying all elect/heat flows:


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




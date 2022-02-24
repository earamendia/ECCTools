
#' Specifies electricity and heat produced by renewable energy
#'
#' This code selects main activity autoproducer electricity, heat and CHP plants,
#' and figures out which fraction of the output is due to renewable energy, based on the conversion factors
#' of energy output to energy input provided by the IEA's WEEB.
#' The energy due to renewable energy is then subtracted from the main activity and autoproducer elect, heat and CHP plants,
#' and the input and output flows due to renewable energy are directed to a new industry called "Renewable energy plants",
#' which now produce "Electricity `[`from Renewables`]`" and "Heat `[`from Renewables`]`".
#'
#' @param .tidy_iea_df The `.tidy_iea_df` for which electricity and heat products need to be specified
#'                     when they come from renewable energy. Default is `.tidy_iea_df`.
#' @param country,method,energy_type,last_stage,year,ledger_side,flow_aggregation_point,flow,product,e_dot,unit
#'        See `IEATools::iea_cols`.
#' @param transformation_processes Name of transformation process flows in data frame.
#'                                 Default is IEATools::aggregation_flows$transformation_processes.
#' @param eiou_flows Name of energy industry own use flows in data frame.
#'                   Default is IEATools::aggregation_flows$energy_industry_own_use.
#' @param main_act_prod_elect Name of electricity producing plants.
#'                            Default is IEATools::main_act_plants$main_act_prod_elect_plants.
#' @param main_act_prod_chp Name of CHP producing plants.
#'                            Default is IEATools::main_act_plants$main_act_prod_chp_plants
#' @param main_act_prod_heat Name of heat producing plants.
#'                            Default is IEATools::main_act_plants$main_act_prod_heat_plants
#' @param autoprod_elect Name of autoproducer electricity plants.
#'                       Default is "Autoproducer electricity plants".
#' @param autoprod_chp Name of autoproducer CHP plants.
#'                       Default is "Autoproducer CHP plants".
#' @param autoprod_heat Name of autoproducer heat plants.
#'                       Default is "Autoproducer heat plants".
#' @param hydro Temporary column name. Default is "Hydro".
#' @param geothermal Temporary column name. Default is "Geothermal".
#' @param solar_pv Temporary column name. Default is "Solar photovoltaics".
#' @param solar_th Temporary column name. Default is "Solar thermal".
#' @param tide_wave_ocean Temporary column name. Default is "Tide, wave and ocean".
#' @param wind Temporary column name. Default is "Wind".
#' @param electricity Temporary column name. Default is "Electricity".
#' @param heat Temporary column name. Default is "Heat".
#' @param ratio_hydro_to_input Ratio of input hydro to output electricity.
#'                             Default is 1, value comes from IEA's WEEB.
#' @param ratio_solar_PV_to_input Ratio of output electricity to input solar PV.
#'                             Default is 1, value comes from IEA's WEEB.
#' @param ratio_solar_thermal_elect_to_input Ratio of output electricity to input solar thermal.
#'                             Default is 0.33, value comes from IEA's WEEB.
#' @param ratio_solar_thermal_heat_to_input Ratio of output heat to input solar thermal.
#'                             Default is 1, value comes from IEA's WEEB.
#' @param ratio_wind_to_input Ratio of output electricity to input wind.
#'                             Default is 1, value comes from IEA's WEEB.
#' @param ratio_geothermal_elect_to_input Ratio of output electricity to input geothermal.
#'                             Default is 0.1, value comes from IEA's WEEB.
#' @param ratio_geothermal_heat_to_input Ratio of output heat to input geothermal.
#'                             Default is 0.5, value comes from IEA's WEEB.
#' @param ratio_tidal_wave_to_input Ratio of output electricity to input tide, wave, ocean.
#'                             Default is 1, value comes from IEA's WEEB.
#' @param share_elect_output_From_Func Temporary column name. Default is "Share_Output_Elect_From_Func".
#' @param share_renewables_From_Func Temporary column name. Default is "Share_Renewables_From_Func".
#' @param e_dot_renewables Temporary column name. Default is "E_dot_Renewables".
#' @param e_dot_rest Temporary column name. Default is "E_dot_Rest".
#' @param negzeropos Temporary column name. Default is ".netzeropos".
#'
#' @return A `.tidy_iea_df` with electricity and heat products specified when they come from renewable energy.
#' @export
#'
#' @examples
#' A_B_path <- system.file("extdata/A_B_data_full_2018_format.csv", package = "ECCTools")
#' IEATools::load_tidy_iea_df(A_B_path) %>%
#' IEATools::specify_all() %>%
#' specify_elect_heat_renewables()
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




#' Specifies electricity and heat products
#'
#' This function specified the electricity and heat energy products by the type of energy carrier
#' that has been used to produce them, at the moment "Oil products", "Coal products", "Natural gas", and "Other products".
#' To do so, the code keeps the main activity and autoproducer electricity, heat and CHP activities,
#' and calculates the inputs shares by each of the four product types group. The same shares are ascribe to electricity and heat outputs.
#' New industries that are now specified as function of the product type they take as input.
#'
#' @param .tidy_iea_df Name of the `.tidy_iea_df` for which electricity and heat products
#'                     need to be specified with origin product type.
#' @param country,method,energy_type,last_stage,year,ledger_side,flow_aggregation_point,flow,product,e_dot,unit
#'        See `IEATools::iea_cols`.
#' @param transformation_processes Name of transformation process flows in data frame.
#'                                 Default is IEATools::aggregation_flows$transformation_processes.
#' @param eiou_flows Name of energy industry own use flows in data frame.
#'                   Default is IEATools::aggregation_flows$energy_industry_own_use.
#' @param main_act_prod_elect Name of electricity producing plants.
#'                            Default is IEATools::main_act_plants$main_act_prod_elect_plants.
#' @param main_act_prod_chp Name of CHP producing plants.
#'                            Default is IEATools::main_act_plants$main_act_prod_chp_plants
#' @param main_act_prod_heat Name of heat producing plants.
#'                            Default is IEATools::main_act_plants$main_act_prod_heat_plants
#' @param autoprod_elect Name of autoproducer electricity plants.
#'                       Default is "Autoproducer electricity plants".
#' @param autoprod_chp Name of autoproducer CHP plants.
#'                       Default is "Autoproducer CHP plants".
#' @param autoprod_heat Name of autoproducer heat plants.
#'                       Default is "Autoproducer heat plants".
#' @param oil_products Name of oil products. Default is "Oil products".
#' @param coal_products Name of coal products. Default is "Coal products".
#' @param natural_gas Name of natural gas. Default is "Natural gas".
#' @param other_products Name of other products. Default is "Other products".
#' @param product_type Name of a temporary column. Default is "Product type".
#' @param share_inputs_from_Func Name of a temporary column. Default is "Share_inputs_from_Func".
#' @param negzeropos Temporary column name. Default is ".netzeropos".
#'
#' @return Returns a `.tidy_iea_df` with electricity and heat products specified
#'         with origin product type.
#' @export
#'
#' @examples
#' A_B_path <- system.file("extdata/A_B_data_full_2018_format.csv", package = "ECCTools")
#' IEATools::load_tidy_iea_df(A_B_path) %>%
#' IEATools::specify_all() %>%
#' specify_elect_heat_fossil_fuels()
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
        TRUE ~ stringr::str_c(.data[[flow]], " [from Other products]")
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
      "{share_inputs_from_Func}" := dplyr::case_when(
        is.na(.data[[share_inputs_from_Func]]) ~ 1,
        TRUE ~ .data[[share_inputs_from_Func]]
      ),
      "{product_type}" := dplyr::case_when(
        is.na(.data[[product_type]]) ~ "Oil products",
        TRUE ~ .data[[product_type]]
      )
    ) %>%
    dplyr::mutate(
      "{e_dot}" := .data[[e_dot]] * .data[[share_inputs_from_Func]],
      "{flow}" := dplyr::case_when(
        .data[[product_type]] == oil_products ~ stringr::str_c(.data[[flow]], " [from Oil products]"),
        .data[[product_type]] == coal_products ~ stringr::str_c(.data[[flow]], " [from Coal products]"),
        .data[[product_type]] == natural_gas ~ stringr::str_c(.data[[flow]], " [from Natural gas]"),
        TRUE ~ stringr::str_c(.data[[flow]], " [from Other products]")
      ),
      "{product}" := dplyr::case_when(
        .data[[product_type]] == oil_products ~ stringr::str_c(.data[[product]], " [from Oil products]"),
        .data[[product_type]] == coal_products ~ stringr::str_c(.data[[product]], " [from Coal products]"),
        .data[[product_type]] == natural_gas ~ stringr::str_c(.data[[product]], " [from Natural gas]"),
        TRUE ~ stringr::str_c(.data[[product]], " [from Other products]")
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
      "{share_inputs_from_Func}" := dplyr::case_when(
        is.na(.data[[share_inputs_from_Func]]) ~ 1,
        TRUE ~ .data[[share_inputs_from_Func]]
      ),
      "{product_type}" := dplyr::case_when(
        is.na(.data[[product_type]]) ~ "Oil products",
        TRUE ~ .data[[product_type]]
      )
    ) %>%
    dplyr::mutate(
      "{e_dot}" := .data[[e_dot]] * .data[[share_inputs_from_Func]],
      "{flow}" := dplyr::case_when(
        .data[[product_type]] == oil_products ~ stringr::str_c(.data[[flow]], " [from Oil products]"),
        .data[[product_type]] == coal_products ~ stringr::str_c(.data[[flow]], " [from Coal products]"),
        .data[[product_type]] == natural_gas ~ stringr::str_c(.data[[flow]], " [from Natural gas]"),
        TRUE ~ stringr::str_c(.data[[flow]], " [from Other products]")
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


#' Specifies electricity and heat from nuclear
#'
#' This function selects electricity and heat produced by the nuclear industry, and specifies them, so that the new product name is
#' respectively "Electricity `[`from Nuclear`]`" and "Heat `[`from Nuclear`]`".
#'
#' @param .tidy_iea_df The `.tidy_iea_df` for which electricity and heat products coming from the nuclear industry need to be specified.
#' @param flow_aggregation_point,flow,e_dot,product See `IEATools::iea_cols`.
#' @param transformation_processes The name of transformation processes in the data frame.
#'                                 Default is `IEATools::aggregation_flows$transformation_processes`.
#' @param nuclear_industry The name of the nuclear industry in the flow column of the data frame.
#'                         Default is `IEATools::eiou_flows$nuclear_industry`.
#' @param negzeropos Temporary column name. Default is ".netzeropos".
#'
#' @return A `.tidy_iea_df` with electricity and heat products specified when they come from the nuclear industry.
#' @export
#'
#' @examples
#' A_B_path <- system.file("extdata/A_B_data_full_2018_format.csv", package = "ECCTools")
#' IEATools::load_tidy_iea_df(A_B_path) %>%
#' IEATools::specify_all() %>%
#' specify_elect_heat_nuclear()
specify_elect_heat_nuclear <- function(.tidy_iea_df,
                                       flow_aggregation_point = IEATools::iea_cols$flow_aggregation_point,
                                       flow = IEATools::iea_cols$flow,
                                       e_dot = IEATools::iea_cols$e_dot,
                                       product = IEATools::iea_cols$product,
                                       transformation_processes = IEATools::aggregation_flows$transformation_processes,
                                       nuclear_industry = IEATools::eiou_flows$nuclear_industry,
                                       negzeropos = ".negzeropos"){

  modified_nuclear_output <- .tidy_iea_df %>%
    dplyr::filter(.data[[flow_aggregation_point]] == transformation_processes) %>%
    dplyr::filter(.data[[flow]] == nuclear_industry) %>%
    dplyr::filter(.data[[e_dot]] > 0) %>%
    dplyr::filter(.data[[product]] == "Electricity" | .data[[product]] == "Heat") %>%
    dplyr::mutate(
      "{product}" := stringr::str_c(.data[[product]], " [from Nuclear]")
    )

  # Returning values:
  .tidy_iea_df %>%
    # Exclude elec and heat output flows from nuclear activities
    dplyr::filter(! ((.data[[flow_aggregation_point]] == transformation_processes) & (.data[[flow]] == nuclear_industry) &
                       (.data[[e_dot]] > 0) & (.data[[product]] == "Electricity" | .data[[product]] == "Heat"))) %>%
    dplyr::bind_rows(modified_nuclear_output) %>%
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



#' Title
#'
#' @param .tidy_iea_df
#' @param flow_aggregation_point
#' @param flow
#' @param e_dot
#' @param product
#' @param transformation_processes
#' @param negzeropos
#'
#' @return
#' @export
#'
#' @examples
specify_other_elec_heat_production <- function(.tidy_iea_df,
                                               flow_aggregation_point = IEATools::iea_cols$flow_aggregation_point,
                                               flow = IEATools::iea_cols$flow,
                                               e_dot = IEATools::iea_cols$e_dot,
                                               product = IEATools::iea_cols$product,
                                               transformation_processes = IEATools::aggregation_flows$transformation_processes,
                                               negzeropos = ".negzeropos"){

  modified_production_flows <- .tidy_iea_df %>%
  dplyr::filter(.data[[product]] == "Electricity" | .data[[product]] == "Heat") %>%
  dplyr::filter(.data[[flow_aggregation_point]] == transformation_processes) %>%
  dplyr::filter(.data[[e_dot]] > 0) %>%
  dplyr::mutate(
    "{product}" := stringr::str_c(.data[[product]], " [from Other processes]")
  )

  .tidy_iea_df %>%
    dplyr::filter(! ((.data[[product]] == "Electricity" | .data[[product]] == "Heat") & (.data[[flow_aggregation_point]] == transformation_processes) & (.data[[e_dot]] > 0))) %>%
    dplyr::bind_rows(modified_production_flows) %>%
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
}



#' Specifies electricity and heat markets
#'
#' This function specifies electricity and heat markets. See details for more information.
#'
#' This function specifies electricity and heat markets by selecting production flows (V matrix) of:
#' "Electricity `[`from Oil products`]`", "Electricity `[`from Coal products`]`", "Electricity `[`from Natural gas`]`",
#' "Electricity `[`from Other products`]`", "Electricity `[`from Renewables`]`", and "Electricity `[`from Nuclear`]`.
#' and routing them as inputs to a new industry: "Electricity market". The electricity market industry then produces
#' "Electricity" in the same amount that it receives as input.
#'
#' Exactly the same process is conducted for heat.
#'
#' @param .tidy_iea_df The `.tidy_iea_df` for which electricity and heat markets need to be specified.
#' @param country,method,energy_type,last_stage,year,ledger_side,flow_aggregation_point,flow,product,e_dot,unit
#'        See `IEATools::iea_cols`.
#' @param transformation_processes The name of transformation processes in the data frame.
#'                                 Default is IEATools::aggregation_flows$transformation_processes.
#' @param negzeropos Temporary column name. Default is ".netzeropos".
#'
#' @return Returns a `.tidy_iea_df` with specified electricity and heat markets.
#' @export
#'
#' @examples
#' A_B_path <- system.file("extdata/A_B_data_full_2018_format.csv", package = "ECCTools")
#' IEATools::load_tidy_iea_df(A_B_path) %>%
#' IEATools::specify_all() %>%
#' specify_elect_heat_markets()
specify_elect_heat_markets <- function(.tidy_iea_df,
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
                                       # Helper column names
                                       negzeropos = ".negzeropos"){


  # Sorting out input flows to the electricity market
  input_electricity_market_flows <- .tidy_iea_df %>%
    dplyr::filter(.data[[flow_aggregation_point]] == transformation_processes) %>%
    dplyr::filter(.data[[e_dot]] > 0) %>%
    dplyr::filter(.data[[product]] %in% c("Electricity [from Oil products]", "Electricity [from Coal products]",
                                          "Electricity [from Natural gas]", "Electricity [from Other products]",
                                          "Electricity [from Renewables]", "Electricity [from Nuclear]",
                                          "Electricity [from Other processes]")) %>%
    dplyr::mutate(
      "{flow}" := "Electricity market",
      "{e_dot}" := -.data[[e_dot]]
    )

  # Sorting out output flows to the electricity market
  output_electricity_market_flows <- .tidy_iea_df %>%
    dplyr::filter(.data[[flow_aggregation_point]] == transformation_processes) %>%
    dplyr::filter(.data[[e_dot]] > 0) %>%
    dplyr::filter(.data[[product]] %in% c("Electricity [from Oil products]", "Electricity [from Coal products]",
                                          "Electricity [from Natural gas]", "Electricity [from Other products]",
                                          "Electricity [from Renewables]", "Electricity [from Nuclear]",
                                          "Electricity [from Other processes]")) %>%
    dplyr::mutate(
      "{flow}" := "Electricity market",
      "{product}" := "Electricity"
    )

  # Sorting out input flows to the heat market
  input_heat_market_flows <- .tidy_iea_df %>%
    dplyr::filter(.data[[flow_aggregation_point]] == transformation_processes) %>%
    dplyr::filter(.data[[e_dot]] > 0) %>%
    dplyr::filter(.data[[product]] %in% c("Heat [from Oil products]", "Heat [from Coal products]",
                                          "Heat [from Natural gas]", "Heat [from Other products]",
                                          "Heat [from Renewables]", "Heat [from Nuclear]",
                                          "Heat [from Other processes]")) %>%
    dplyr::mutate(
      "{flow}" := "Heat market",
      "{e_dot}" := -.data[[e_dot]]
    )

  # Sorting out output flows to the heat market
  output_heat_market_flows <- .tidy_iea_df %>%
    dplyr::filter(.data[[flow_aggregation_point]] == transformation_processes) %>%
    dplyr::filter(.data[[e_dot]] > 0) %>%
    dplyr::filter(.data[[product]] %in% c("Heat [from Oil products]", "Heat [from Coal products]",
                                          "Heat [from Natural gas]", "Heat [from Other products]",
                                          "Heat [from Renewables]", "Heat [from Nuclear]",
                                          "Heat [from Other processes]")) %>%
    dplyr::mutate(
      "{flow}" := "Heat market",
      "{product}" := "Heat"
    )

  # Returning results
  .tidy_iea_df %>%
    dplyr::bind_rows(input_electricity_market_flows) %>%
    dplyr::bind_rows(output_electricity_market_flows) %>%
    dplyr::bind_rows(input_heat_market_flows) %>%
    dplyr::bind_rows(output_heat_market_flows) %>%
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


# Fourth, specifying  elect and heat EIOU flows

# specify_elect_heat_eiou_flows <- function(.tidy_iea_df,
#                                           country = IEATools::iea_cols$country,
#                                           method = IEATools::iea_cols$method,
#                                           energy_type = IEATools::iea_cols$energy_type,
#                                           last_stage = IEATools::iea_cols$last_stage,
#                                           year = IEATools::iea_cols$year,
#                                           ledger_side = IEATools::iea_cols$ledger_side,
#                                           flow_aggregation_point = IEATools::iea_cols$flow_aggregation_point,
#                                           flow = IEATools::iea_cols$flow,
#                                           product = IEATools::iea_cols$product,
#                                           e_dot = IEATools::iea_cols$e_dot,
#                                           unit = IEATools::iea_cols$unit,
#                                           # Flow aggregation points
#                                           transformation_processes = IEATools::aggregation_flows$transformation_processes,
#                                           eiou_flows = IEATools::aggregation_flows$energy_industry_own_use,
#                                           # Elect and heat producing industries
#                                           main_act_prod_elect = IEATools::main_act_plants$main_act_prod_elect_plants,
#                                           main_act_prod_chp = IEATools::main_act_plants$main_act_prod_chp_plants,
#                                           main_act_prod_heat = IEATools::main_act_plants$main_act_prod_heat_plants,
#                                           autoprod_elect = "Autoproducer electricity plants",
#                                           autoprod_chp = "Autoproducer CHP plants",
#                                           autoprod_heat = "Autoproducer heat plants",
#                                           # Helper column names, removed after:
#                                           product_origin = "Product_origin",
#                                           share_electricity_by_origin = "Share_electricity_by_origin",
#                                           share_heat_by_origin = "Share_heat_by_origin",
#                                           negzeropos = ".negzeropos"){
#
#   # Defining industries lists:
#   #elect_heat_prod_industries <- c(main_act_prod_elect, main_act_prod_chp, main_act_prod_heat, autoprod_elect, autoprod_chp, autoprod_heat)
#   #electricity_prod_industries <- c(main_act_prod_elect, autoprod_elect, main_act_prod_chp, autoprod_chp)
#   #heat_prod_industries <- c(main_act_prod_chp, autoprod_chp, main_act_prod_heat, autoprod_heat)
#
#   # Figuring out share of each energy source type electricity output
#   share_electricity_output <- .tidy_iea_df %>%
#     dplyr::filter(
#       .data[[flow_aggregation_point]] == transformation_processes
#     ) %>%
#     dplyr::filter(
#       stringr::str_detect(.data[[product]], "Electricity")
#     ) %>%
#     dplyr::filter(.data[[e_dot]] > 0) %>%
#     dplyr::mutate(
#       "{product_origin}" := dplyr::case_when(
#         .data[[product]] == "Electricity [from Oil products]" ~ "Oil products",
#         .data[[product]] == "Electricity [from Coal products]" ~ "Coal products",
#         .data[[product]] == "Electricity [from Natural gas]" ~ "Natural gas",
#         .data[[product]] == "Electricity [from Renewables]" ~ "Renewables",
#         TRUE ~ .data[[product]]
#       )
#     ) %>%
#     dplyr::group_by(.data[[country]], .data[[year]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[product_origin]]) %>%
#     dplyr::summarise(
#       "{e_dot}" := sum(.data[[e_dot]])
#     ) %>%
#     dplyr::group_by(.data[[country]], .data[[year]], .data[[method]], .data[[energy_type]], .data[[last_stage]]) %>%
#     dplyr::mutate(
#       "{share_electricity_by_origin}" := .data[[e_dot]] / sum(.data[[e_dot]])
#     ) %>%
#     dplyr::select(-.data[[e_dot]])
#
#
#   # Figuring out share of each energy source type heat output
#   share_heat_output <- .tidy_iea_df %>%
#     dplyr::filter(
#       .data[[flow_aggregation_point]] == transformation_processes
#     ) %>%
#     dplyr::filter(
#       stringr::str_detect(.data[[product]], "Heat")
#     ) %>%
#     dplyr::filter(.data[[e_dot]] > 0) %>%
#     dplyr::mutate(
#       "{product_origin}" := dplyr::case_when(
#         .data[[product]] == "Heat [from Oil products]" ~ "Oil products",
#         .data[[product]] == "Heat [from Coal products]" ~ "Coal products",
#         .data[[product]] == "Heat [from Natural gas]" ~ "Natural gas",
#         .data[[product]] == "Heat [from Renewables]" ~ "Renewables",
#         TRUE ~ .data[[product]]
#       )
#     ) %>%
#     dplyr::group_by(.data[[country]], .data[[year]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[product_origin]]) %>%
#     dplyr::summarise(
#       "{e_dot}" := sum(.data[[e_dot]])
#     ) %>%
#     dplyr::group_by(.data[[country]], .data[[year]], .data[[method]], .data[[energy_type]], .data[[last_stage]]) %>%
#     dplyr::mutate(
#       "{share_heat_by_origin}" := .data[[e_dot]] / sum(.data[[e_dot]])
#     ) %>%
#     dplyr::select(-.data[[e_dot]])
#
#   # Modifying electricity EIOU flows:
#   modified_elec_eiou_flows <- .tidy_iea_df %>%
#     dplyr::filter(
#       (.data[[flow_aggregation_point]] == eiou_flows) & (.data[[product]] == "Electricity")
#     ) %>%
#     dplyr::left_join(
#       share_electricity_output, by = c({country}, {year}, {method}, {last_stage}, {energy_type})
#     ) %>%
#     dplyr::mutate(
#       "{e_dot}" := .data[[e_dot]] * .data[[share_electricity_by_origin]],
#       "{product}" := dplyr::case_when(
#         .data[[product_origin]] == "Oil products" ~ stringr::str_c(.data[[product]], " [from Oil products]"),
#         .data[[product_origin]] == "Coal products" ~ stringr::str_c(.data[[product]], " [from Coal products]"),
#         .data[[product_origin]] == "Natural gas" ~ stringr::str_c(.data[[product]], " [from Natural gas]"),
#         .data[[product_origin]] == "Renewables" ~ stringr::str_c(.data[[product]], " [from Renewables]"),
#         TRUE ~ .data[[product]]
#       )
#     ) %>%
#     dplyr::select(-.data[[product_origin]], -.data[[share_electricity_by_origin]])
#
#   # Modifying heat EIOU flows:
#   modified_heat_eiou_flows <- .tidy_iea_df %>%
#     dplyr::filter(
#       (.data[[flow_aggregation_point]] == eiou_flows) & (.data[[product]] == "Heat")
#     ) %>%
#     dplyr::left_join(
#       share_heat_output, by = c({country}, {year}, {method}, {last_stage}, {energy_type})
#     ) %>%
#     dplyr::mutate(
#       "{e_dot}" := .data[[e_dot]] * .data[[share_heat_by_origin]],
#       "{product}" := dplyr::case_when(
#         .data[[product_origin]] == "Oil products" ~ stringr::str_c(.data[[product]], " [from Oil products]"),
#         .data[[product_origin]] == "Coal products" ~ stringr::str_c(.data[[product]], " [from Coal products]"),
#         .data[[product_origin]] == "Natural gas" ~ stringr::str_c(.data[[product]], " [from Natural gas]"),
#         .data[[product_origin]] == "Renewables" ~ stringr::str_c(.data[[product]], " [from Renewables]"),
#         TRUE ~ .data[[product]]
#       )
#     ) %>%
#     dplyr::select(-.data[[product_origin]], -.data[[share_heat_by_origin]])
#
#
#   # Filter out relevant flows, binding modified flows, and doing the negzeropos trick:
#   .tidy_iea_df %>%
#     # FILTER OUT MODIFIED FLOWS
#     dplyr::filter(
#       ! ((.data[[flow_aggregation_point]] == eiou_flows) & (.data[[product]] == "Electricity"))
#     ) %>%
#     dplyr::filter(
#       ! ((.data[[flow_aggregation_point]] == eiou_flows) & (.data[[product]] == "Heat"))
#     ) %>%
#     # BIND NEWLY MODIFIED FLOWS
#     dplyr::bind_rows(modified_elec_eiou_flows) %>%
#     dplyr::bind_rows(modified_heat_eiou_flows) %>%
#     # NEGZEROPOS TRICK
#     dplyr::mutate(
#       "{negzeropos}" := dplyr::case_when(
#         .data[[e_dot]] < 0 ~ "neg",
#         .data[[e_dot]] == 0 ~ "zero",
#         .data[[e_dot]] > 0 ~ "pos"
#       )
#     ) %>%
#     # Now sum similar rows using summarise.
#     # Group by everything except the energy flow rate column, "E.dot".
#     matsindf::group_by_everything_except(e_dot) %>%
#     dplyr::summarise(
#       "{e_dot}" := sum(.data[[e_dot]])
#     ) %>%
#     dplyr::mutate(
#       #Eliminate the column we added.
#       "{negzeropos}" := NULL
#     ) %>%
#     dplyr::ungroup() %>%
#     return()
# }


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

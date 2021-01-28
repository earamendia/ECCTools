# Meant to remain in the ECCTools package
# This script defines transformation functions


# This function specifies the multiregional R matrix
specify_MR_R <- function(.tidy_iea_df,
                         R_matrix = "R",
                         matnames = "matnames",
                         imports = IEATools::interface_industries$imports,
                         country = IEATools::iea_cols$country,
                         flow = IEATools::iea_cols$flow,
                         product = IEATools::iea_cols$product,
                         aggregate_country_name = "World"){

  MR_R <- .tidy_iea_df %>%
    dplyr::filter(.data[[matnames]] == R_matrix) %>%
    dplyr::mutate(
      "{flow}" := paste0("{", .data[[country]], "}_", .data[[flow]]),
      "{product}" := paste0("{", .data[[country]], "}_", .data[[product]]),
      "{country}":= aggregate_country_name
    )

  return(MR_R)
}


# This function specifies the multiregional V matrix.
specify_MR_V <- function(.tidy_iea_df,
                         V_matrix = "V",
                         Epsilon_matrix = "Epsilon",
                         matnames = "matnames",
                         imports = IEATools::interface_industries$imports,
                         country = IEATools::iea_cols$country,
                         flow = IEATools::iea_cols$flow,
                         product = IEATools::iea_cols$product,
                         e_dot = IEATools::iea_cols$e_dot,
                         aggregate_country_name = "World"){

  MR_V <- .tidy_iea_df %>%
    dplyr::filter((.data[[matnames]] == V_matrix & (! stringr::str_detect(.data[[flow]], imports))) |
                    (.data[[matnames]] == Epsilon_matrix & .data[[e_dot]] < 0 & (! stringr::str_detect(.data[[flow]], imports)))) %>%
    dplyr::mutate(
      "{flow}" := paste0("{", .data[[country]], "}_", .data[[flow]]),
      "{product}" := paste0("{", .data[[country]], "}_", .data[[product]]),
      "{country}":= aggregate_country_name
    )

  return(MR_V)
}



# Function that calculates the total consumption by product for each country in each year.

calc_total_consumption_by_product <- function(.tidy_iea_df,
                                         flow = IEATools::iea_cols$flow,
                                         country = IEATools::iea_cols$country,
                                         method = IEATools::iea_cols$method,
                                         energy_type = IEATools::iea_cols$energy_type,
                                         last_stage = IEATools::iea_cols$last_stage,
                                         year = IEATools::iea_cols$year,
                                         product = IEATools::iea_cols$product,
                                         unit = IEATools::iea_cols$unit,
                                         e_dot = IEATools::iea_cols$e_dot,
                                         exports = IEATools::interface_industries$exports,
                                         matnames = "matnames",
                                         Y_matrix = "Y",
                                         U_feed_matrix = "U_feed",
                                         U_EIOU_matrix = "U_EIOU",
                                         Epsilon_matrix = "Epsilon"){

  tidy_total_consumption_by_product <- .tidy_iea_df %>%
    dplyr::filter(
      ((.data[[matnames]] == Y_matrix | .data[[matnames]] == U_feed_matrix | .data[[matnames]] == U_EIOU_matrix) & (! stringr::str_detect(.data[[flow]], exports))) |
        (.data[[matnames]] == Epsilon_matrix & .data[[e_dot]] >= 0 & (! stringr::str_detect(.data[[flow]], exports)))
      ) %>% # There shouldn't be anymore exports where there are imports, now.
    dplyr::mutate(
      "{e_dot}" := dplyr::case_when(
        .data[[matnames]] == U_feed_matrix ~ -.data[[e_dot]],
        .data[[matnames]] == U_EIOU_matrix ~ -.data[[e_dot]],
        TRUE ~ .data[[e_dot]]
      )
    ) %>%
    dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[product]], .data[[unit]]) %>%
    dplyr::summarise(
      Total_Consumption_From_Func = sum(.data[[e_dot]])
    )

  return(tidy_total_consumption_by_product)
}


# Function that calculates total imports by products, for each country, in each year.

calc_imports_by_product <- function(.tidy_iea_df,
                               flow = IEATools::iea_cols$flow,
                               e_dot = IEATools::iea_cols$e_dot,
                               imports = IEATools::interface_industries$imports){

  tidy_total_imports_by_product <- .tidy_iea_df %>%
    dplyr::filter(stringr::str_detect(.data[[flow]], imports)) %>%
    dplyr::rename("{imports}" := .data[[e_dot]]) %>%
    matsindf::group_by_everything_except(e_dot) %>%
    dplyr::summarise(
      "{imports}" := sum(.data[[imports]])
    ) %>%
    dplyr::ungroup()

  return(tidy_total_imports_by_product)
}



# Function that calculates the shares of imports to total consumption, for each product, for each country, for each year.

calc_share_imports_by_products <- function(.tidy_iea_df,
                                           flow = IEATools::iea_cols$flow,
                                           country = IEATools::iea_cols$country,
                                           method = IEATools::iea_cols$method,
                                           energy_type = IEATools::iea_cols$energy_type,
                                           last_stage = IEATools::iea_cols$last_stage,
                                           ledger_side = IEATools::iea_cols$ledger_side,
                                           flow_aggregation_point = IEATools::iea_cols$flow_aggregation_point,
                                           year = IEATools::iea_cols$year,
                                           product = IEATools::iea_cols$product,
                                           unit = IEATools::iea_cols$unit,
                                           e_dot = IEATools::iea_cols$e_dot,
                                           imports = IEATools::interface_industries$imports,
                                           matnames = "matnames"){

  share_imports_by_product <- dplyr::left_join(
                                  calc_total_consumption_by_product(.tidy_iea_df),
                                  calc_imports_by_product(.tidy_iea_df),
                                  by = c({country}, {method}, {energy_type}, {last_stage}, {year}, {product}, {unit})) %>%
    dplyr::select(-.data[[ledger_side]], -.data[[matnames]], -.data[[flow_aggregation_point]], -.data[[flow]], -.data[[unit]]) %>%
    dplyr::mutate(
      "{imports}" := tidyr::replace_na(.data[[imports]], 0),
      Share_Imports_From_Func = .data[[imports]] / .data[["Total_Consumption_From_Func"]]
    )
}


# Function that calculates global production by product

calc_global_production_by_product <- function(.tidy_iea_df){

  .tidy_iea_df %>%
    dplyr::filter((matnames == V_matrix | matnames == R_matrix) & (! stringr::str_detect(.data[[flow]], imports))) %>%
    dplyr::select(-.data[[country]], -.data[[ledger_side]], -.data[[flow_aggregation_point]], -.data[[flow]]) %>%
    matsindf::group_by_everything_except(e_dot) %>%
    dplyr::summarise(
      "{e_dot}" := sum(.data[[e_dot]])
    )
}

# Function that calculates the national production by product

calc_national_production_by_product <- function(.tidy_iea_df){
  .tidy_iea_df %>%
    dplyr::filter((matnames == V_matrix | matnames == R_matrix) & (! stringr::str_detect(.data[[flow]], imports))) %>%
    dplyr::select(-.data[[ledger_side]], -.data[[flow_aggregation_point]], -.data[[flow]]) %>%
    matsindf::group_by_everything_except(e_dot) %>%
    dplyr::summarise(
      "{e_dot}" := sum(.data[[e_dot]])
    )
}


# Function that calculates the share of global production by product

calc_share_global_production_by_product <- function(.tidy_iea_df){

  share_global_production_by_product <- dplyr::left_join(
      calc_global_production_by_product(.tidy_iea_df),
      calc_production_by_product(.tidy_iea_df),
      by = c({country}, {method}, {energy_type}, {last_stage}, {year}, {product}, {unit})
    ) %>%
    dplyr::select(-.data[[ledger_side]], -.data[[matnames]], -.data[[flow_aggregation_point]], -.data[[flow]], -.data[[unit]]) %>%
    dplyr::mutate(
      Share_Global_Production_From_Func = .data[["Production"]] / .data[["Global_Production"]]
    )

}


# Function that specifies imported versus domestically produced products

specify_imported_products <- function(.tidy_iea_df,
                                      V_matrix = "V",
                                      Y_matrix = "Y",
                                      U_feed_matrix = "U_feed",
                                      U_EIOU_matrix = "U_EIOU",
                                      Epsilon_matrix = "Epsilon",
                                      matnames = "matnames",
                                      domestic = "Domestic",
                                      imported = "Imported",
                                      origin = "Origin",
                                      flow = IEATools::iea_cols$flow,
                                      country = IEATools::iea_cols$country,
                                      method = IEATools::iea_cols$method,
                                      energy_type = IEATools::iea_cols$energy_type,
                                      last_stage = IEATools::iea_cols$last_stage,
                                      year = IEATools::iea_cols$year,
                                      product = IEATools::iea_cols$product,
                                      unit = IEATools::iea_cols$unit,
                                      e_dot = IEATools::iea_cols$e_dot,
                                      exports = IEATools::interface_industries$exports,
                                      imports = IEATools::interface_industries$imports){

  defined_imported_flows <- .tidy_iea_df %>%
    dplyr::filter(
        ((.data[[matnames]] == Y_matrix | .data[[matnames]] == U_feed_matrix | .data[[matnames]] == U_EIOU_matrix) & (! stringr::str_detect(.data[[flow]], exports))) |
          ((.data[[matnames]] == Epsilon_matrix & .data[[e_dot]] >= 0) & (! stringr::str_detect(.data[[flow]], exports)))
    ) %>%
    dplyr::left_join(
      calc_share_imports_by_products(.tidy_iea_df), by = c({country}, {year}, {product}, {method}, {energy_type}, {last_stage})
    ) %>%
    dplyr::mutate(
      "{domestic}" := .data[[e_dot]] * (1 - Share_Imports_From_Func),
      "{imported}" := .data[[e_dot]] * Share_Imports_From_Func
    ) %>%
    dplyr::select(-.data[[e_dot]], -.data[[imports]], -Total_Consumption_From_Func, -Share_Imports_From_Func) %>%
    tidyr::pivot_longer(cols = c(.data[[domestic]], .data[[imported]]), names_to = origin, values_to = e_dot) %>%
    dplyr::relocate(.data[[origin]], .after = .data[[product]]) %>%
    dplyr::filter(.data[[e_dot]] != 0)

  return(defined_imported_flows)
}




# This function computes global exports by product

calc_global_exports <- function(.tidy_iea_df,
                                flow = IEATools::iea_cols$flow,
                                method = IEATools::iea_cols$method,
                                energy_type = IEATools::iea_cols$energy_type,
                                last_stage = IEATools::iea_cols$last_stage,
                                year = IEATools::iea_cols$year,
                                ledger_side = IEATools::iea_cols$ledger_side,
                                flow_aggregation_point = IEATools::iea_cols$flow_aggregation_point,
                                product = IEATools::iea_cols$product,
                                e_dot = IEATools::iea_cols$e_dot,
                                exports = IEATools::interface_industries$exports){


  tidy_global_exports_by_product <- .tidy_iea_df %>%
    dplyr::filter(stringr::str_detect(.data[[flow]], exports)) %>%
    dplyr::mutate(
      "{e_dot}" := abs(.data[[e_dot]])
    ) %>%
    dplyr::group_by(.data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[ledger_side]], .data[[flow_aggregation_point]],
                    .data[[product]]) %>%
    dplyr::summarise(
      Total_Exports_From_Func = sum(.data[[e_dot]])
    )

  return(tidy_global_exports_by_product)
}


# This function computes the shares of exports for each country, for each product

calc_share_exports_by_product <- function(.tidy_iea_df,
                                          flow = IEATools::iea_cols$flow,
                                          country = IEATools::iea_cols$country,
                                          method = IEATools::iea_cols$method,
                                          energy_type = IEATools::iea_cols$energy_type,
                                          last_stage = IEATools::iea_cols$last_stage,
                                          year = IEATools::iea_cols$year,
                                          ledger_side = IEATools::iea_cols$ledger_side,
                                          flow_aggregation_point = IEATools::iea_cols$flow_aggregation_point,
                                          product = IEATools::iea_cols$product,
                                          e_dot = IEATools::iea_cols$e_dot,
                                          unit = IEATools::iea_cols$unit,
                                          exports = IEATools::interface_industries$exports,
                                          matnames = "matnames",
                                          provenience = "Provenience"){

  tidy_share_exports_by_product <- .tidy_iea_df %>%
    dplyr::filter(stringr::str_detect(.data[[flow]], exports)) %>%
    dplyr::mutate(
      "{e_dot}" := abs(.data[[e_dot]])
    ) %>%
    dplyr::left_join(calc_global_exports(.tidy_iea_df), by = c({method}, {energy_type}, {last_stage}, {year}, {ledger_side}, {flow_aggregation_point}, {product})) %>%
    dplyr::mutate(
      Share_Exports_From_Func = .data[[e_dot]] / Total_Exports_From_Func
    ) %>%
    dplyr::rename("{provenience}" := .data[[country]]) %>%
    dplyr::ungroup() %>%
    dplyr::select(-.data[[ledger_side]], -.data[[flow_aggregation_point]], -.data[[flow]], -.data[[unit]], -.data[[e_dot]], -.data[[matnames]], -.data[["Total_Exports_From_Func"]])

  return(tidy_share_exports_by_product)
}



# This function specifies the multiregional Y matrix using the GMA assumption
specify_MR_Y_U_gma <- function(.tidy_iea_df,
                               flow = IEATools::iea_cols$flow,
                               product = IEATools::iea_cols$product,
                               year = IEATools::iea_cols$year,
                               method = IEATools::iea_cols$method,
                               energy_type = IEATools::iea_cols$energy_type,
                               last_stage = IEATools::iea_cols$last_stage,
                               e_dot = IEATools::iea_cols$e_dot,
                               country = IEATools::iea_cols$country,
                               aggregate_country_name = "World",
                               provenience = "Provenience"){

  # (1) Differentiating domestically produced and imported products in the Y and U matrices flows
  tidy_iea_df_specified_imports <- .tidy_iea_df %>%
    specify_imported_products()

  # (2) Specifying domestic consumption
  tidy_domestic_consumption_MR_gma <- tidy_iea_df_specified_imports %>%
    dplyr::filter(Origin == "Domestic") %>%
    dplyr::mutate(
      "{flow}" := paste0("{", .data[[country]], "}_", .data[[flow]]),
      "{product}" := paste0("{", .data[[country]], "}_", .data[[product]]),
      "{country}" := aggregate_country_name
    ) %>%
    dplyr::select(-Origin)


  # (3) Specifying foreign consumption
  tidy_imported_consumption_MR_gma <- tidy_iea_df_specified_imports %>%
    dplyr::filter(Origin == "Imported") %>%
    dplyr::left_join(calc_share_exports_by_product(.tidy_iea_df),
                     by = c({method}, {energy_type}, {last_stage}, {year}, {product})) %>%
    dplyr::mutate(
      "{e_dot}" := .data[[e_dot]] * Share_Exports_From_Func,
      "{flow}" := paste0("{", .data[[country]], "}_", .data[[flow]]),
      "{product}" := paste0("{", .data[[provenience]], "}_", .data[[product]]),
      "{country}" := aggregate_country_name
    ) %>%
    dplyr::select(-.data[[provenience]], -.data[["Share_Exports_From_Func"]], -.data[["Origin"]])

  # (4) When a given product is imported by a given country, but no country exports such product, then calculate the global production mix.
  tidy_imported_consumption_MR_gma %>%
    dplyr::filter(is.na(.data[[e_dot]])) %>%
    dplyr::left_join(calc_share_global_production_by_product(.tidy_iea_df),
                     by = c({method}, {energy_type}, {last_stage}, {year}, {product})) %>%
    dplyr::mutate(
      "{e_dot}" := .data[[e_dot]] * Share_Global_Production_From_Func,
      "{product}" := stringr::str_replace(.data[[product]], "NA", .data[["Origin"]]),
      "{country}" := aggregate_country_name
    )


  # (5) Testing if we have any NAs in the join...
  if (NA %in% tidy_imported_consumption_MR_gma[[e_dot]]){
    stop("There an NA in the join here, do worry about it.")# How do I get code coverage here?
  }

  # Right. And here we need to "gather" flows of the same things, are these may appear in (4) if the country also produces locally the product.
  tidy_consumption_MR_gma <- dplyr::bind_rows(tidy_domestic_consumption_MR_gma, tidy_imported_consumption_MR_gma) %>%
    matsindf::group_by_everything_except(e_dot) %>%
    dplyr::summarise(
      "{e_dot}" := sum(.data[[e_dot]])
    )

  return(tidy_consumption_MR_gma)

}



# Transform to gma function
transform_to_gma <- function(.tidy_iea_df){

  # (1) Create MR-R matrix data frame
  MR_R_gma <- specify_MR_R(.tidy_iea_df)

  # (2) Creating MR-V matrix data frame
  MR_V_gma <- specify_MR_V(.tidy_iea_df)

  # (3) Creating MR-Y and MR-U matrix data frames, with GMA assumption
  MR_Y_U_gma <- specify_MR_Y_U_gma(.tidy_iea_df)

  # Binding all rows and returning full data frame
  tidy_iea_MR_gma_df <- dplyr::bind_rows(MR_R_gma, MR_V_gma, MR_Y_U_gma)

  return(tidy_iea_MR_gma_df)
}



# This function calculates the bilateral trade matrix data frame using the GMA assumption.
# Basically it has two purposes:
# (i) if the user doesn't load a trade matrix in the specify_MR_Y_U_bta() function, then the default trade matrix is the one built from the GMA assumption;
# (ii) if the user loads an incomplete trade matrix in the specify_MR_Y_U_bta() function (likely as trade data is not available for all years!);
# then the missing information is loaded from the GMA bilateral trade matrix.

calc_bilateral_trade_matrix_df_gma <- function(.tidy_iea_df,
                                               year = IEATools::iea_cols$year,
                                               method = IEATools::iea_cols$method,
                                               energy_type = IEATools::iea_cols$energy_type,
                                               last_stage = IEATools::iea_cols$last_stage,
                                               country = IEATools::iea_cols$country,
                                               provenience = "Provenience",
                                               matnames = "matnames"){

  bilateral_trade_matrix_df_gma <- calc_share_exports_by_product(.tidy_iea_df) %>%
    tidyr::expand_grid(
      "{country}" := .tidy_iea_df %>%
        tidyr::expand(.data[[country]]) %>%
        dplyr::pull()
    ) %>%
    dplyr::mutate(
      Share_Exports_From_Func = dplyr::case_when(
       .data[[provenience]] == .data[[country]] ~ 0,
       TRUE ~ Share_Exports_From_Func
      )
    ) %>%
    dplyr::relocate(.data[[country]], .after = .data[[provenience]])

  return(bilateral_trade_matrix_df_gma)

}


# This function specifies the multiregional Y matrix using the BTA assumption, using the specific trade matrix provided as input.
# For values that are not available in the provided trade matrix, it fills in using the GMA assumption.

specify_MR_Y_U_bta <- function(.tidy_iea_df,
                               bilateral_trade_matrix_df = calc_bilateral_trade_matrix_df_gma(.tidy_iea_df),
                               flow = IEATools::iea_cols$flow,
                               product = IEATools::iea_cols$product,
                               year = IEATools::iea_cols$year,
                               method = IEATools::iea_cols$method,
                               energy_type = IEATools::iea_cols$energy_type,
                               last_stage = IEATools::iea_cols$last_stage,
                               e_dot = IEATools::iea_cols$e_dot,
                               country = IEATools::iea_cols$country,
                               aggregate_country_name = "World",
                               provenience = "Provenience"){


  # (1) Differentiating domestically produced and imported products in the Y and U matrices flows
  tidy_iea_df_specified_imports <- .tidy_iea_df %>%
    specify_imported_products()

  # (2) Specifying domestic consumption
  tidy_domestic_consumption_MR_bta <- tidy_iea_df_specified_imports %>%
    dplyr::filter(Origin == "Domestic") %>%
    dplyr::mutate(
      "{flow}" := paste0("{", .data[[country]], "}_", .data[[flow]]),
      "{product}" := paste0("{", .data[[country]], "}_", .data[[product]]),
      "{country}" := aggregate_country_name
    ) %>%
    dplyr::select(-Origin)

  # (3.i) Specifying foreign consumption with the provided trade matrix
  tidy_imported_consumption_with_bt_matrix <- tidy_iea_df_specified_imports %>%
    dplyr::filter(Origin == "Imported") %>%
    dplyr::left_join(bilateral_trade_matrix_df,
                    by = c({country}, {method}, {energy_type}, {last_stage}, {year}, {product})) %>%
    dplyr::filter(! is.na(.data[["Share_Exports_From_Func"]])) %>%
    dplyr::mutate(
      "{e_dot}" := .data[[e_dot]] * Share_Exports_From_Func,
      "{flow}" := paste0("{", .data[[country]], "}_", .data[[flow]]),
      "{product}" := paste0("{", .data[[provenience]], "}_", .data[[product]]),
      "{country}" := aggregate_country_name
    ) %>%
    dplyr::select(-.data[[provenience]], -.data[["Share_Exports_From_Func"]], -.data[["Origin"]])

  # (3.ii) Specifying the rest with the global market assumption GMA
  tidy_imported_consumption_with_gma_bt_matrix <- tidy_iea_df_specified_imports %>%
    dplyr::filter(Origin == "Imported") %>%
    dplyr::left_join(bilateral_trade_matrix_df,
                     by = c({country}, {method}, {energy_type}, {last_stage}, {year}, {product})) %>%
    dplyr::filter(is.na(.data[["Share_Exports_From_Func"]])) %>%
    dplyr::select(-.data[[provenience]], -.data[["Share_Exports_From_Func"]]) %>%
    dplyr::left_join(
      calc_bilateral_trade_matrix_df_gma(.tidy_iea_df),
      by = c({country}, {method}, {energy_type}, {last_stage}, {year}, {product})
    ) %>%
    dplyr::mutate(
      "{e_dot}" := .data[[e_dot]] * Share_Exports_From_Func,
      "{flow}" := paste0("{", .data[[country]], "}_", .data[[flow]]),
      "{product}" := paste0("{", .data[[provenience]], "}_", .data[[product]]),
      "{country}" := aggregate_country_name
    ) %>%
    dplyr::select(-.data[[provenience]], -.data[["Share_Exports_From_Func"]], -.data[["Origin"]])


  # (3.iii) Binding both
  tidy_imported_consumption_MR_bta <- dplyr::bind_rows(tidy_imported_consumption_with_bt_matrix,
                                                       tidy_imported_consumption_with_gma_bt_matrix)


  # Binding data frames together and returning result
  tidy_consumption_MR_bta <- dplyr::bind_rows(tidy_domestic_consumption_MR_bta,
                                              tidy_imported_consumption_MR_bta)

  return(tidy_consumption_MR_bta)

}




# Transform to bta function
transform_to_bta <- function(.tidy_iea_df,
                             bilateral_trade_matrix_df = calc_bilateral_trade_matrix_df_gma(.tidy_iea_df)){

  # (1) Create MR-R matrix data frame
  MR_R_bta <- specify_MR_R(.tidy_iea_df)

  # (2) Creating MR-V matrix data frame
  MR_V_bta <- specify_MR_V(.tidy_iea_df)

  # (3) Creating MR-Y and MR-U matrix data frames, with GMA assumption
  MR_Y_U_bta <- specify_MR_Y_U_bta(.tidy_iea_df, bilateral_trade_matrix_df = bilateral_trade_matrix_df)

  # Binding all rows and returning full data frame
  tidy_iea_MR_bta_df <- dplyr::bind_rows(MR_R_bta, MR_V_bta, MR_Y_U_bta)

  return(tidy_iea_MR_bta_df)
}



# Find list of observations for which the DTA can be implemented

find_list_dta_observations <- function(.tidy_iea_df,
                                       country = IEATools::iea_cols$country,
                                       method = IEATools::iea_cols$method,
                                       energy_type = IEATools::iea_cols$energy_type,
                                       last_stage = IEATools::iea_cols$last_stage,
                                       year = IEATools::iea_cols$year,
                                       flow = IEATools::iea_cols$flow,
                                       product = IEATools::iea_cols$product,
                                       imports = IEATools::interface_industries$imports,
                                       matnames = "matnames",
                                       y_matrix = "Y",
                                       u_eiou_matrix = "U_EIOU",
                                       u_feed_matrix = "U_feed",
                                       v_matrix = "V",
                                       r_matrix = "R"){

  list_supplied_products_per_observation <- .tidy_iea_df %>%
    dplyr::filter((.data[[matnames]] == v_matrix | .data[[matnames]] == r_matrix) & (! stringr::str_detect(.data[[flow]], imports))) %>%
    dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]]) %>%
    tidyr::expand(.data[[product]]) %>%
    tidyr::unite(col = "Observation_Product_From_Func", .data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[product]]) %>%
    dplyr::pull()


  list_dta_observations <- .tidy_iea_df %>%
    dplyr::filter(.data[[matnames]] == y_matrix | .data[[matnames]] == u_eiou_matrix | .data[[matnames]] == u_feed_matrix) %>% #These are products used
    dplyr::mutate(
      is_produced_domestically = dplyr::case_when(
        stringr::str_c(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[product]], sep = "_") %in% list_supplied_products_per_observation ~ TRUE,
        TRUE ~ FALSE
      )
    ) %>%
    dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]]) %>%
    dplyr::filter(! (FALSE %in% .data[["is_produced_domestically"]])) %>%
    tidyr::unite(col = "Observation_For_DTA_From_Func", .data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]]) %>%
    tidyr::expand(.data[["Observation_For_DTA_From_Func"]]) %>%
    dplyr::pull()

  return(list_dta_observations)
}



# Transform to dta function

transform_to_dta <- function(.tidy_iea_df,
                             country = IEATools::iea_cols$country,
                             method = IEATools::iea_cols$method,
                             energy_type = IEATools::iea_cols$energy_type,
                             last_stage = IEATools::iea_cols$last_stage,
                             year = IEATools::iea_cols$year,
                             flow = IEATools::iea_cols$flow,
                             ledger_side = IEATools::iea_cols$ledger_side,
                             e_dot = IEATools::iea_cols$e_dot,
                             imports = IEATools::interface_industries$imports,
                             epsilon = "Epsilon"){

  list_dta_observations <- .tidy_iea_df %>%
    find_list_dta_observations()

  .tidy_iea_df %>%
    dplyr::filter(stringr::str_c(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], sep = "_") %in% list_dta_observations) %>%
    dplyr::mutate(
      "{ledger_side}" := dplyr::case_when(
        stringr::str_detect(.data[[flow]], imports) ~ stringr::str_c("{", epsilon, "}_", .data[[ledger_side]]),
        TRUE ~ .data[[ledger_side]]
      ),
      "{e_dot}" := dplyr::case_when(
        stringr::str_detect(.data[[flow]], imports) ~ -.data[[e_dot]],
        TRUE ~ .data[[e_dot]]
      )
    )
}



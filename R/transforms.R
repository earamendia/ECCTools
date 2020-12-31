
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
                         matnames = "matnames",
                         imports = IEATools::interface_industries$imports,
                         country = IEATools::iea_cols$country,
                         flow = IEATools::iea_cols$flow,
                         product = IEATools::iea_cols$product,
                         aggregate_country_name = "World"){

  MR_V <- .tidy_iea_df %>%
    dplyr::filter(.data[[matnames]] == V_matrix, ! stringr::str_detect(.data[[flow]], imports)) %>%
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
                                         U_EIOU_matrix = "U_EIOU"){

  tidy_total_consumption_by_product <- .tidy_iea_df %>%
    dplyr::filter((.data[[matnames]] == Y_matrix |
              .data[[matnames]] == U_feed_matrix |
              .data[[matnames]] == U_EIOU_matrix),
           ! stringr::str_detect(.data[[flow]], exports)) %>% # There shouldn't be anymore exports where there are imports, now.
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
    dplyr::rename("{imports}" := .data[[e_dot]])

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

  share_imports_by_product <- left_join(
                                  calc_total_consumption_by_product(.tidy_iea_df),
                                  calc_imports_by_product(.tidy_iea_df),
                                  by = c({country}, {method}, {energy_type}, {last_stage}, {year}, {product}, {unit})) %>%
    select(-.data[[ledger_side]], -.data[[matnames]], -.data[[flow_aggregation_point]], -.data[[flow]], -.data[[unit]]) %>%
    mutate(
      Share_Imports_From_Func = case_when(
          is.na(.data[[imports]]) ~ 0,
          !is.na(.data[[imports]]) ~ .data[[imports]] / .data[["Total_Consumption_From_Func"]]
      )
    )
}



# Function that specifies imported versus domestically produced products

specify_imported_products <- function(.tidy_iea_df,
                                      V_matrix = "V",
                                      Y_matrix = "Y",
                                      U_feed_matrix = "U_feed",
                                      U_EIOU_matrix = "U_EIOU",
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
    filter(
      .data[[matnames]] == Y_matrix |
        .data[[matnames]] == U_feed_matrix |
        .data[[matnames]] == U_EIOU_matrix,
      ! str_detect(.data[[flow]], exports)
    ) %>%
    left_join(
      calc_share_imports_by_products(.tidy_iea_df), by = c({country}, {year}, {product}, {method}, {energy_type}, {last_stage})
    ) %>%
    mutate(
      "{domestic}" := .data[[e_dot]] * (1 - Share_Imports_From_Func),
      "{imported}" := .data[[e_dot]] * Share_Imports_From_Func
    ) %>%
    select(-.data[[e_dot]], -.data[[imports]], -Total_Consumption_From_Func, -Share_Imports_From_Func) %>%
    pivot_longer(cols = c(.data[[domestic]], .data[[imported]]), names_to = origin, values_to = e_dot) %>%
    relocate(.data[[origin]], .after = .data[[product]]) %>%
    filter(.data[[e_dot]] != 0)

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
                                          method = IEATools::iea_cols$method,
                                          energy_type = IEATools::iea_cols$energy_type,
                                          last_stage = IEATools::iea_cols$last_stage,
                                          year = IEATools::iea_cols$year,
                                          ledger_side = IEATools::iea_cols$ledger_side,
                                          flow_aggregation_point = IEATools::iea_cols$flow_aggregation_point,
                                          product = IEATools::iea_cols$product,
                                          e_dot = IEATools::iea_cols$e_dot,
                                          exports = IEATools::interface_industries$exports){

  tidy_share_exports_by_product <- .tidy_iea_df %>%
    dplyr::filter(stringr::str_detect(.data[[flow]], exports)) %>%
    dplyr::mutate(
      "{e_dot}" := abs(.data[[e_dot]])
    ) %>%
    dplyr::left_join(calc_global_exports(.tidy_iea_df), by = c({method}, {energy_type}, {last_stage}, {year}, {ledger_side}, {flow_aggregation_point}, {product})) %>%
    dplyr::mutate(
      Share_Exports_From_Func = .data[[e_dot]] / Total_Exports_From_Func
    )

  return(tidy_share_exports_by_product)
}



# This function specifies the multiregional Y matrix using the GMA assumption
specify_MR_Y_U_gma <- function(.tidy_iea_df,
                               flow = IEATools::iea_cols$flow,
                               product = IEATools::iea_cols$product,
                               country = IEATools::iea_cols$country,
                               aggregate_country_name = "World"){

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
  tidy_foreign_consumption_MR_gma <- tidy_iea_df_specified_imports %>%
    dplyr::filter(Origin == "Imported") #%>%
# Still some dev to do here. The "Countries" fields do not correspond.


  tidy_consumption_MR_gma <- bind_rows(tidy_domestic_consumption_MR_gma, tidy_foreign_consumption_MR_gma)

  return(tidy_foreign_consumption_MR)

}




# Third, specifying all consumption flows - actually specifying the country of origin!
AB_domestic_consumption_MR <- defining_imported_products %>%
  filter(Origin == "Domestic") %>%
  mutate(
    Flow = paste0("{", Country, "}_", Flow),
    Product = paste0("{", Country, "}_", Product),
    Country = "World"
  ) %>%
  select(-Origin, -Unit.x, -Unit.y) %>%
  print()

# If we write here testing instead of share_exports_by_origin_destination, we have a decent test.
AB_imported_consumption_MR <- defining_imported_products %>%
  filter(Origin == "Imported") %>%
  inner_join(share_exports_by_origin_destination, by = c("Country", "Method", "Energy.type", "Last.stage", "Year", "Product")) %>%
  relocate(Provenience, .before = Country) %>%
  mutate(
    E.dot = E.dot * Share_Exports,
    Flow = paste0("{", Country, "}_", Flow),
    Product = paste0("{", Provenience, "}_", Product)
  ) %>%
  select(-Provenience, -Share_Exports, -Origin) %>%
  print()


# Join both data frames
AB_consumption_MR <- bind_rows(AB_domestic_consumption_MR, AB_imported_consumption_MR)






# Transform to gma function
transform_to_gma <- function() {
  return("hello world")

  # (1) Create MR-R matrix data frame
  #MR_R <- specify_MR_R()

  # (2) Creating MR-V matrix data frame
  #MR_V <- specify_MR_V()

  # (3) Creating MR-Y and MR-U matrix data frames, with GMA assumption
  #MR_Y <- specify_MR_Y_gma()

  # (4) Creating MR-U matrix data frame, with GMA assumption
  #MR_U <- specify_MR_U_gma()

}




# Transform to bta function
transform_to_bta <- function() {
  return("hello world")
}


# Transform to dta function

transform_to_dta <- function() {
  return("hello world")
}



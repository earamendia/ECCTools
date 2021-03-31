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

calc_global_production_by_product <- function(.tidy_iea_df,
                                              country = IEATools::iea_cols$country,
                                              ledger_side = IEATools::iea_cols$ledger_side,
                                              flow_aggregation_point = IEATools::iea_cols$flow_aggregation_point,
                                              flow = IEATools::iea_cols$flow,
                                              e_dot = IEATools::iea_cols$e_dot,
                                              matnames = IEATools::mat_meta_cols$matnames,
                                              V_matrix = IEATools::psut_cols$V,
                                              R_matrix = IEATools::psut_cols$R,
                                              imports = IEATools::interface_industries$imports){

  .tidy_iea_df %>%
    dplyr::filter((matnames == V_matrix | matnames == R_matrix) & (! stringr::str_detect(.data[[flow]], imports))) %>%
    dplyr::ungroup() %>%
    dplyr::select(-.data[[country]], -.data[[ledger_side]], -.data[[flow_aggregation_point]], -.data[[flow]], -.data[[matnames]]) %>%
    matsindf::group_by_everything_except(e_dot) %>%
    dplyr::summarise(
      Global_Production_From_Func = sum(.data[[e_dot]])
    )
}

# Function that calculates the national production by product

calc_national_production_by_product <- function(.tidy_iea_df,
                                                ledger_side = IEATools::iea_cols$ledger_side,
                                                flow_aggregation_point = IEATools::iea_cols$flow_aggregation_point,
                                                flow = IEATools::iea_cols$flow,
                                                e_dot = IEATools::iea_cols$e_dot,
                                                matnames = IEATools::mat_meta_cols$matnames,
                                                V_matrix = IEATools::psut_cols$V,
                                                R_matrix = IEATools::psut_cols$R,
                                                imports = IEATools::interface_industries$imports){
  .tidy_iea_df %>%
    dplyr::filter((matnames == V_matrix | matnames == R_matrix) & (! stringr::str_detect(.data[[flow]], imports))) %>%
    dplyr::ungroup() %>%
    dplyr::select(-.data[[ledger_side]], -.data[[flow_aggregation_point]], -.data[[flow]], -.data[[matnames]]) %>%
    matsindf::group_by_everything_except(e_dot) %>%
    dplyr::summarise(
      National_Production_From_Func = sum(.data[[e_dot]])
    )
}


# Function that calculates the share of global production by product

calc_share_global_production_by_product <- function(.tidy_iea_df,
                                                    country = IEATools::iea_cols$country,
                                                    year = IEATools::iea_cols$year,
                                                    method = IEATools::iea_cols$method,
                                                    energy_type = IEATools::iea_cols$energy_type,
                                                    last_stage = IEATools::iea_cols$last_stage,
                                                    ledger_side = IEATools::iea_cols$ledger_side,
                                                    flow_aggregation_point = IEATools::iea_cols$flow_aggregation_point,
                                                    flow = IEATools::iea_cols$flow,
                                                    product = IEATools::iea_cols$product,
                                                    e_dot = IEATools::iea_cols$e_dot,
                                                    unit = IEATools::iea_cols$unit,
                                                    matnames = IEATools::mat_meta_cols$matnames
){

  share_global_production_by_product <- dplyr::left_join(
    calc_global_production_by_product(.tidy_iea_df),
    calc_national_production_by_product(.tidy_iea_df),
    by = c({method}, {energy_type}, {last_stage}, {year}, {product}, {unit})
  ) %>%
    dplyr::mutate(
      Share_Global_Production_From_Func = .data[["National_Production_From_Func"]] / .data[["Global_Production_From_Func"]]
    ) %>%
    dplyr::select(-.data[["National_Production_From_Func"]], -.data[["Global_Production_From_Func"]]) %>%
    dplyr::rename(Producing_Country_From_Func = .data[[country]])

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


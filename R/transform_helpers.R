
#' Calculates total consumption by product, for each country and year
#'
#' The function calculates total consumption by product, for each country and year.
#'
#' Flows included in the calculation of the total consumption by product are flows belonging to either the Y,
#' U_feed, or U_eiou matrices. Flows belonging to the Y matrix but representing exports, are excluded.
#' In addition, flows belonging to the Balancing matrix and akin to a final demand (i.e. E.dot > 0) are also included, provided they do not represent exports.
#'
#' @param .tidy_iea_df The `.tidy_iea_df` to which the function should be applied.
#' @param flow,country,method,energy_type,last_stage,year,product,unit,e_dot See `IEATools::iea_cols`.
#' @param exports The name of exports flows in the input data.
#'                Default is `IEATools::interface_industries$exports`.
#' @param matnames The column name for matrices names.
#'                 Default is `IEATools::mat_meta_cols$matnames`.
#' @param Y_matrix The name of the final demand Y matrix.
#'                 Default is `IEATools::psut_cols$Y`.
#' @param U_feed_matrix The name of the U_feed matrix.
#'                      Default is `IEATools::psut_cols$U_feed`.
#' @param U_EIOU_matrix The name of the U_eiou matrix.
#'                      Default is `IEATools::psut_cols$U_eiou`.
#' @param balancing_matrix The name of the Balancing matrix.
#'                      Default is "B".
#' @param Total_Consumption_By_Product The name of the new column reporting total consumption by product.
#'
#' @return A data frame with total consumption by product, for each country and year.
#' @export
#'
#' @examples
#' tidy_AB_data %>%
#' IEATools::add_psut_matnames() %>%
#' calc_total_consumption_by_product() %>%
#' print()
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
                                              balancing_matrix = "B",
                                              Total_Consumption_By_Product = "Total_Consumption_By_Product"){

  tidy_total_consumption_by_product <- .tidy_iea_df %>%
    dplyr::filter(
      ((.data[[matnames]] == Y_matrix | .data[[matnames]] == U_feed_matrix | .data[[matnames]] == U_EIOU_matrix) & (! stringr::str_detect(.data[[flow]], exports))) |
        (.data[[matnames]] == balancing_matrix & .data[[e_dot]] >= 0 & (! stringr::str_detect(.data[[flow]], exports)))
    ) %>% # There shouldn't be anymore exports where there are imports, now.
    dplyr::mutate(
      "{e_dot}" := dplyr::case_when(
        .data[[matnames]] == U_feed_matrix ~ -.data[[e_dot]],
        .data[[matnames]] == U_EIOU_matrix ~ -.data[[e_dot]],
        # Try to take abs(.data[[e_dot]])!
        # This may enable to deal with losses, which have negative e_dot values, but which are part of Y....
        TRUE ~ abs(.data[[e_dot]])
      )
    ) %>%
    dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[product]], .data[[unit]]) %>%
    dplyr::summarise(
      "{Total_Consumption_By_Product}" := sum(.data[[e_dot]])
    )

  return(tidy_total_consumption_by_product)
}


#' Calculates total imports by product, for each country, for each year.
#'
#' The function calculates total imports by product, for each country, for each year.
#'
#' @param .tidy_iea_df The `.tidy_iea_df` for which total imports need to be calculated.
#' @param flow,e_dot See `IEATools::iea_cols`.
#' @param imports The name of the column containing the quantity (in energy terms) of imported products.
#'        Default is `IEATools::interface_industries$imports`.
#'
#' @return A data frame returning the value of imported energy by product, for each country and year.
#' @export
#'
#' @examples
#' tidy_AB_data %>%
#' calc_imports_by_product() %>%
#' print()
calc_imports_by_product <- function(.tidy_iea_df,
                                    flow = IEATools::iea_cols$flow,
                                    e_dot = IEATools::iea_cols$e_dot,
                                    imports = IEATools::interface_industries$imports){

  tidy_total_imports_by_product <- .tidy_iea_df %>%
    dplyr::filter(stringr::str_detect(.data[[flow]], imports)) %>%
    dplyr::rename("{imports}" := tidyselect::all_of(e_dot)) %>%
    matsindf::group_by_everything_except(e_dot) %>%
    dplyr::summarise(
      "{imports}" := sum(.data[[imports]])
    ) %>%
    dplyr::ungroup()

  return(tidy_total_imports_by_product)
}



#' Calculates the shares of imports to total consumption by product, for each country and year.
#'
#' The function calculates the shares of imports to total consumption by product, for each country and year.
#'
#' Total consumption is calculated using the `calc_total_consumption_by_product()` function.
#'
#' Note: the function needs to have a column indicating matrix names added first, most likely using the `IEATools::add_psut_matnames()` function.
#'
#'
#' @param .tidy_iea_df The `.tidy_iea_df` for which the shares of imports over final consumption, by product, need to be calculated.
#' @param flow,country,method,energy_type,last_stage,ledger_side,flow_aggregation_point,year,product,unit,e_dot See `IEATools::iea_cols`.
#' @param imports The name of the column containing the quantity (in energy terms) of imported products.
#'                Default is `IEATools::interface_industries$imports`.
#' @param matnames The column name for matrices names.
#'                 Default is `IEATools::mat_meta_cols$matnames`.
#' @param Total_Consumption_By_Product The name of the new column returning total consumption by product.
#' @param Share_Imports_By_Product The name of the new column returning the share of imports over total consumption.
#'
#' @return A data frame returning the share of imports over final consumption, for each product.
#' @export
#'
#' @examples
#' tidy_AB_data %>%
#' IEATools::add_psut_matnames() %>%
#' calc_share_imports_by_products() %>%
#' print()
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
                                           matnames = "matnames",
                                           Total_Consumption_By_Product = "Total_Consumption_By_Product",
                                           Share_Imports_By_Product = "Share_Imports_By_Product"){

  share_imports_by_product <- dplyr::left_join(
    calc_total_consumption_by_product(.tidy_iea_df,
                                      Total_Consumption_By_Product = Total_Consumption_By_Product),
    calc_imports_by_product(.tidy_iea_df,
                            imports = imports),
    by = c({country}, {method}, {energy_type}, {last_stage}, {year}, {product}, {unit})) %>%
    dplyr::select(-tidyselect::all_of(c(ledger_side, matnames, flow_aggregation_point, flow, unit))) %>%
    dplyr::mutate(
      "{imports}" := tidyr::replace_na(.data[[imports]], 0),
      "{Share_Imports_By_Product}" := .data[[imports]] / .data[[Total_Consumption_By_Product]]
    )
}


#' Calculates global production by product
#'
#' This function calculates global production by product. Included flows are flows belonging to either the
#' R or V matrices, excluding import flows.
#'
#' Note: the function needs to have a column indicating matrix names added first, most likely using the `IEATools::add_psut_matnames()` function.
#'
#' @param .tidy_iea_df The `.tidy_iea_df` for which global production needs to be calculated.
#' @param country,ledger_side,flow_aggregation_point,flow,e_dot See `IEATools::iea_cols`.
#' @param matnames The column name for matrices names.
#'                 Default is `IEATools::mat_meta_cols$matnames`.
#' @param V_matrix The name of the V matrix.
#'                 Default is `IEATools::psut_cols$V`.
#' @param R_matrix The name of the R matrix.
#'                 Default is `IEATools::psut_cols$R`.
#' @param imports The name of imports flows in the `.tidy_iea_df`.
#'                Default is `IEATools::interface_industries$imports`.
#' @param Global_Production_By_Product The name of the new column returning global production by product.
#'
#' @return A data frame representing global production by product, for each year.
#' @export
#'
#' @examples
#' tidy_AB_data %>%
#' IEATools::add_psut_matnames() %>%
#' calc_global_production_by_product() %>%
#' print()
calc_global_production_by_product <- function(.tidy_iea_df,
                                              country = IEATools::iea_cols$country,
                                              ledger_side = IEATools::iea_cols$ledger_side,
                                              flow_aggregation_point = IEATools::iea_cols$flow_aggregation_point,
                                              flow = IEATools::iea_cols$flow,
                                              e_dot = IEATools::iea_cols$e_dot,
                                              matnames = IEATools::mat_meta_cols$matnames,
                                              V_matrix = IEATools::psut_cols$V,
                                              R_matrix = IEATools::psut_cols$R,
                                              imports = IEATools::interface_industries$imports,
                                              Global_Production_By_Product = "Global_Production_By_Product"){

  .tidy_iea_df %>%
    dplyr::filter((matnames == V_matrix | matnames == R_matrix) & (! stringr::str_detect(.data[[flow]], imports))) %>%
    dplyr::ungroup() %>%
    dplyr::select(-tidyselect::all_of(c(country, ledger_side, flow_aggregation_point, flow, matnames))) %>%
    matsindf::group_by_everything_except(e_dot) %>%
    dplyr::summarise(
      "{Global_Production_By_Product}" := sum(.data[[e_dot]])
    )
}


#' Calculates the national production by product
#'
#' The function calculates the national production by product. Included flows are flows belonging to either the
#' R or V matrices, excluding import flows.
#'
#' Note: the function needs to have a column indicating matrix names added first, most likely using the `IEATools::add_psut_matnames()` function.
#'
#' @param .tidy_iea_df The `.tidy_iea_df` for which national production by product needs to be calculated.
#' @param ledger_side,flow_aggregation_point,flow,e_dot See `IEATools::iea_cols`.
#' @param matnames The column name for matrices names.
#'                 Default is `IEATools::mat_meta_cols$matnames`.
#' @param V_matrix The name of the V matrix.
#'                 Default is `IEATools::psut_cols$V`.
#' @param R_matrix The name of the R matrix.
#'                 Default is `IEATools::psut_cols$R`.
#' @param imports The name of imports flows in the `.tidy_iea_df`.
#'                Default is `IEATools::interface_industries$imports`.
#' @param National_Production_By_Product The name of the column containing national production for each product, by country.
#'                                       Default is "National_Production_By_Product".
#'
#' @return A data frame that returns the national production by product.
#' @export
#'
#' @examples
#' tidy_AB_data %>%
#' IEATools::add_psut_matnames() %>%
#' calc_national_production_by_product() %>%
#' print()
calc_national_production_by_product <- function(.tidy_iea_df,
                                                ledger_side = IEATools::iea_cols$ledger_side,
                                                flow_aggregation_point = IEATools::iea_cols$flow_aggregation_point,
                                                flow = IEATools::iea_cols$flow,
                                                e_dot = IEATools::iea_cols$e_dot,
                                                matnames = IEATools::mat_meta_cols$matnames,
                                                V_matrix = IEATools::psut_cols$V,
                                                R_matrix = IEATools::psut_cols$R,
                                                imports = IEATools::interface_industries$imports,
                                                National_Production_By_Product = "National_Production_By_Product"){
  .tidy_iea_df %>%
    dplyr::filter((matnames == V_matrix | matnames == R_matrix) & (! stringr::str_detect(.data[[flow]], imports))) %>%
    dplyr::ungroup() %>%
    dplyr::select(-tidyselect::all_of(c(ledger_side, flow_aggregation_point, flow, matnames))) %>%
    matsindf::group_by_everything_except(e_dot) %>%
    dplyr::summarise(
      "{National_Production_By_Product}" := sum(.data[[e_dot]])
    )
}



#' Calculates for each product the share of global production by country
#'
#' This function calculates for each product the share of global production by country.
#'
#' The calculation is done using sequentially the functions:
#' * `calc_global_production_by_product()`;
#' * `calc_national_production_by_product()`;
#' and then calculating shares.
#'
#' Note: the function needs to have a column indicating matrix names added first, most likely using the `IEATools::add_psut_matnames()` function.
#'
#' @param .tidy_iea_df The `.tidy_iea_df` for which the shares of global production by country, for each product, need to be calculated.
#' @param country,year,method,energy_type,last_stage,ledger_side,flow_aggregation_point,flow,product,e_dot,unit See `IEATools::iea_cols`.
#' @param matnames The column name for matrices names.
#'                 Default is `IEATools::mat_meta_cols$matnames`.
#' @param National_Production_By_Product The name of a temporary column that contains the national production by product.
#' @param Global_Production_By_Product The name of a temporary column that contains the global production by product.
#' @param Share_Global_Production_By_Product The name of the column containing the share of global production.
#' @param Producing_Country The name of the column containing the name of the producing country.
#'
#' @return A data frame representing the share of global production for each product, by country.
#' @export
#'
#' @examples
#' tidy_AB_data %>%
#' IEATools::add_psut_matnames() %>%
#' calc_share_global_production_by_product() %>%
#' print()
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
                                                    matnames = IEATools::mat_meta_cols$matnames,
                                                    National_Production_By_Product = "National_Production_By_Product",
                                                    Global_Production_By_Product = "Global_Production_By_Product",
                                                    Share_Global_Production_By_Product = "Share_Global_Production_By_Product",
                                                    Producing_Country = "Producing_Country"){

  share_global_production_by_product <- dplyr::left_join(
    calc_global_production_by_product(.tidy_iea_df,
                                      Global_Production_By_Product = Global_Production_By_Product),
    calc_national_production_by_product(.tidy_iea_df,
                                        National_Production_By_Product = National_Production_By_Product),
    by = c({method}, {energy_type}, {last_stage}, {year}, {product}, {unit})
  ) %>%
    dplyr::mutate(
      "{Share_Global_Production_By_Product}" := .data[[National_Production_By_Product]] / .data[[Global_Production_By_Product]]
    ) %>%
    dplyr::select(-tidyselect::all_of(c(National_Production_By_Product, Global_Production_By_Product))) %>%
    dplyr::rename(
      "{Producing_Country}" := tidyselect::all_of(country)
    )

  return(share_global_production_by_product)
}




#' Calculates global exports by product
#'
#' The function calculates global exports by product.
#'
#' @param .tidy_iea_df The `.tidy_iea_df` for which global exports by product need to be calculated.
#' @param flow,method,energy_type,last_stage,year,ledger_side,flow_aggregation_point,product,e_dot See `IEATools::iea_cols`.
#' @param exports The name of exports flows in the `.tidy_iea_df`.
#'                Default is `IEATools::interface_industries$exports`.
#' @param Global_Exports_By_Product The name of the column containing global exports by product.
#'
#' @return A data frame reporting global exports by product.
#' @export
#'
#' @examples
#' tidy_AB_data %>%
#' calc_global_exports() %>%
#' print()
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
                                exports = IEATools::interface_industries$exports,
                                Global_Exports_By_Product = "Global_Exports_By_Product"){


  tidy_global_exports_by_product <- .tidy_iea_df %>%
    dplyr::filter(stringr::str_detect(.data[[flow]], exports)) %>%
    dplyr::mutate(
      "{e_dot}" := abs(.data[[e_dot]])
    ) %>%
    dplyr::group_by(.data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[ledger_side]], .data[[flow_aggregation_point]],
                    .data[[product]]) %>%
    dplyr::summarise(
      "{Global_Exports_By_Product}" := sum(.data[[e_dot]])
    )

  return(tidy_global_exports_by_product)
}



#' Calculates the share of exports for each product, by country
#'
#' This function calculates the share of exports for each product, by country.
#'
#' The function calls first the `calc_global_exports()` function.
#'
#' Note: the function needs to have a column indicating matrix names added first, most likely using the `IEATools::add_psut_matnames()` function.
#'
#' @param .tidy_iea_df The `.tidy_iea_df` for which the shares of exports for each product need to be calculated.
#' @param flow,country,method,energy_type,last_stage,year,ledger_side,flow_aggregation_point,product,e_dot,unit See `IEATools::iea_cols`.
#' @param exports The name of exports flows in the `.tidy_iea_df`.
#'                Default is `IEATools::interface_industries$exports`.
#' @param matnames The column name for matrices names.
#'                 Default is `IEATools::mat_meta_cols$matnames`.
#' @param provenience The name of the column reporting the country of provenience, i.e. the exporting country.
#' @param Global_Exports_By_Product The name of a temporary column that contains global exports by product.
#' @param Share_Exports_By_Product The name of the column that contains the share of exports for each product, by country.
#'
#' @return A data frame that reports the share of exports for each product, by country.
#' @export
#'
#' @examples
#' tidy_AB_data %>%
#' IEATools::add_psut_matnames() %>%
#' calc_share_exports_by_product() %>%
#' print()
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
                                          provenience = "Provenience",
                                          Global_Exports_By_Product = "Global_Exports_By_Product",
                                          Share_Exports_By_Product = "Share_Exports_By_Product"){

  tidy_share_exports_by_product <- .tidy_iea_df %>%
    dplyr::filter(stringr::str_detect(.data[[flow]], exports)) %>%
    dplyr::mutate(
      "{e_dot}" := abs(.data[[e_dot]])
    ) %>%
    dplyr::left_join(calc_global_exports(.tidy_iea_df,
                                         Global_Exports_By_Product = Global_Exports_By_Product), by = c({method}, {energy_type}, {last_stage}, {year}, {ledger_side}, {flow_aggregation_point}, {product})) %>%
    dplyr::mutate(
      "{Share_Exports_By_Product}" := .data[[e_dot]] / .data[[Global_Exports_By_Product]]
    ) %>%
    dplyr::rename("{provenience}" := tidyselect::all_of(country)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-tidyselect::all_of(c(ledger_side, flow_aggregation_point, flow, unit, e_dot, matnames, Global_Exports_By_Product)))

  return(tidy_share_exports_by_product)
}


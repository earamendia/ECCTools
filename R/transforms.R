
#' Specifies Multi-Regional R matrix
#'
#' This function specified the Multi-Regional Resources matrix.
#' Only flows that belong to the Multi-Regional Resources matrix are returned,
#' other flows are filtered out.
#'
#' The specification process is to modify both the flow and the product produced
#' so that the producing country is added as prefix using brackets first.
#' Note: matrix names need to be added, most likely using the `IEATools::add_psut_matnames`, prior to using the function.
#'
#' @param .tidy_iea_df The `.tidy_iea_df` from which the Multi-Regional R matrix needs to be created.
#' @param R_matrix The name of the R matrix.
#'                 Default is `IEATools::psut_cols$R`.
#' @param matnames The name of the matrix names column.
#'                 Default is `IEATools::mat_meta_cols$matnames`.
#' @param imports The name of the Imports flows in the `.tidy_iea_df`.
#'                Default is `IEATools::interface_industries$imports`.
#' @param country,flow,product See `IEATools::iea_cols`.
#' @param aggregate_country_name The name of the new region that gathers all flows of the `.tidy_iea_df`.
#'                               Default is "World".
#'
#' @return A `.tidy_iea_df` representing the Multi-Regional Resources matrix, with all flows specified by country.
#' @export
#'
#' @examples
#' tidy_AB_data %>%
#'  IEATools::add_psut_matnames() %>%
#'  specify_MR_R() %>%
#'  print()
specify_MR_R <- function(.tidy_iea_df,
                         R_matrix = IEATools::psut_cols$R,
                         matnames = IEATools::mat_meta_cols$matnames,
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



#' Specifies Multi-Regional V matrix
#'
#' This function specified the Multi-Regional Supply (V) matrix.
#' Only flows that belong to the Multi-Regional Supply matrix are returned,
#' other flows are filtered out. Exceptions are flows akin to supply (with E.dot < 0) but that have been
#' previously re-directed to the Epsilon matrix by the analyst - those are also kept.
#'
#' The specification process is to modify both the flow and the product produced
#' so that the producing country is added as prefix using brackets first.
#' Note: matrix names need to be added, most likely using the `IEATools::add_psut_matnames`, prior to using the function.
#'
#' @param .tidy_iea_df The `.tidy_iea_df` from which the Multi-Regional R matrix needs to be created.
#' @param V_matrix The name of the V matrix.
#'                 Default is `IEATools::psut_cols$V`.
#' @param Epsilon The name of the Epsilon matrix.
#'                Default is `IEATools::psut_cols$epsilon`.
#' @param matnames The name of the matrix names column.
#'                 Default is `IEATools::mat_meta_cols$matnames`.
#' @param imports The name of the Imports flows in the `.tidy_iea_df`.
#'                Default is `IEATools::interface_industries$imports`.
#' @param country,flow,product,e_dot See `IEATools::iea_cols`.
#' @param aggregate_country_name The name of the new region that gathers all flows of the `.tidy_iea_df`.
#'                               Default is "World".
#'
#' @return A `.tidy_iea_df` representing the Multi-Regional Supply (V) matrix, with all flows specified by country.
#' @export
#'
#' @examples
#' tidy_AB_data %>%
#'  IEATools::add_psut_matnames() %>%
#'  specify_MR_V() %>%
#'  print()
specify_MR_V <- function(.tidy_iea_df,
                         V_matrix = IEATools::psut_cols$V,
                         Epsilon_matrix = IEATools::psut_cols$epsilon,
                         matnames = IEATools::mat_meta_cols$matnames,
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



#' Specifies imported and domestic products
#'
#' This function specifies imported and domestic products in the Y, U_EIOU, U_feed, and Epsilon matrices.
#' For instance, a flow of "Gasoline" will be broken down in two flows: one of imported gasoline, and one of domestic gasoline.
#' The origin is specified in a new column. One of the two flows may be absent, in the case of a product that is not imported,
#' or not domestically produced.
#'
#' Only flows belonging to the Y, U_eiou, U_feed, or Epsilon matrices are returned.
#' Note that matrices names need to be added first, most likely using the `IEATools::add_psut_matnames()` function.
#'
#' @param .tidy_iea_df The `.tidy_iea_df` that needs being specified.
#' @param V_matrix The name of the V matrix.
#'                 Default is `IEATools::psut_cols$V`.
#' @param Y_matrix The name of the Y matrix.
#'                 Default is `IEATools::psut_cols$Y`.
#' @param U_feed_matrix The name of the U_feed matrix.
#'                 Default is `IEATools::psut_cols$U_feed`.
#' @param U_EIOU_matrix The name of the U_eiou matrix.
#'                 Default is `IEATools::psut_cols$U_eiou`.
#' @param Epsilon_matrix The name of the Epsilon matrix.
#'                 Default is `IEATools::psut_cols$epsilon`.
#' @param matnames The name of the matrix names column.
#'                 Default is `IEATools::mat_meta_cols$matnames`.
#' @param domestic The string that indicates that the product is of domestic origin in the new origin column.
#'                 Default is "Domestic".
#' @param imported The string that indicates that the product is of imported origin in the new origin column.
#'                 Default is "Imported".
#' @param origin The name of the colupn that specifies the origin of each product.
#' @param flow,country,method,energy_type,last_stage,year,product,unit,e_dot See `IEATools::iea_cols`.
#' @param exports The name of the Exports flows in the `.tidy_iea_df`.
#'                Default is `IEATools::interface_industries$exports`.
#' @param imports The name of the Imports flows in the `.tidy_iea_df`.
#'                Default is `IEATools::interface_industries$imports`.
#'
#' @return A `.tidy_iea_df` that includes only flows belonging to the Y, U_eiou, U_feed, or Epsilon matrices,
#'         and for which flows are specified in terms of flows or imported or domestic products.
#' @export
#'
#' @examples
#' .tidy_AB_df %>%
#' IEATools::add_psut_matnames() %>%
#' specify_imported_products() %>%
#' print()
specify_imported_products <- function(.tidy_iea_df,
                                      V_matrix = IEATools::psut_cols$V,
                                      Y_matrix = IEATools::psut_cols$Y,
                                      U_feed_matrix = IEATools::psut_cols$U_feed,
                                      U_EIOU_matrix = IEATools::psut_cols$U_eiou,
                                      Epsilon_matrix = IEATools::psut_cols$epsilon,
                                      matnames = IEATools::mat_meta_cols$matnames,
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




#' Specifies Multi-Regional final demand (Y) and use (U) matrices with Global Market Assumption
#'
#' This function specifies flows belonging to the Y, U_feed, U_eiou, and Epsilon (only for those flows akin to final demand) matrices,
#' according to the Global Market Assumption. See details for more explanations.
#'
#' First, each flow is separated into a flow of domestic product and imported product, using the `specify_imported_products()` function.
#' Then, the `calc_share_exports_by_product()` function is used to calculate the share of global exports, for each product, by country.
#' These shares is used to specify the importations of each country, for each product, hence the assumption of a global market.
#'
#' Only flows belonging to the Y, U_feed, U_eiou, and Epsilon matrices are returned.
#' Note that matrices names need to be added first, most likely using the `IEATools::add_psut_matnames()` function.
#'
#' @param .tidy_iea_df The `.tidy_iea_df` from which the Multi-Regional Y and U matrices needs to be created.
#' @param flow,product,method,year,energy_type,last_stage,e_dot,unit,country See `IEATools::iea_cols`.
#' @param aggregate_country_name The name of the new aggregate region for which the Multi-Regional tables are created.
#'                               Default is "World".
#' @param domestic The string that indicates that the product is of domestic origin in the new origin column.
#'                 Default is "Domestic".
#' @param imported The string that indicates that the product is of imported origin in the new origin column.
#'                 Default is "Imported".
#' @param provenience The name of a temporary column that provides the provenience of each imported flow, according to the Global Market Assumption.
#'                    Default is "Provenience".
#'
#' @return A `.tidy_iea_df` with flows corresponding to the Y, U_feed, U_eiou, and Epsilon matrices are specified.
#' @export
#'
#' @examples
#' .tidy_AB_df %>%
#' IEATools::add_psut_matnames() %>%
#' specify_MR_Y_U_gma() %>%
#' print()
specify_MR_Y_U_gma <- function(.tidy_iea_df,
                               flow = IEATools::iea_cols$flow,
                               product = IEATools::iea_cols$product,
                               year = IEATools::iea_cols$year,
                               method = IEATools::iea_cols$method,
                               energy_type = IEATools::iea_cols$energy_type,
                               last_stage = IEATools::iea_cols$last_stage,
                               e_dot = IEATools::iea_cols$e_dot,
                               unit = IEATools::iea_cols$unit,
                               country = IEATools::iea_cols$country,
                               aggregate_country_name = "World",
                               domestic = "Domestic",
                               imported = "Imported",
                               provenience = "Provenience"){

  # (1) Differentiating domestically produced and imported products in the Y and U matrices flows
  tidy_iea_df_specified_imports <- .tidy_iea_df %>%
    specify_imported_products()

  # (2) Specifying domestic consumption
  tidy_domestic_consumption_MR_gma <- tidy_iea_df_specified_imports %>%
    dplyr::filter(Origin == domestic) %>%
    dplyr::mutate(
      "{flow}" := paste0("{", .data[[country]], "}_", .data[[flow]]),
      "{product}" := paste0("{", .data[[country]], "}_", .data[[product]]),
      "{country}" := aggregate_country_name
    ) %>%
    dplyr::select(-Origin)


  # (3) Specifying foreign consumption
  tidy_imported_consumption_MR_gma_with_nas <- tidy_iea_df_specified_imports %>%
    dplyr::filter(Origin == imported) %>%
    dplyr::left_join(calc_share_exports_by_product(.tidy_iea_df),
                     by = c({method}, {energy_type}, {last_stage}, {year}, {product})) %>%
    dplyr::mutate(
      "{e_dot}" := dplyr::case_when(
        is.na(Share_Exports_From_Func) ~ .data[[e_dot]],
        TRUE ~ .data[[e_dot]] * Share_Exports_From_Func
      ),
      "{flow}" := paste0("{", .data[[country]], "}_", .data[[flow]]),
      "{product}" := paste0("{", .data[[provenience]], "}_", .data[[product]]),
      "{country}" := aggregate_country_name
    ) %>%
    dplyr::select(-.data[[provenience]], -.data[["Share_Exports_From_Func"]], -.data[["Origin"]])

  # (4) When a given product is imported by a given country, but no country exports such product, then calculate the global production mix.
  tidy_imported_consumption_MR_gma_whout_nas <- tidy_imported_consumption_MR_gma_with_nas %>%
    dplyr::filter(stringr::str_detect(.data[[product]], "\\{NA\\}_")) %>%
    dplyr::mutate(
      "{product}" := stringr::str_remove(.data[[product]], "\\{NA\\}_")
    ) %>%
    dplyr::left_join(calc_share_global_production_by_product(.tidy_iea_df),
                     by = c({method}, {energy_type}, {last_stage}, {year}, {product}, {unit})) %>%
    dplyr::mutate(
      "{e_dot}" := .data[[e_dot]] * Share_Global_Production_From_Func,
      "{product}" := stringr::str_c("{", .data[["Producing_Country_From_Func"]], "}_", .data[[product]])
    ) %>%
    dplyr::select(-.data[["Producing_Country_From_Func"]], -.data[["Share_Global_Production_From_Func"]])

  # (5) Bind rows.
  tidy_imported_consumption_MR_gma <- tidy_imported_consumption_MR_gma_with_nas %>%
    dplyr::filter(! stringr::str_detect(.data[[product]], "\\{NA\\}_")) %>%
    dplyr::bind_rows(tidy_imported_consumption_MR_gma_whout_nas)

  # (6) Testing if we have any NAs in the join...
  if (NA %in% tidy_imported_consumption_MR_gma[[e_dot]]){
    stop("There an NA in the join here, do worry about it.")# How do I get code coverage here?
  }

  # Right. And here we need to "gather" flows of the same things, are these may appear in (4) if the country also produces locally the product.
  tidy_consumption_MR_gma <- dplyr::bind_rows(tidy_domestic_consumption_MR_gma, tidy_imported_consumption_MR_gma) %>%
    matsindf::group_by_everything_except(e_dot) %>%
    dplyr::summarise(
      "{e_dot}" := sum(.data[[e_dot]])
    ) %>%
    dplyr::ungroup()

  return(tidy_consumption_MR_gma)

}



# Transform to gma function
#' Title
#'
#' @param .tidy_iea_df
#'
#' @return
#' @export
#'
#' @examples
transform_to_gma <- function(.tidy_iea_df){

  # (1) Create MR-R matrix data frame
  MR_R_gma <- specify_MR_R(.tidy_iea_df)

  # (2) Creating MR-V matrix data frame
  MR_V_gma <- specify_MR_V(.tidy_iea_df)

  # (3) Creating MR-Y and MR-U matrix data frames, with GMA assumption
  MR_Y_U_gma <- specify_MR_Y_U_gma(.tidy_iea_df)

  # Binding all rows and returning full data frame
  tidy_iea_MR_gma_df <- dplyr::bind_rows(MR_R_gma, MR_V_gma, MR_Y_U_gma) %>%
    dplyr::ungroup()

  return(tidy_iea_MR_gma_df)
}



# This function calculates the bilateral trade matrix data frame using the GMA assumption.
# Basically it has two purposes:
# (i) if the user doesn't load a trade matrix in the specify_MR_Y_U_bta() function, then the default trade matrix is the one built from the GMA assumption;
# (ii) if the user loads an incomplete trade matrix in the specify_MR_Y_U_bta() function (likely as trade data is not available for all years!);
# then the missing information is loaded from the GMA bilateral trade matrix.

#' Title
#'
#' @param .tidy_iea_df
#' @param year
#' @param method
#' @param energy_type
#' @param last_stage
#' @param country
#' @param provenience
#' @param matnames
#'
#' @return
#' @export
#'
#' @examples
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

#' Title
#'
#' @param .tidy_iea_df
#' @param bilateral_trade_matrix_df
#' @param flow
#' @param product
#' @param year
#' @param method
#' @param energy_type
#' @param last_stage
#' @param e_dot
#' @param country
#' @param unit
#' @param aggregate_country_name
#' @param provenience
#'
#' @return
#' @export
#'
#' @examples
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
                               unit = IEATools::iea_cols$unit,
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
  tidy_imported_consumption_with_gma_bt_matrix_with_nas <- tidy_iea_df_specified_imports %>%
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
      "{e_dot}" := dplyr::case_when(
        is.na(Share_Exports_From_Func) ~ .data[[e_dot]],
        TRUE ~ .data[[e_dot]] * Share_Exports_From_Func
      ),
      "{flow}" := paste0("{", .data[[country]], "}_", .data[[flow]]),
      "{product}" := paste0("{", .data[[provenience]], "}_", .data[[product]]),
      "{country}" := aggregate_country_name
    ) %>%
    dplyr::select(-.data[[provenience]], -.data[["Share_Exports_From_Func"]], -.data[["Origin"]])


  # (3.iii) Where there are NAs - i.e. where an imported product is not exported by any country, use the global production mix!
  tidy_imported_consumption_with_gma_bt_matrix_whout_nas <- tidy_imported_consumption_with_gma_bt_matrix_with_nas %>%
    dplyr::filter(stringr::str_detect(.data[[product]], "\\{NA\\}_")) %>%
    dplyr::mutate(
      "{product}" := stringr::str_remove(.data[[product]], "\\{NA\\}_")
    ) %>%
    dplyr::left_join(calc_share_global_production_by_product(.tidy_iea_df),
                     by = c({method}, {energy_type}, {last_stage}, {year}, {product}, {unit})) %>%
    dplyr::mutate(
      "{e_dot}" := .data[[e_dot]] * Share_Global_Production_From_Func,
      "{product}" := stringr::str_c("{", .data[["Producing_Country_From_Func"]], "}_", .data[[product]])
    ) %>%
    dplyr::select(-.data[["Producing_Country_From_Func"]], -.data[["Share_Global_Production_From_Func"]])


  # (3.iv) Bind rows of the tidy_imported_consumption_with_gma_bt_matrix data frame
  tidy_imported_consumption_with_gma_bt_matrix <- tidy_imported_consumption_with_gma_bt_matrix_with_nas %>%
    dplyr::filter(! stringr::str_detect(.data[[product]], "\\{NA\\}_")) %>%
    dplyr::bind_rows(tidy_imported_consumption_with_gma_bt_matrix_whout_nas)

  if (NA %in% tidy_imported_consumption_with_bt_matrix[[e_dot]]){
    stop("There an NA in the join here, do worry about it.")# How do I get code coverage here?
  }

  # (3.vi) Binding data frames computed with bt matrix and with gma trade matrix
  tidy_imported_consumption_MR_bta <- dplyr::bind_rows(tidy_imported_consumption_with_bt_matrix,
                                                       tidy_imported_consumption_with_gma_bt_matrix)


  # Binding domestic and foreign consumption data frames together and returning result
  tidy_consumption_MR_bta <- dplyr::bind_rows(tidy_domestic_consumption_MR_bta,
                                              tidy_imported_consumption_MR_bta) %>%
    matsindf::group_by_everything_except(e_dot) %>%
    dplyr::summarise(
      "{e_dot}" := sum(.data[[e_dot]])
    ) %>%
    dplyr::ungroup()

  return(tidy_consumption_MR_bta)
}



# Transform to bta function
#' Title
#'
#' @param .tidy_iea_df
#' @param bilateral_trade_matrix_df
#'
#' @return
#' @export
#'
#' @examples
transform_to_bta <- function(.tidy_iea_df,
                             bilateral_trade_matrix_df = calc_bilateral_trade_matrix_df_gma(.tidy_iea_df)){

  # (1) Create MR-R matrix data frame
  MR_R_bta <- specify_MR_R(.tidy_iea_df)

  # (2) Creating MR-V matrix data frame
  MR_V_bta <- specify_MR_V(.tidy_iea_df)

  # (3) Creating MR-Y and MR-U matrix data frames, with GMA assumption
  MR_Y_U_bta <- specify_MR_Y_U_bta(.tidy_iea_df, bilateral_trade_matrix_df = bilateral_trade_matrix_df)

  # Binding all rows and returning full data frame
  tidy_iea_MR_bta_df <- dplyr::bind_rows(MR_R_bta, MR_V_bta, MR_Y_U_bta) %>%
    dplyr::ungroup()

  return(tidy_iea_MR_bta_df)
}



# Find list of observations for which the DTA can be implemented

#' Title
#'
#' @param .tidy_iea_df
#' @param country
#' @param method
#' @param energy_type
#' @param last_stage
#' @param year
#' @param flow
#' @param product
#' @param imports
#' @param matnames
#' @param y_matrix
#' @param u_eiou_matrix
#' @param u_feed_matrix
#' @param v_matrix
#' @param r_matrix
#'
#' @return
#' @export
#'
#' @examples
find_list_dta_observations <- function(.tidy_iea_df,
                                       products_to_look_for = IEATools::products,
                                       requirement_matrices_list = c(IEATools::psut_cols$Y, IEATools::psut_cols$U_eiou, IEATools::psut_cols$U_feed),
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
    dplyr::filter(.data[[matnames]] %in% requirement_matrices_list) %>% #These are products used
    dplyr::filter(.data[[product]] %in% products_to_look_for) %>%
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

#' Title
#'
#' @param .tidy_iea_df
#' @param country
#' @param method
#' @param energy_type
#' @param last_stage
#' @param year
#' @param flow
#' @param ledger_side
#' @param e_dot
#' @param imports
#' @param matnames
#' @param epsilon
#'
#' @return
#' @export
#'
#' @examples
transform_to_dta <- function(.tidy_iea_df,
                             products_to_look_for = IEATools::products,
                             requirement_matrices_list = c(IEATools::psut_cols$Y, IEATools::psut_cols$U_eiou, IEATools::psut_cols$U_feed),
                             select_dta_observations = TRUE,
                             country = IEATools::iea_cols$country,
                             method = IEATools::iea_cols$method,
                             energy_type = IEATools::iea_cols$energy_type,
                             last_stage = IEATools::iea_cols$last_stage,
                             year = IEATools::iea_cols$year,
                             flow = IEATools::iea_cols$flow,
                             ledger_side = IEATools::iea_cols$ledger_side,
                             e_dot = IEATools::iea_cols$e_dot,
                             imports = IEATools::interface_industries$imports,
                             matnames = IEATools::mat_meta_cols$matnames,
                             epsilon = "Epsilon"){

  if (select_dta_observations == TRUE){
    list_dta_observations <- .tidy_iea_df %>%
      find_list_dta_observations(products_to_look_for = products_to_look_for,
                                 requirement_matrices_list = requirement_matrices_list)

    tidy_iea_dta_df <- .tidy_iea_df %>%
      dplyr::filter(stringr::str_c(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], sep = "_") %in% list_dta_observations) %>%
      dplyr::mutate(
        "{matnames}" := dplyr::case_when(
          stringr::str_detect(.data[[flow]], imports) ~ epsilon,
          TRUE ~ .data[[matnames]]
        ),
        "{e_dot}" := dplyr::case_when(
          stringr::str_detect(.data[[flow]], imports) ~ -.data[[e_dot]],
          TRUE ~ .data[[e_dot]]
        )
      ) %>%
      dplyr::ungroup()

    return(tidy_iea_dta_df)
  } else {

    tidy_iea_dta_df <- .tidy_iea_df %>%
      dplyr::mutate(
        "{matnames}" := dplyr::case_when(
          stringr::str_detect(.data[[flow]], imports) ~ epsilon,
          TRUE ~ .data[[matnames]]
        ),
        "{e_dot}" := dplyr::case_when(
          stringr::str_detect(.data[[flow]], imports) ~ -.data[[e_dot]],
          TRUE ~ .data[[e_dot]]
        )
      ) %>%
      dplyr::ungroup()

    return(tidy_iea_dta_df)
  }
}



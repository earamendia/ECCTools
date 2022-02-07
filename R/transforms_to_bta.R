
#' Checks bilateral trade data
#'
#' This function checks the bilateral trade data before it is provided to the transform_to_bta() function.
#' It returns TRUE if the sum of the shares of exported products, by country of provenience and by product, is 1.
#' Otherwise, it returns an error.
#'
#'
#' @param .bilateral_trade_df The bilateral trade data frame to be checked.
#' @param country The name of the country column.
#'                Default is `IEATools::iea_cols$country`.
#' @param provenience The name of the country of provenience (i.e. exporting country).
#'                    Default is "Provenience".
#' @param method The name of the Method column.
#'               Default is `IEATools::iea_cols$method`.
#' @param energy_type The name of the energy type column.
#'                    Default is `IEATools::iea_cols$energy_type`.
#' @param last_stage The name of the last stage column.
#'                   Default is `IEATools::iea_cols$last_stage`.
#' @param year The name of the year column.
#'             Default is `year = IEATools::iea_cols$year`.
#' @param product The name of the product column.
#'                Default is `IEATools::iea_cols$product`.
#' @param e_dot The name of the E.dot column.
#'               Default is `IEATools::iea_cols$e_dot`.
#' @param share_exports_by_product The name of the share of exports by product column.
#'                                 Default is "Share_Exports_By_Product".
#' @param .sum_share_exports The name of the temporary sum shares of exports by product column.
#'                           Default is ".sum_share_exports".
#'
#' @return Returns TRUE if the sum of the shares of exported products, by country of provenience and by product, is 1.
#'         Returns an error if otherwise.
#' @export
#'
#' @examples
#' A_B_path <- system.file("extdata/A_B_data_full_2018_format.csv", package = "ECCTools")
#' AB_data <- A_B_path %>%
#'  IEATools::load_tidy_iea_df() %>%
#'  IEATools::specify_all()
#' bilateral_trade_df_gma <- AB_data %>%
#'  IEATools::add_psut_matnames() %>%
#'  calc_bilateral_trade_df_gma()
#' check_bilateral_trade_df(bilateral_trade_df_gma)
check_bilateral_trade_df <- function(.bilateral_trade_df,
                                     country = IEATools::iea_cols$country,
                                     provenience = "Provenience",
                                     method = IEATools::iea_cols$method,
                                     energy_type = IEATools::iea_cols$energy_type,
                                     last_stage = IEATools::iea_cols$last_stage,
                                     year = IEATools::iea_cols$year,
                                     product = IEATools::iea_cols$product,
                                     e_dot = IEATools::iea_cols$e_dot,
                                     share_exports_by_product = "Share_Exports_By_Product",
                                     .sum_share_exports = ".sum_share_exports"){

  # First, sum by exporting country, by product
  bilateral_trade_df_testing_res <- .bilateral_trade_df %>%
    dplyr::group_by(.data[[provenience]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[product]]) %>%
    dplyr::summarise(
      "{.sum_share_exports}" := sum(.data[[share_exports_by_product]])
    )

  assertthat::assert_that(all(abs(bilateral_trade_df_testing_res[[.sum_share_exports]] - 1) <= 1e-3),
                          msg = "The sum of shares by product and exporting country is not equal to 1.")


  # Second, sum by importing country, by product
  bilateral_trade_df_testing_res2 <- .bilateral_trade_df %>%
    dplyr::filter(.data[[share_exports_by_product]] != 0) %>%
    dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[product]]) %>%
    dplyr::summarise(
      "{.sum_share_exports}" := sum(.data[[share_exports_by_product]])
    )

  assertthat::assert_that(all(abs(bilateral_trade_df_testing_res2[[.sum_share_exports]] - 1) <= 1e-3),
                          msg = "The sum of shares by product and importing country is not equal to 1.")
}



#' Specifies Multi-Regional final demand (Y) and use (U) matrices with Bilateral Trade Assumption
#'
#' This function specifies flows belonging to the Y, U_feed, U_eiou, and B (only for those flows akin to final demand) matrices,
#' according to the Bilateral Trade Assumption. See details for more explanations.
#'
#' First, each flow is separated into a flow of domestic product and imported product, using the `specify_imported_products()` function.
#' Then, the bilateral trade data passed as `bilateral_trade_df` argument, is used to specify the flows that are imported.
#'
#' Only flows belonging to the Y, U_feed, U_eiou, and B matrices are returned.
#' Note that matrices names need to be added first, most likely using the `IEATools::add_psut_matnames()` function.
#'
#' @param .tidy_iea_df The `.tidy_iea_df` from which the Multi-Regional Y and U matrices needs to be created.
#' @param bilateral_trade_df The bilateral trade data to be used to specify Y and U matrices flows.
#'                                  Default is bilateral trade data corresponding to the Global Market Assumption, calculated as `calc_bilateral_trade_df_gma(.tidy_iea_df)`.
#' @param flow,product,year,method,energy_type,last_stage,e_dot,country,unit See `IEATools::iea_cols`.
#' @param aggregate_country_name The name of the new region that gathers all flows of the `.tidy_iea_df`.
#'                               Default is "World".
#' @param provenience The name of the temporary column that specifies the origin of a given flow.
#'                    Default is "Provenience".
#' @param origin The name of the column specifying whether a given flow comes refers to a domestic or imported product.
#'               Default is "Origin".
#' @param domestic The string that indicates that the product is of domestic origin in the new origin column.
#'                 Default is "Domestic".
#' @param imported The string that indicates that the product is of imported origin in the new origin column.
#'                 Default is "Imported".
#' @param Share_Exports_By_Product The name of a temporary column that contains the share of global exports by country, for each product.
#'                                 Default is "Share_Exports_By_Product".
#' @param Producing_Country The name of a temporary column that contains the provenance country for a given flow, i.e. the exporting country of a given flow.
#'                          Default is "Producing_Country".
#' @param Share_Global_Production_By_Product The name if a temporary column that contains the share of global product by country, for each product.
#'                                           Default is "Share_Global_Production_By_Product".
#'
#'
#' @return A `.tidy_iea_df` with flows corresponding to the Y, U_feed, U_eiou, and B matrices are specified.
#' @export
#'
#' @examples
#' tidy_AB_data %>%
#' IEATools::add_psut_matnames() %>%
#' specify_MR_Y_U_bta() %>%
#' print()
specify_MR_Y_U_bta <- function(.tidy_iea_df,
                               bilateral_trade_df = calc_bilateral_trade_df_gma(.tidy_iea_df),
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
                               provenience = "Provenience",
                               origin = "Origin",
                               domestic = "Domestic",
                               imported = "Imported",
                               Share_Exports_By_Product = "Share_Exports_By_Product",
                               Producing_Country = "Producing_Country",
                               Share_Global_Production_By_Product = "Share_Global_Production_By_Product"
){


  # (1) Differentiating domestically produced and imported products in the Y and U matrices flows
  tidy_iea_df_specified_imports <- .tidy_iea_df %>%
    specify_imported_products()

  # (2) Specifying domestic consumption
  tidy_domestic_consumption_MR_bta <- tidy_iea_df_specified_imports %>%
    dplyr::filter(.data[[origin]] == "Domestic") %>%
    dplyr::mutate(
      "{flow}" := paste0("{", .data[[country]], "}_", .data[[flow]]),
      "{product}" := paste0("{", .data[[country]], "}_", .data[[product]]),
      "{country}" := aggregate_country_name
    ) %>%
    dplyr::select(-.data[[origin]])

  # (3.i) Specifying foreign consumption with the provided trade matrix
  tidy_imported_consumption_with_bt_matrix <- tidy_iea_df_specified_imports %>%
    dplyr::filter(.data[[origin]] == "Imported") %>%
    dplyr::left_join(bilateral_trade_df,
                     by = c({country}, {method}, {energy_type}, {last_stage}, {year}, {product})) %>%
    dplyr::filter(! is.na(.data[[Share_Exports_By_Product]])) %>%
    dplyr::mutate(
      "{e_dot}" := .data[[e_dot]] * Share_Exports_By_Product,
      "{flow}" := paste0("{", .data[[country]], "}_", .data[[flow]]),
      "{product}" := paste0("{", .data[[provenience]], "}_", .data[[product]]),
      "{country}" := aggregate_country_name
    ) %>%
    dplyr::select(-.data[[provenience]], -.data[[Share_Exports_By_Product]], -.data[[origin]])

  # (3.ii) Specifying the rest with the global market assumption GMA
  tidy_imported_consumption_with_gma_bt_matrix_with_nas <- tidy_iea_df_specified_imports %>%
    dplyr::filter(.data[[origin]] == "Imported") %>%
    dplyr::left_join(bilateral_trade_df,
                     by = c({country}, {method}, {energy_type}, {last_stage}, {year}, {product})) %>%
    dplyr::filter(is.na(.data[[Share_Exports_By_Product]])) %>%
    dplyr::select(-.data[[provenience]], -.data[[Share_Exports_By_Product]]) %>%
    dplyr::left_join(
      calc_bilateral_trade_df_gma(.tidy_iea_df),
      by = c({country}, {method}, {energy_type}, {last_stage}, {year}, {product})
    ) %>%
    dplyr::mutate(
      "{e_dot}" := dplyr::case_when(
        is.na(Share_Exports_By_Product) ~ .data[[e_dot]],
        TRUE ~ .data[[e_dot]] * Share_Exports_By_Product
      ),
      "{flow}" := paste0("{", .data[[country]], "}_", .data[[flow]]),
      "{product}" := paste0("{", .data[[provenience]], "}_", .data[[product]]),
      "{country}" := aggregate_country_name
    ) %>%
    dplyr::select(-.data[[provenience]], -.data[[Share_Exports_By_Product]], -.data[[origin]])


  # (3.iii) Where there are NAs - i.e. where an imported product is not exported by any country, use the global production mix!
  tidy_imported_consumption_with_gma_bt_matrix_whout_nas <- tidy_imported_consumption_with_gma_bt_matrix_with_nas %>%
    dplyr::filter(stringr::str_detect(.data[[product]], "\\{NA\\}_")) %>%
    dplyr::mutate(
      "{product}" := stringr::str_remove(.data[[product]], "\\{NA\\}_")
    ) %>%
    dplyr::left_join(calc_share_global_production_by_product(.tidy_iea_df),
                     by = c({method}, {energy_type}, {last_stage}, {year}, {product}, {unit})) %>%
    dplyr::mutate(
      "{e_dot}" := .data[[e_dot]] * Share_Global_Production_By_Product,
      "{product}" := stringr::str_c("{", .data[["Producing_Country"]], "}_", .data[[product]])
    ) %>%
    dplyr::select(-.data[[Producing_Country]], -.data[[Share_Global_Production_By_Product]])


  # (3.iv) Bind rows of the tidy_imported_consumption_with_gma_bt_matrix data frame
  tidy_imported_consumption_with_gma_bt_matrix <- tidy_imported_consumption_with_gma_bt_matrix_with_nas %>%
    dplyr::filter(! stringr::str_detect(.data[[product]], "\\{NA\\}_")) %>%
    dplyr::bind_rows(tidy_imported_consumption_with_gma_bt_matrix_whout_nas)

  assertthat::assert_that(
    ! NA %in% tidy_imported_consumption_with_bt_matrix[[e_dot]],
    msg = "There an NA in the join here, do worry about it.")


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



#' Transforms to Bilateral Trade Assumption
#'
#' This function transforms a `.tidy_iea_df` that contains the representation of the Energy Conversion Chain for multiple
#' countries into a new tidy_iea_df that represents a Global Energy Conversion Chain, adopting the Bilateral Trade Assumption,
#' using a bilateral trade data the particular trade data passed as `bilateral_trade_df` argument.
#'
#' This function runs sequentially the following functions:
#' * `specify_MR_R()`;
#' * `specify_MR_V()`;
#' * `specify_MR_Y_U_bta()`;
#' and then binds all rows obtained using `dplyr::bind_rows()`.
#'
#' Note 1: When no bilateral trade data is provided, the function calculates and uses the bilateral trade data associated with
#' the Global Market Assumption.
#'
#' Note 2: When bilateral trade data is provided only for an observation (Country, Product), then bilateral trade data obtained from the Global Market Assumption
#' is used to fill the gap, for that particular (Country, Product) observation.
#'
#' Note 3: running this function to transform to the Bilateral Trade Assumption only makes sense when the country coverage is
#' global, or close to global (i.e. only countries consuming a very small fraction of global energy consumption, and only
#' producing a very small fraction of global energy production, are missing).
#'
#' @param .tidy_iea_df The `.tidy_iea_df` for which the Bilateral Trade Assumption flows need to be calculated according to a given `bilateral_trade_df`.
#' @param bilateral_trade_df The bilateral trade data frame that will be used.
#'
#' @return A `.tidy_iea_df` describing a Global Energy Conversion Chain, adopting a Bilateral Trade Perspective.
#' @export
#'
#' @examples
#' tidy_AB_data %>%
#' IEATools::add_psut_matnames() %>%
#' transform_to_bta() # Here, as we pass an empty bilateral trade matrix,
#' # the result will be equivalent to the Global Market Assumption.
transform_to_bta <- function(.tidy_iea_df,
                             bilateral_trade_df = calc_bilateral_trade_df_gma(.tidy_iea_df)){

  # (1) Create MR-R matrix data frame
  MR_R_bta <- specify_MR_R(.tidy_iea_df)

  # (2) Creating MR-V matrix data frame
  MR_V_bta <- specify_MR_V(.tidy_iea_df)

  # (3) Creating MR-Y and MR-U matrix data frames, with GMA assumption
  MR_Y_U_bta <- specify_MR_Y_U_bta(.tidy_iea_df, bilateral_trade_df = bilateral_trade_df)

  # Binding all rows and returning full data frame
  tidy_iea_MR_bta_df <- dplyr::bind_rows(MR_R_bta, MR_V_bta, MR_Y_U_bta) %>%
    dplyr::ungroup()

  return(tidy_iea_MR_bta_df)
}

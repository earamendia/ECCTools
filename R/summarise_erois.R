# This script writes functions that enable to deal with EROIs as calculated by the Recca package.

# This function extracts product level erois in a tidy format.
#' Title
#'
#' @param .tidy_io_mats
#' @param country
#' @param method
#' @param energy_type
#' @param last_stage
#' @param year
#' @param matnames
#' @param matvals
#' @param eroi_g_p
#' @param eroi_n_p
#' @param eroi_g_p_feed
#' @param eroi_n_p_feed
#' @param type
#' @param boundary
#' @param eroi
#' @param product
#' @param colnames
#' @param rowtypes
#' @param coltypes
#'
#' @return
#' @export
#'
#' @examples
extract_tidy_product_erois <- function(.tidy_io_mats,
                               country = IEATools::iea_cols$country,
                               method = IEATools::iea_cols$method,
                               energy_type = IEATools::iea_cols$energy_type,
                               last_stage = IEATools::iea_cols$last_stage,
                               year = IEATools::iea_cols$year,
                               matnames = IEATools::mat_meta_cols$matnames,
                               matvals = IEATools::mat_meta_cols$matvals,
                               eroi_g_p = "eroi_g_p",
                               eroi_n_p = "eroi_n_p",
                               eroi_g_p_feed = "eroi_g_p_feed",
                               eroi_n_p_feed = "eroi_n_p_feed",
                               type = "Type",
                               boundary = "Boundary",
                               eroi = "EROI",
                               product = IEATools::iea_cols$product,
                               colnames = "colnames",
                               rowtypes = "rowtypes",
                               coltypes = "coltypes"
                               ){

  .tidy_io_mats %>%
    tidyr::pivot_longer(cols = -c({country}, {method}, {energy_type}, {last_stage}, {year}), names_to = {matnames}, values_to = {matvals}) %>%
    dplyr::filter(.data[[matnames]] %in% c(eroi_g_p, eroi_n_p, eroi_g_p_feed, eroi_n_p_feed)) %>%
    matsindf::expand_to_tidy(matnames = matnames, matvals = matvals, rownames = product, colnames = colnames) %>%
    dplyr::select(-.data[[colnames]], -.data[[rowtypes]], -.data[[coltypes]]) %>%
    dplyr::mutate(
      "{boundary}" := dplyr::case_when(
        stringr::str_detect(.data[[matnames]], "_feed") ~ "Feedstock",
        TRUE ~ "All"
      ),
      "{type}" := dplyr::case_when(
        stringr::str_detect(.data[[matnames]], "_g_") ~ "Gross",
        TRUE ~ "Net"
      )
    ) %>%
    dplyr::select(-.data[[matnames]]) %>%
    dplyr::relocate(.data[[boundary]], .before = product) %>%
    dplyr::relocate(.data[[type]], .before = boundary) %>%
    dplyr::rename(
      "{eroi}" := .data[[matvals]]
      ) %>%
    dplyr::filter(.data[[eroi]] != Inf) # This need to be modified!!!! detect resources.
}




# This function summarises EROIs per main groups
summarise_erois <- function(.tidy_erois_df,
                            .tidy_iea_df,
                            # Whether you want to include non-energy uses products in the EROI calculation
                            include_non_energy_uses = FALSE,
                            # Lists defining each product group
                            list_primary_oil_products = IEATools::primary_oil_products,
                            list_primary_coal_products = IEATools::primary_coal_products,
                            list_primary_gas_products = "Natural gas",
                            list_oil_products = IEATools::oil_and_oil_products,
                            list_coal_products = IEATools::coal_and_coal_products,
                            list_gas_products = "Natural gas",
                            # Do not change
                            product.group = "Product.Group",
                            country = IEATools::iea_cols$country,
                            method = IEATools::iea_cols$method,
                            energy_type = IEATools::iea_cols$energy_type,
                            last_stage = IEATools::iea_cols$last_stage,
                            year = IEATools::iea_cols$year,
                            product = IEATools::iea_cols$product,
                            non_energy_uses = "Non_Energy_Uses",
                            eroi.method = "Eroi.method",
                            type = "Type",
                            boundary = "Boundary",
                            share = "Share",
                            eroi = "EROI",
                            group.eroi = "Group.eroi"
                            ){

  tidy_shares_df <- dplyr::bind_rows(
    # First, shares of primary fossil fuel use by product, by main group:
    calc_share_primary_ff_use_by_product_by_group(.tidy_iea_df,
                                                  include_non_energy_uses = include_non_energy_uses),
    # Second, shares of all fossil fuel use by product, by main group
    calc_share_ff_use_by_product_by_group(.tidy_iea_df,
                                          include_non_energy_uses = include_non_energy_uses),
    # Third, shares of primary fossil fuel use by product
    calc_share_primary_ff_use_by_product(.tidy_iea_df,
                                         include_non_energy_uses = include_non_energy_uses),
    # Fourth, shares of all fossil fuel use, by product
    calc_share_ff_use_by_product(.tidy_iea_df,
                                 include_non_energy_uses = include_non_energy_uses)
  )

  # create a tidy_shares_df first

  summarised_erois <- tidy_shares_df %>%
    dplyr::left_join(.tidy_erois_df, by = c({country}, {method}, {energy_type}, {last_stage}, {year}, {product})) %>%
    dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]],
                    .data[[eroi.method]], .data[[type]], .data[[boundary]], .data[[non_energy_uses]], .data[[product.group]]) %>%
    dplyr::summarise(
      "{group.eroi}" := 1/(sum(.data[[share]] * (1/.data[[eroi]])))
    )

  return(summarised_erois)
}





#
# add_indirect_energy_to_erois <- funtion(.tidy_erois_df,
#                                         .tidy_indirect_energy){
#
#
#
#
#
# }


# This function extracts industry level erois in a tidy format.
extract_tidy_industry_erois <- function(.tidy_io_mats,
                                       country = IEATools::iea_cols$country,
                                       method = IEATools::iea_cols$method,
                                       energy_type = IEATools::iea_cols$energy_type,
                                       last_stage = IEATools::iea_cols$last_stage,
                                       year = IEATools::iea_cols$year,
                                       matnames = IEATools::mat_meta_cols$matnames,
                                       matvals = IEATools::mat_meta_cols$matvals,
                                       eroi_g_i = "eroi_g_i",
                                       eroi_n_i = "eroi_n_i",
                                       eroi_g_i_feed = "eroi_g_i_feed",
                                       eroi_n_i_feed = "eroi_n_i_feed",
                                       type = "Type",
                                       boundary = "Boundary",
                                       eroi = "EROI",
                                       industry = "Industry",
                                       colnames = "colnames",
                                       rowtypes = "rowtypes",
                                       coltypes = "coltypes"
){

  .tidy_io_mats %>%
    tidyr::pivot_longer(cols = -c({country}, {method}, {energy_type}, {last_stage}, {year}), names_to = {matnames}, values_to = {matvals}) %>%
    dplyr::filter(.data[[matnames]] %in% c(eroi_g_i, eroi_n_i, eroi_g_i_feed, eroi_n_i_feed)) %>%
    matsindf::expand_to_tidy(matnames = matnames, matvals = matvals, rownames = industry, colnames = colnames) %>%
    dplyr::select(-.data[[colnames]], -.data[[rowtypes]], -.data[[coltypes]]) %>%
    dplyr::mutate(
      "{boundary}" := dplyr::case_when(
        stringr::str_detect(.data[[matnames]], "_feed") ~ "Feedstock",
        TRUE ~ "All"
      ),
      "{type}" := dplyr::case_when(
        stringr::str_detect(.data[[matnames]], "_g_") ~ "Gross",
        TRUE ~ "Net"
      )
    ) %>%
    dplyr::select(-.data[[matnames]]) %>%
    dplyr::relocate(.data[[boundary]], .before = industry) %>%
    dplyr::relocate(.data[[type]], .before = boundary) %>%
    dplyr::rename(
      "{eroi}" := .data[[matvals]]
    ) %>%
    dplyr::filter(.data[[eroi]] != Inf) # This need to be modified!!!! detect resources.
}






# This script writes functions that enable to deal with EROIs as calculated by the Recca package.

# This function extracts product level erois in a tidy format.
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





# summarise_erois <- function(.tidy_erois_df,
#                             .tidy_iea_df,
#                             oil_products = IEATools::oil_and_oil_products,
#                             coal_products = c(IEATools::coal_and_coal_products, peat_and_peat_products),
#                             gas_products = "Natural gas",
#                             primary_oil_products = IEATools::primary_oil_products,
#                             primary_coal_products = c(IEATools::primary_coal_products, IEATools::primary_peat_products),
#                             primary_gas_products = "Natural gas",
#                             country = IEATools::iea_cols$country,
#                             method = IEATools::iea_cols$method,
#                             energy_type = IEATools::iea_cols$energy_type,
#                             last_stage = IEATools::iea_cols$last_stage,
#                             year = IEATools::iea_cols$year,
#                             product = IEATools::iea_cols$product
#                             ){
#
#   list_all_ff_carriers <- c(
#     oil_products,
#     coal_products,
#     gas_products
#   )
#
#   shares_per_group_excl_non_energy_uses <- calc_shares_per_groups(.tidy_iea_df,
#                                                                   excl_non_energy_uses = TRUE,
#                                                                   oil_products,
#                                                                   coal_products,
#                                                                   gas_products,
#                                                                   primary_oil_products,
#                                                                   primary_coal_products,
#                                                                   primary_gas_products,
#                                                                   list_all_ff_carriers)
#
#   shares_per_group_with_non_energy_uses <- calc_shares_per_groups(.tidy_iea_df,
#                                                                   excl_non_energy_uses = TRUE,
#                                                                   oil_products,
#                                                                   coal_products,
#                                                                   gas_products,
#                                                                   primary_oil_products,
#                                                                   primary_coal_products,
#                                                                   primary_gas_products,
#                                                                   list_all_ff_carriers)
#
#   shares_use_per_group <- shares_per_group_excl_non_energy_uses %>%
#     dplyr::bind_rows(shares_per_group_with_non_energy_uses)
#
#
#   .tidy_erois_df %>%
#     dplyr::left_join(shares_use_per_group, by = by = c({country}, {method}, {energy_type}, {last_stage}, {year}, {product})) %>%
#     dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]],
#                     .data[[type]], .data[[boundary]], .data[[non_energy]], .data[[carrier_group]]) %>%
#     dplyr::summarise(
#       eroi = sum(.data[[share_by_product]] * (1/eroi))
#     )
# }



# calc_shares_per_groups(.tidy_iea_df,
#                        excl_non_energy_uses = TRUE){
#
#
#
#
#
#
#
#
# }






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
extract_tidy_product_erois <- function(.tidy_io_mats,
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






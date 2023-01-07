
#' Extracts product level EROIs from tidy Input Output matrices
#'
#' The function extracts product level EROIs from tidy Input Output matrices, in a tidy format.
#'
#' The function can be called after calculating EROIs through the `Recca::calc_erois()` function.
#' The type column indicates whether the calculated EROI is gross or net.
#' The boundary column indicates whether the EROI includes:
#' * Only energy use for feedstock production ("Feedstock");
#' * Both energy use for feedstock production and for EIOU production ("All").
#'
#' @param .tidy_io_mats The `.tidy_io_mats` data frame from which EROIs should be extracted.
#' @param country,method,energy_type,last_stage,year,product See `IEATools::iea_cols`.
#' @param matnames The column name of the column having matrices names.
#'                 Default is `IEATools::mat_meta_cols$matnames`.
#' @param matvals The column name of the column reporting matrices values, once expanded.
#'                Default is `IEATools::mat_meta_cols$matvals`.
#' @param eroi_g_p The name of the column containing vectors of product-level gross EROIs,
#'                 including both energy use for feedstock and EIOU production.
#'                 Default is "eroi_g_p".
#' @param eroi_n_p The name of the column containing vectors of product-level net EROIs,
#'                 including both energy use for feedstock and EIOU production.
#'                 Default is "eroi_n_p".
#' @param eroi_g_p_feed The name of the column containing vectors of product-level gross EROIs,
#'                 including only energy use for feedstock production.
#'                 Default is "eroi_g_p_feed".
#' @param eroi_n_p_feed The name of the column containing vectors of product-level net EROIs,
#'                 including only energy use for feedstock production.
#'                 Default is "eroi_g_p_feed".
#' @param type The name of the EROI type column (i.e. gross or net EROI).
#'             Default is "Type".
#' @param boundary The name of the boundary column.
#'                 Default is "Boundary".
#' @param eroi The name of the product EROI column in output.
#'             Default is "EROI".
#' @param colnames The name of columns when expanding matrices.
#'                 Default is "colnames".
#' @param rowtypes The name of row types when expanding matrices.
#'                 Default is "rowtypes".
#' @param coltypes The name of column types when expanding matrices.
#'                 Default is "coltypes".
#'
#' @return A data frame reporting calculated EROIs in a tidy format.
#' @export
#'
#' @examples
#' # Let's first have a look at the raw data obtained when calculating EROIs:
#' calculated_erois_raw <- tidy_AB_data %>%
#'  IEATools::prep_psut() %>%
#'  Recca::calc_io_mats() %>%
#'  Recca::calc_E_EIOU() %>%
#'  Recca::calc_erois() %>%
#'  dplyr::glimpse()
#' # Let's then extract EROIs in a tidy format:
#' calculated_erois_raw %>%
#'  extract_tidy_product_erois() %>%
#'  print()
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
    dplyr::select(-tidyselect::all_of(c(colnames, rowtypes, coltypes))) %>%
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
    dplyr::select(-tidyselect::all_of(matnames)) %>%
    dplyr::relocate(tidyselect::all_of(boundary), .before = tidyselect::all_of(product)) %>%
    dplyr::relocate(tidyselect::all_of(type), .before = tidyselect::all_of(boundary)) %>%
    dplyr::rename(
      "{eroi}" := tidyselect::all_of(matvals)
      ) %>%
    dplyr::mutate(
      "{eroi}" := dplyr::case_when(
        .data[[eroi]] < 0 ~ Inf,
        TRUE ~ .data[[eroi]]
      )
    ) %>%
    dplyr::filter(! stringr::str_detect(.data[[product]], "\\[from Resources\\]"))
}




#' Extracts industry level EROIs from tidy Input Output matrices
#'
#' The function extracts industry level EROIs from tidy Input Output matrices, in a tidy format.
#'
#' The function can be called after calculating EROIs through the `Recca::calc_erois()` function.
#' The type column indicates whether the calculated EROI is gross or net.
#' The boundary column indicates whether the EROI includes:
#' * Only energy use for feedstock production ("Feedstock");
#' * Both energy use for feedstock production and for EIOU production ("All").
#'
#' @param .tidy_io_mats The `.tidy_io_mats` data frame from which EROIs should be extracted.
#' @param country,method,energy_type,last_stage,year See `IEATools::iea_cols`.
#' @param matnames The column name of the column having matrices names.
#'                 Default is `IEATools::mat_meta_cols$matnames`.
#' @param matvals The column name of the column reporting matrices values, once expanded.
#'                Default is `IEATools::mat_meta_cols$matvals`.
#' @param industry The name of the industry column returned in the output data frame.
#'                 Default is "Industry_name".
#' @param eroi_g_i The name of the column containing vectors of industry-level gross EROIs,
#'                 including both energy use for feedstock and EIOU production.
#'                 Default is "eroi_g_i".
#' @param eroi_n_i The name of the column containing vectors of industry-level net EROIs,
#'                 including both energy use for feedstock and EIOU production.
#'                 Default is "eroi_n_i".
#' @param eroi_g_i_feed The name of the column containing vectors of industry-level gross EROIs,
#'                 including only energy use for feedstock production.
#'                 Default is "eroi_g_i_feed".
#' @param eroi_n_i_feed The name of the column containing vectors of industry-level net EROIs,
#'                 including only energy use for feedstock production.
#'                 Default is "eroi_g_i_feed".
#' @param type The name of the EROI type column (i.e. gross or net EROI).
#'             Default is "Type".
#' @param boundary The name of the boundary column.
#'                 Default is "Boundary".
#' @param eroi The name of the product EROI column in output.
#'             Default is "EROI".
#' @param colnames The name of columns when expanding matrices.
#'                 Default is "colnames".
#' @param rowtypes The name of row types when expanding matrices.
#'                 Default is "rowtypes".
#' @param coltypes The name of column types when expanding matrices.
#'                 Default is "coltypes".
#'
#' @return A data frame reporting calculated EROIs in a tidy format.
#' @export
#'
#' @examples
#' # Let's first have a look at the raw data obtained when calculating EROIs:
#' calculated_erois_raw <- tidy_AB_data %>%
#'  IEATools::prep_psut() %>%
#'  Recca::calc_io_mats() %>%
#'  Recca::calc_E_EIOU() %>%
#'  Recca::calc_erois() %>%
#'  dplyr::glimpse()
#' # Let's then extract EROIs in a tidy format:
#' calculated_erois_raw %>%
#'  extract_tidy_industry_erois() %>%
#'  print()
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
                                       industry = "Industry_name",
                                       colnames = "colnames",
                                       rowtypes = "rowtypes",
                                       coltypes = "coltypes"
){

  .tidy_io_mats %>%
    tidyr::pivot_longer(cols = -c({country}, {method}, {energy_type}, {last_stage}, {year}), names_to = {matnames}, values_to = {matvals}) %>%
    dplyr::filter(.data[[matnames]] %in% c(eroi_g_i, eroi_n_i, eroi_g_i_feed, eroi_n_i_feed)) %>%
    matsindf::expand_to_tidy(matnames = matnames, matvals = matvals, rownames = industry, colnames = colnames) %>%
    dplyr::select(-tidyselect::all_of(c(colnames, rowtypes, coltypes))) %>%
    dplyr::mutate(
      "{boundary}" := dplyr::case_when(
        stringr::str_detect(.data[[matnames]], "_feed") ~ "Feedstock",
        TRUE ~ "All"
      ),
      "{type}" := dplyr::case_when(
        stringr::str_detect(.data[[matnames]], "_g_") ~ "Gross",
        TRUE ~ "Net"
      ),
    ) %>%
    dplyr::select(-tidyselect::all_of(matnames)) %>%
    dplyr::relocate(tidyselect::all_of(boundary), .before = tidyselect::all_of(industry)) %>%
    dplyr::relocate(tidyselect::all_of(type), .before = tidyselect::all_of(boundary)) %>%
    dplyr::rename(
      "{eroi}" := tidyselect::all_of(matvals)
    )
}


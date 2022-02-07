
#' Find list of observations for which Domestic Technology Assumption is applicable
#'
#' This function returns a list of observations for which the Domestic Technology Assumption is applicable,
#' in the format Country_Method_Energy.type_Last.stage_Year. See details for the process.
#'
#' Strictly speaking, the Domestic Technology Assumption can only be formulated when at least one unit of each consumed product (be it in the U_feed, U_eiou, or Y matrices)
#' is produced domestically - i.e., appears at least once in the V matrix in a non-importing flow. The default run of the function is set up to return only those observations
#' that fulfil such a requirement.
#'
#' However, one may want to be more flexible about the formulation of the Domestic Technology Assumption. Indeed, nothing prevents from formulating the Domestic
#' Technology Assumption in cases that do not fulfil the aforementioned conditions, but in that case, any demand for a given product that is not produced anywhere
#' (be it direct or indirect demand) in the domestic economy will not be translated into any upstream demand, which is an important caveat.
#'
#' The `products_to_look_for` and `requirement_matrices_list` offer additional flexibility, so that only products passed as argument to the `products_to_look_for` argument
#' and matrices passed as arguments to the `requirement_matrices_list` will act as constraining elements. Indeed, the new condition for formulation of the Domestic Technology
#' Assumption becomes that all products passed as `products_to_look_for` argument and used one of the `requirement_matrices_list`, must be produced at least once in the domestic
#' conversion chain (i.e. must appear in V in a non-importing flow).
#'
#' A column specifying matrix name for each flows needs to be added before, most likely using the `IEATools::add_psut_matnames()` function.
#'
#' @param .tidy_iea_df The `.tidy_iea_df` for which the list of observations is needed.
#' @param products_to_look_for The list of products that need to be looked for as consumed products.
#'                             Default is `IEATools::products`.
#' @param requirement_matrices_list The list of matrices where the `products_to_look_for` are looked for.
#'                                  Default is `c(IEATools::psut_cols$Y, IEATools::psut_cols$U_eiou, IEATools::psut_cols$U_feed)`.
#' @param country,method,energy_type,last_stage,year,product,flow See `IEATools::iea_cols`.
#' @param imports The name of imports flows in the `.tidy_iea_df`.
#'                Default is `IEATools::interface_industries$imports`.
#' @param matnames The column name for matrices names.
#'                 Default is `IEATools::mat_meta_cols$matnames`.
#' @param y_matrix The name of the Y matrix.
#'                 Default is `IEATools::psut_cols$Y`.
#' @param u_eiou_matrix The name of the U_EIOU matrix.
#'                 Default is `IEATools::psut_cols$U_EIOU`.
#' @param u_feed_matrix The name of the U_feed matrix.
#'                 Default is `IEATools::psut_cols$U_feed`.
#' @param v_matrix The name of the V matrix.
#'                 Default is `IEATools::psut_cols$V`.
#' @param r_matrix The name of the R matrix.
#'                 Default is `IEATools::psut_cols$R`.
#'
#' @return A list of observations for which the Domestic Technology Assumption is applicable.
#' Each observation is defined as Country_Method_Energy.type_Last.stage_Year.
#' @export
#'
#' @examples
#' tidy_AB_data %>%
#' IEATools::add_psut_matnames() %>%
#' find_list_dta_observations()
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
                                       matnames = IEATools::mat_meta_cols$matnames,
                                       y_matrix = IEATools::psut_cols$Y,
                                       u_eiou_matrix = IEATools::psut_cols$U_eiou,
                                       u_feed_matrix = IEATools::psut_cols$U_feed,
                                       v_matrix = IEATools::psut_cols$V,
                                       r_matrix = IEATools::psut_cols$R){

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



#' Transforms to Domestic Technology Assumption
#'
#' The function transforms the `.tidy_iea_df` into a data frame that describes the same Energy Conversion Chain from a Domestic Technology Assumption perspective,
#' for those assumptions that comply with the required criterion for formulating the Domestic Technology Assumption. See details.
#'
#' Strictly speaking, the Domestic Technology Assumption can only be formulated when at least one unit of each consumed product (be it in the U_feed, U_eiou, or Y matrices)
#' is produced domestically - i.e., appears at least once in the V matrix in a non-importing flow. The default run of the function is set up to return only those observations
#' that fulfil such a requirement.
#'
#' However, one may want to be more flexible about the formulation of the Domestic Technology Assumption. Indeed, nothing prevents from formulating the Domestic
#' Technology Assumption in cases that do not fulfil the aforementioned conditions, but in that case, any demand for a given product that is not produced anywhere
#' (be it direct or indirect demand) in the domestic economy will not be translated into any upstream demand, which is an important caveat.
#'
#' The `products_to_look_for` and `requirement_matrices_list` offer additional flexibility, so that only products passed as argument to the `products_to_look_for` argument
#' and matrices passed as arguments to the `requirement_matrices_list` will act as constraining elements. Indeed, the new condition for formulation of the Domestic Technology
#' Assumption becomes that all products passed as `products_to_look_for` argument and used one of the `requirement_matrices_list`, must be produced at least once in the domestic
#' conversion chain (i.e. must appear in V in a non-importing flow).
#'
#' In addition, the `select_dta_observations` boolean enable to turn on and off the selection of observations that comply with the Domestic Technology Assumption requirements.
#' When the argument is FALSE (note that default is TRUE!), then no filter is applied to observations, and the Domestic Technology Assumption is applied to all observations.
#'
#' A column specifying matrix name for each flows needs to be added before, most likely using the `IEATools::add_psut_matnames()` function.
#'
#' @param .tidy_iea_df The `.tidy_iea_df` that needs to be transformed to a data frame representing a Domestic Technology Assumption conversion chain.
#' @param products_to_look_for The list of products that need to be looked for as consumed products.
#'                             Default is `IEATools::products`.
#' @param requirement_matrices_list The list of matrices where the `products_to_look_for` are looked for.
#'                                  Default is `c(IEATools::psut_cols$Y, IEATools::psut_cols$U_eiou, IEATools::psut_cols$U_feed)`.
#' @param select_dta_observations A boolean that states whether observations that do not comply with the Domestic Technology Assumption criterion should be filtered out or not.
#'                                Default is TRUE.
#' @param country,method,energy_type,last_stage,year,flow,ledger_side,e_dot See `IEATools::iea_cols`.
#' @param imports The name of imports flows in the `.tidy_iea_df`.
#'                Default is `IEATools::interface_industries$imports`.
#' @param matnames The column name for matrices names.
#'                 Default is `IEATools::mat_meta_cols$matnames`.
#' @param balancing_matrix The name of the Balancing matrix.
#'                Default is `IEATools::psut_cols$balancing_matrix`.
#'
#' @return A `.tidy_iea_df` that describes the Energy Conversion Chain from a Domestic Technology Assumption perspective.
#' @export
#'
#' @examples
#' tidy_AB_data %>%
#' IEATools::add_psut_matnames() %>%
#' transform_to_dta() %>%
#' print()
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
                             balancing_matrix = "B"){

  if (select_dta_observations == TRUE){
    list_dta_observations <- .tidy_iea_df %>%
      find_list_dta_observations(products_to_look_for = products_to_look_for,
                                 requirement_matrices_list = requirement_matrices_list)

    tidy_iea_dta_df <- .tidy_iea_df %>%
      dplyr::filter(stringr::str_c(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], sep = "_") %in% list_dta_observations) %>%
      dplyr::mutate(
        "{matnames}" := dplyr::case_when(
          stringr::str_detect(.data[[flow]], imports) ~ balancing_matrix,
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
          stringr::str_detect(.data[[flow]], imports) ~ balancing_matrix,
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

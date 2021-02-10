# This script defines helpers functions for the EROI summaries.

# Note it excludes exports, it excludes U_feed.
calc_total_use_by_product <- function(.tidy_iea_df,
                                include_non_energy_uses = FALSE,
                                list_oil_products = IEATools::oil_and_oil_products,
                                list_coal_products = IEATools::coal_and_coal_products,
                                list_gas_products = list(natural_gas = "Natural gas"),
                                list_non_energy_flows = IEATools::non_energy_flows,
                                exports = IEATools::interface_industries$exports,
                                losses = IEATools::tfc_compare_flows$losses,
                                country = IEATools::iea_cols$country,
                                method = IEATools::iea_cols$method,
                                energy_type = IEATools::iea_cols$energy_type,
                                last_stage = IEATools::iea_cols$last_stage,
                                year = IEATools::iea_cols$year,
                                product = IEATools::iea_cols$product,
                                unit = IEATools::iea_cols$unit,
                                flow = IEATools::iea_cols$flow,
                                e_dot = IEATools::iea_cols$e_dot,
                                matnames = IEATools::mat_meta_cols$matnames,
                                Y_mat = IEATools::psut_cols$Y,
                                U_eiou_mat = IEATools::psut_cols$U_eiou,
                                total_product_use = "Total_Product_Use"
                                ){

  if (isFALSE(include_non_energy_uses)){

    to_return <- .tidy_iea_df %>%
      dplyr::filter(.data[[product]] %in% c(list_coal_products, list_oil_products, list_gas_products)) %>%
      dplyr::filter(
        ! (stringr::str_detect(.data[[flow]], exports) | stringr::str_detect(.data[[flow]], losses))
      ) %>%
      dplyr::filter(matnames == Y_mat | matnames == U_eiou_mat) %>%
      dplyr::filter(! .data[[flow]] %in% list_non_energy_flows) %>%
      dplyr::mutate(
        "{e_dot}" := dplyr::case_when(
          .data[[matnames]] == U_eiou_mat ~ -.data[[e_dot]],
          TRUE ~ .data[[e_dot]]
        )
      ) %>%
      dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[product]], .data[[unit]]) %>%
      dplyr::summarise(
        "{total_product_use}" := sum(.data[[e_dot]])
      )

    return(to_return)

  } else if (isTRUE(include_non_energy_uses)){

    to_return <- .tidy_iea_df %>%
      dplyr::filter(.data[[product]] %in% c(list_coal_products, list_oil_products, list_gas_products)) %>%
      dplyr::filter(
        ! (stringr::str_detect(.data[[flow]], exports) | stringr::str_detect(.data[[flow]], losses))
      ) %>%
      dplyr::filter(matnames == Y_mat | matnames == U_eiou_mat) %>%
      dplyr::mutate(
        "{e_dot}" := dplyr::case_when(
          .data[[matnames]] == U_eiou_mat ~ -.data[[e_dot]],
          TRUE ~ .data[[e_dot]]
        )
      ) %>%
      dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[product]], .data[[unit]]) %>%
      dplyr::summarise(
        "{total_product_use}" := sum(.data[[e_dot]])
      )

    return(to_return)

  } else {
    stop("The include_non_energy_uses argument must be either TRUE or FALSE.")
  }
}


# Calculate primary products use for each main group: coal products and oil&gas products
calc_primary_products_use_by_group <- function(.tidy_iea_df,
                                               include_non_energy_uses = FALSE,
                                               list_primary_oil_products = IEATools::primary_oil_products,
                                               list_primary_coal_products = IEATools::primary_coal_products,
                                               list_primary_gas_products = list(natural_gas = "Natural gas"),
                                               list_non_energy_flows = IEATools::non_energy_flows,
                                               exports = IEATools::interface_industries$exports,
                                               losses = IEATools::tfc_compare_flows$losses,
                                               country = IEATools::iea_cols$country,
                                               method = IEATools::iea_cols$method,
                                               energy_type = IEATools::iea_cols$energy_type,
                                               last_stage = IEATools::iea_cols$last_stage,
                                               year = IEATools::iea_cols$year,
                                               product = IEATools::iea_cols$product,
                                               unit = IEATools::iea_cols$unit,
                                               flow = IEATools::iea_cols$flow,
                                               e_dot = IEATools::iea_cols$e_dot,
                                               matnames = IEATools::mat_meta_cols$matnames,
                                               Y_mat = IEATools::psut_cols$Y,
                                               U_eiou_mat = IEATools::psut_cols$U_eiou,
                                               product.group = "Product.Group",
                                               total_product_use = "Total_Product_Use"){

  if (isFALSE(include_non_energy_uses)){

    to_return <- .tidy_iea_df %>%
      dplyr::filter(.data[[product]] %in% c(list_primary_coal_products, list_primary_oil_products, list_primary_gas_products)) %>%
      dplyr::filter(
        ! (stringr::str_detect(.data[[flow]], exports) | stringr::str_detect(.data[[flow]], losses))
      ) %>%
      dplyr::filter(matnames == Y_mat | matnames == U_eiou_mat) %>%
      dplyr::filter(! .data[[flow]] %in% list_non_energy_flows) %>%
      dplyr::mutate(
        "{product.group}" := dplyr::case_when(
          .data[[product]] %in% c(list_primary_oil_products, list_primary_gas_products) ~ "Primary oil and gas products",
          .data[[product]] %in% list_primary_coal_products ~ "Primary coal products"
        ),
        "{e_dot}" := dplyr::case_when(
          .data[[matnames]] == U_eiou_mat ~ -.data[[e_dot]],
          TRUE ~ .data[[e_dot]]
        )
      ) %>%
      dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[product.group]], .data[[unit]]) %>%
      dplyr::summarise(
        "{total_product_use}" := sum(.data[[e_dot]])
      )

    return(to_return)

  } else if (isTRUE(include_non_energy_uses)){

    to_return <- .tidy_iea_df %>%
      dplyr::filter(.data[[product]] %in% c(list_primary_coal_products, list_primary_oil_products, list_primary_gas_products)) %>%
      dplyr::filter(
        ! (stringr::str_detect(.data[[flow]], exports) | stringr::str_detect(.data[[flow]], losses))
      ) %>%
      dplyr::filter(matnames == Y_mat | matnames == U_eiou_mat) %>%
      dplyr::mutate(
        "{product.group}" := dplyr::case_when(
          .data[[product]] %in% c(list_primary_oil_products, list_primary_gas_products) ~ "Primary oil and gas products",
          .data[[product]] %in% list_primary_coal_products ~ "Primary coal products"
        ),
        "{e_dot}" := dplyr::case_when(
          .data[[matnames]] == U_eiou_mat ~ -.data[[e_dot]],
          TRUE ~ .data[[e_dot]]
        )
      ) %>%
      dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[product.group]], .data[[unit]]) %>%
      dplyr::summarise(
        "{total_product_use}" := sum(.data[[e_dot]])
      )

    return(to_return)

  } else {
    stop("The include_non_energy_uses argument must be either TRUE or FALSE.")
  }
}





calc_all_products_use_by_group <- function(){


  if (isFALSE(include_non_energy_uses)){

  } else if (isTRUE(include_non_energy_uses)){

  } else {
    stop("The include_non_energy_uses argument must be either TRUE or FALSE.")
  }


}

calc_primary_ff_use() <- function(){

  if (isFALSE(include_non_energy_uses)){

  } else if (isTRUE(include_non_energy_uses)){

  } else {
    stop("The include_non_energy_uses argument must be either TRUE or FALSE.")
  }


}


calc_ff_use() <- function(){

  if (isFALSE(include_non_energy_uses)){

  } else if (isTRUE(include_non_energy_uses)){

  } else {
    stop("The include_non_energy_uses argument must be either TRUE or FALSE.")
  }

}










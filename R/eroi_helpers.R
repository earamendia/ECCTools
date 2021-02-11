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
                                               total_group_use = "Total_Group_Use"){

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
        "{total_group_use}" := sum(.data[[e_dot]])
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
        "{total_group_use}" := sum(.data[[e_dot]])
      )

    return(to_return)

  } else {
    stop("The include_non_energy_uses argument must be either TRUE or FALSE.")
  }
}


# Calculates use of products by main group (coal vs oil&gas)
calc_all_products_use_by_group <- function(.tidy_iea_df,
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
                                           product.group = "Product.Group",
                                           total_group_use = "Total_Group_Use"){

  if (isFALSE(include_non_energy_uses)){

    to_return <- .tidy_iea_df %>%
      dplyr::filter(.data[[product]] %in% c(list_coal_products, list_oil_products, list_gas_products)) %>%
      dplyr::filter(
        ! (stringr::str_detect(.data[[flow]], exports) | stringr::str_detect(.data[[flow]], losses))
      ) %>%
      dplyr::filter(matnames == Y_mat | matnames == U_eiou_mat) %>%
      dplyr::filter(! .data[[flow]] %in% list_non_energy_flows) %>%
      dplyr::mutate(
        "{product.group}" := dplyr::case_when(
          .data[[product]] %in% c(list_oil_products, list_gas_products) ~ "All oil and gas products",
          .data[[product]] %in% list_coal_products ~ "All coal products"
        ),
        "{e_dot}" := dplyr::case_when(
          .data[[matnames]] == U_eiou_mat ~ -.data[[e_dot]],
          TRUE ~ .data[[e_dot]]
        )
      ) %>%
      dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[product.group]], .data[[unit]]) %>%
      dplyr::summarise(
        "{total_group_use}" := sum(.data[[e_dot]])
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
        "{product.group}" := dplyr::case_when(
          .data[[product]] %in% c(list_oil_products, list_gas_products) ~ "All oil and gas products",
          .data[[product]] %in% list_coal_products ~ "All coal products"
        ),
        "{e_dot}" := dplyr::case_when(
          .data[[matnames]] == U_eiou_mat ~ -.data[[e_dot]],
          TRUE ~ .data[[e_dot]]
        )
      ) %>%
      dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[product.group]], .data[[unit]]) %>%
      dplyr::summarise(
        "{total_group_use}" := sum(.data[[e_dot]])
      )

    return(to_return)

  } else {
    stop("The include_non_energy_uses argument must be either TRUE or FALSE.")
  }
}


# Calculates total primary fossil fuel use
calc_primary_ff_use <- function(.tidy_iea_df,
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
                                  total_group_use = "Total_Group_Use"){

  if (isFALSE(include_non_energy_uses)){

    to_return <- .tidy_iea_df %>%
      dplyr::filter(.data[[product]] %in% c(list_primary_coal_products, list_primary_oil_products, list_primary_gas_products)) %>%
      dplyr::filter(
        ! (stringr::str_detect(.data[[flow]], exports) | stringr::str_detect(.data[[flow]], losses))
      ) %>%
      dplyr::filter(matnames == Y_mat | matnames == U_eiou_mat) %>%
      dplyr::filter(! .data[[flow]] %in% list_non_energy_flows) %>%
      dplyr::mutate(
        "{product.group}" := "Primary fossil fuels",
        "{e_dot}" := dplyr::case_when(
          .data[[matnames]] == U_eiou_mat ~ -.data[[e_dot]],
          TRUE ~ .data[[e_dot]]
        )
      ) %>%
      dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[product.group]], .data[[unit]]) %>%
      dplyr::summarise(
        "{total_group_use}" := sum(.data[[e_dot]])
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
        "{product.group}" := "Primary fossil fuels",
        "{e_dot}" := dplyr::case_when(
          .data[[matnames]] == U_eiou_mat ~ -.data[[e_dot]],
          TRUE ~ .data[[e_dot]]
        )
      ) %>%
      dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[product.group]], .data[[unit]]) %>%
      dplyr::summarise(
        "{total_group_use}" := sum(.data[[e_dot]])
      )

    return(to_return)


  } else {
    stop("The include_non_energy_uses argument must be either TRUE or FALSE.")
  }
}

# Calculates total fossil fuel use
calc_ff_use <- function(.tidy_iea_df,
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
                          product.group = "Product.Group",
                          total_group_use = "Total_Group_Use"){

  if (isFALSE(include_non_energy_uses)){

    to_return <- .tidy_iea_df %>%
      dplyr::filter(.data[[product]] %in% c(list_coal_products, list_oil_products, list_gas_products)) %>%
      dplyr::filter(
        ! (stringr::str_detect(.data[[flow]], exports) | stringr::str_detect(.data[[flow]], losses))
      ) %>%
      dplyr::filter(matnames == Y_mat | matnames == U_eiou_mat) %>%
      dplyr::filter(! .data[[flow]] %in% list_non_energy_flows) %>%
      dplyr::mutate(
        "{product.group}" := "All fossil fuels",
        "{e_dot}" := dplyr::case_when(
          .data[[matnames]] == U_eiou_mat ~ -.data[[e_dot]],
          TRUE ~ .data[[e_dot]]
        )
      ) %>%
      dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[product.group]], .data[[unit]]) %>%
      dplyr::summarise(
        "{total_group_use}" := sum(.data[[e_dot]])
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
        "{product.group}" := "All fossil fuels",
        "{e_dot}" := dplyr::case_when(
          .data[[matnames]] == U_eiou_mat ~ -.data[[e_dot]],
          TRUE ~ .data[[e_dot]]
        )
      ) %>%
      dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[product.group]], .data[[unit]]) %>%
      dplyr::summarise(
        "{total_group_use}" := sum(.data[[e_dot]])
      )

    return(to_return)

  } else {
    stop("The include_non_energy_uses argument must be either TRUE or FALSE.")
  }
}


# Calculates shares of each product in the primary fossil fuel consumption by group

calc_share_primary_ff_use_by_product_by_group <- function(.tidy_iea_df,
                                                          include_non_energy_uses = FALSE,
                                                          list_primary_oil_products = IEATools::primary_oil_products,
                                                          list_primary_coal_products = IEATools::primary_coal_products,
                                                          list_primary_gas_products = list(natural_gas = "Natural gas"),
                                                          product.group = "Product.Group",
                                                          total_product_use = "Total_Product_Use",
                                                          total_group_use = "Total_Group_Use",
                                                          non_energy_uses = "Non_Energy_Uses",
                                                          share = "Share",
                                                          country = IEATools::iea_cols$country,
                                                          method = IEATools::iea_cols$method,
                                                          energy_type = IEATools::iea_cols$energy_type,
                                                          last_stage = IEATools::iea_cols$last_stage,
                                                          year = IEATools::iea_cols$year,
                                                          unit = IEATools::iea_cols$unit,
                                                          product = IEATools::iea_cols$product,
                                                          boolean_non_energy_uses = "Boolean_Non_Energy_Uses"
                                                          ){




  if (! (isTRUE(include_non_energy_uses) | isFALSE(include_non_energy_uses))){
    stop("The include_non_energy_uses argument must be either TRUE or FALSE.")
  }

    use_ff_by_product <- calc_total_use_by_product(.tidy_iea_df,
                                                           include_non_energy_uses = include_non_energy_uses) %>%
      dplyr::mutate(
        "{product.group}" := dplyr::case_when(
          .data[[product]] %in% c(list_primary_oil_products, list_primary_gas_products) ~ "Primary oil and gas products",
          .data[[product]] %in% list_primary_coal_products ~ "Primary coal products"
        )
      )

    share_primary_ff_use_by_product_by_group <- calc_primary_products_use_by_group(.tidy_iea_df,
                                                                                   include_non_energy_uses = include_non_energy_uses) %>%
      dplyr::left_join(use_ff_by_product, by = c({country}, {method}, {energy_type}, {last_stage}, {year}, {unit}, {product.group})) %>%
      dplyr::mutate(
        "{share}" := .data[[total_product_use]] / .data[[total_group_use]],
        "{boolean_non_energy_uses}" := include_non_energy_uses,
        "{non_energy_uses}" := dplyr::case_when(
          isTRUE(.data[[boolean_non_energy_uses]]) ~ "Included",
          isFALSE(.data[[boolean_non_energy_uses]]) ~ "Excluded"
        )
      ) %>%
      dplyr::select(-.data[[boolean_non_energy_uses]])

    return(share_primary_ff_use_by_product_by_group)
}



# Calculates shares of each product in all the fossil fuel consumption by group

calc_share_ff_use_by_product_by_group <- function(.tidy_iea_df,
                                                  include_non_energy_uses = FALSE,
                                                  list_oil_products = IEATools::oil_and_oil_products,
                                                  list_coal_products = IEATools::coal_and_coal_products,
                                                  list_gas_products = list(natural_gas = "Natural gas"),
                                                  product.group = "Product.Group",
                                                  total_product_use = "Total_Product_Use",
                                                  total_group_use = "Total_Group_Use",
                                                  non_energy_uses = "Non_Energy_Uses",
                                                  share = "Share",
                                                  country = IEATools::iea_cols$country,
                                                  method = IEATools::iea_cols$method,
                                                  energy_type = IEATools::iea_cols$energy_type,
                                                  last_stage = IEATools::iea_cols$last_stage,
                                                  year = IEATools::iea_cols$year,
                                                  unit = IEATools::iea_cols$unit,
                                                  product = IEATools::iea_cols$product,
                                                  boolean_non_energy_uses = "Boolean_Non_Energy_Uses"
                                                  ){

  if (! (isTRUE(include_non_energy_uses) | isFALSE(include_non_energy_uses))){
    stop("The include_non_energy_uses argument must be either TRUE or FALSE.")
  }

  use_ff_by_product <- calc_total_use_by_product(.tidy_iea_df,
                                                  include_non_energy_uses = include_non_energy_uses) %>%
    dplyr::mutate(
      "{product.group}" := dplyr::case_when(
        .data[[product]] %in% c(list_oil_products, list_gas_products) ~ "All oil and gas products",
        .data[[product]] %in% list_coal_products ~ "All coal products"
      )
    )

  share_ff_use_by_product_by_group <- calc_all_products_use_by_group(.tidy_iea_df,
                                                                     include_non_energy_uses = include_non_energy_uses) %>%
    dplyr::left_join(use_ff_by_product, by = c({country}, {method}, {energy_type}, {last_stage}, {year}, {unit}, {product.group})) %>%
    dplyr::mutate(
      "{share}" := .data[[total_product_use]] / .data[[total_group_use]],
      "{boolean_non_energy_uses}" := include_non_energy_uses,
      "{non_energy_uses}" := dplyr::case_when(
        isTRUE(.data[[boolean_non_energy_uses]]) ~ "Included",
        isFALSE(.data[[boolean_non_energy_uses]]) ~ "Excluded"
      )
    ) %>%
    dplyr::select(-.data[[boolean_non_energy_uses]])

  return(share_ff_use_by_product_by_group)
}





# Calculates shares of each product in the total primary fossil fuel consumption
calc_share_primary_ff_use_by_product <- function(.tidy_iea_df,
                                                 include_non_energy_uses = FALSE,
                                                 list_primary_oil_products = IEATools::primary_oil_products,
                                                 list_primary_coal_products = IEATools::primary_coal_products,
                                                 list_primary_gas_products = list(natural_gas = "Natural gas"),
                                                 product.group = "Product.Group",
                                                 total_product_use = "Total_Product_Use",
                                                 total_group_use = "Total_Group_Use",
                                                 non_energy_uses = "Non_Energy_Uses",
                                                 share = "Share",
                                                 country = IEATools::iea_cols$country,
                                                 method = IEATools::iea_cols$method,
                                                 energy_type = IEATools::iea_cols$energy_type,
                                                 last_stage = IEATools::iea_cols$last_stage,
                                                 year = IEATools::iea_cols$year,
                                                 unit = IEATools::iea_cols$unit,
                                                 product = IEATools::iea_cols$product,
                                                 boolean_non_energy_uses = "Boolean_Non_Energy_Uses"
                                                 ){

  if (! (isTRUE(include_non_energy_uses) | isFALSE(include_non_energy_uses))){
    stop("The include_non_energy_uses argument must be either TRUE or FALSE.")
  }

  use_ff_by_product <- calc_total_use_by_product(.tidy_iea_df,
                                                 include_non_energy_uses = include_non_energy_uses) %>%
    dplyr::mutate(
      "{product.group}" := "Primary fossil fuels"
    )

  share_primary_ff_use_by_product <- calc_primary_ff_use(.tidy_iea_df,
                                                         include_non_energy_uses = include_non_energy_uses) %>%
    dplyr::left_join(use_ff_by_product, by = c({country}, {method}, {energy_type}, {last_stage}, {year}, {unit}, {product.group})) %>%
    dplyr::mutate(
      "{share}" := .data[[total_product_use]] / .data[[total_group_use]],
      "{boolean_non_energy_uses}" := include_non_energy_uses,
      "{non_energy_uses}" := dplyr::case_when(
        isTRUE(.data[[boolean_non_energy_uses]]) ~ "Included",
        isFALSE(.data[[boolean_non_energy_uses]]) ~ "Excluded"
      )
    ) %>%
    dplyr::select(-.data[[boolean_non_energy_uses]])

  return(share_primary_ff_use_by_product)
}





# Calculates shares of each product in the total fossil fuel consumption
calc_share_ff_use_by_product <- function(.tidy_iea_df,
                                         include_non_energy_uses = FALSE,
                                         list_oil_products = IEATools::oil_and_oil_products,
                                         list_coal_products = IEATools::coal_and_coal_products,
                                         list_gas_products = list(natural_gas = "Natural gas"),
                                         product.group = "Product.Group",
                                         total_product_use = "Total_Product_Use",
                                         total_group_use = "Total_Group_Use",
                                         non_energy_uses = "Non_Energy_Uses",
                                         share = "Share",
                                         country = IEATools::iea_cols$country,
                                         method = IEATools::iea_cols$method,
                                         energy_type = IEATools::iea_cols$energy_type,
                                         last_stage = IEATools::iea_cols$last_stage,
                                         year = IEATools::iea_cols$year,
                                         unit = IEATools::iea_cols$unit,
                                         product = IEATools::iea_cols$product,
                                         boolean_non_energy_uses = "Boolean_Non_Energy_Uses"
                                         ){


  if (! (isTRUE(include_non_energy_uses) | isFALSE(include_non_energy_uses))){
    stop("The include_non_energy_uses argument must be either TRUE or FALSE.")
  }

  use_ff_by_product <- calc_total_use_by_product(.tidy_iea_df,
                                                 include_non_energy_uses = include_non_energy_uses) %>%
    dplyr::mutate(
      "{product.group}" := "All fossil fuels"
    )

  share_ff_use_by_product <- calc_ff_use(.tidy_iea_df,
                                                         include_non_energy_uses = include_non_energy_uses) %>%
    dplyr::left_join(use_ff_by_product, by = c({country}, {method}, {energy_type}, {last_stage}, {year}, {unit}, {product.group})) %>%
    dplyr::mutate(
      "{share}" := .data[[total_product_use]] / .data[[total_group_use]],
      "{boolean_non_energy_uses}" := include_non_energy_uses,
      "{non_energy_uses}" := dplyr::case_when(
        isTRUE(.data[[boolean_non_energy_uses]]) ~ "Included",
        isFALSE(.data[[boolean_non_energy_uses]]) ~ "Excluded"
      )
    ) %>%
    dplyr::select(-.data[[boolean_non_energy_uses]])

  return(share_ff_use_by_product)
}


# This script defines transformation functions


# This function specifies the multiregional R matrix
specify_MR_R <- function(.tidy_iea_df,
                         R_matrix = "R",
                         matnames = "matnames",
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



# This function specifies the multiregional V matrix.
specify_MR_V <- function(.tidy_iea_df,
                         V_matrix = "V",
                         matnames = "matnames",
                         imports = IEATools::interface_industries$imports,
                         country = IEATools::iea_cols$country,
                         flow = IEATools::iea_cols$flow,
                         product = IEATools::iea_cols$product,
                         aggregate_country_name = "World"){

  MR_V <- .tidy_iea_df %>%
    dplyr::filter(.data[[matnames]] == V_matrix, ! stringr::str_detect(.data[[flow]], imports)) %>%
    dplyr::mutate(
      "{flow}" := paste0("{", .data[[country]], "}_", .data[[flow]]),
      "{product}" := paste0("{", .data[[country]], "}_", .data[[product]]),
      "{country}":= aggregate_country_name
    )

  return(MR_V)
}


# This function specifies the multiregional Y matrix using the GMA assumption
# specify_MR_Y_gma <- function(.tidy_iea_df,
                             #){

  # (1) Differentiating domestically produced and imported products in the Y matrix flows
  # Y_flows_by_origin <- .tidy_iea_df %>%
  #   filter(.data[[matnames]] == Y_matrix) %>%
  #   specify_imported_products()

  # (2) Defining the global market for each product



  # (3) Creating the MR-Y from (1) and (2)



#}


# Function that specifies imported versus domestically produced products

# specify_imported_products <- function(.tidy_iea_df,
#                                       ){
#
#   defined_imported_flows <- .tidy_iea_df %>%
#     filter(.data[[matnames]] == Y_matrix | .data[[matnames]] == U_feed_matrix | .data[[matnames]] == U_EIOU_matrix, ! str_detect(.data[[flow]], exports)) %>%
#     left_join(
#       share_imports_by_products(), by = c()
#     ) %>%
#     mutate(
#       Domestic = E.dot * (1 - Share_Imports),
#       Imported = E.dot * Share_Imports
#     ) %>%
#     select(-E.dot, -total_consumption, -Imports, -Share_Imports) %>%
#     pivot_longer(cols = c("Domestic", "Imported"), names_to = "Origin", values_to = "E.dot") %>%
#     relocate(Origin, .after = Product) %>%
#     filter(E.dot != 0)
#
#   return(defined_imported_flows)
# }



# share_imports_by_products <- function(.tidy_iea_df){
#   left_join(total_consumption_by_product(.tidy_iea_df), imports_by_product(.tidy_iea_df), by = ) %>%
#     select(-Ledger.side, -matnames, -Flow.aggregation.point, -Flow, -Unit) %>%
#     mutate(
#       Share_Imports = case_when(
#         is.na(Imports) ~ 0,
#         !is.na(Imports) ~ Imports / total_consumption
#       )
#     )
# }



total_consumption_by_product <- function(.tidy_iea_df,
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
                                         U_EIOU_matrix = "U_EIOU"){

  tidy_total_consumption_by_product <- .tidy_iea_df %>%
    dplyr::filter((.data[[matnames]] == Y_matrix |
              .data[[matnames]] == U_feed_matrix |
              .data[[matnames]] == U_EIOU_matrix),
           ! stringr::str_detect(.data[[flow]], exports)) %>% # There shouldn't be anymore exports where there are imports, now.
    dplyr::mutate(
      "{e_dot}" := dplyr::case_when(
        .data[[matnames]] == U_feed_matrix ~ -.data[[e_dot]],
        .data[[matnames]] == U_EIOU_matrix ~ -.data[[e_dot]],
        TRUE ~ .data[[e_dot]]
      )
    ) %>%
    dplyr::group_by(.data[[country]], .data[[method]], .data[[energy_type]], .data[[last_stage]], .data[[year]], .data[[product]], .data[[unit]]) %>%
    dplyr::summarise(
      total_consumption = sum(.data[[e_dot]])
    )

  return(tidy_total_consumption_by_product)
}


# Total consumption by product, year, country
# total_consumption_by_product <- AB_tidy_data %>%
#   filter((matnames == "Y" | matnames == "U_feed" | matnames == "U_EIOU"), ! str_detect(Flow, "Exports")) %>% # There shouldn't be anymore exports where there are imports, now.
#   mutate(
#     E.dot = case_when(
#       matnames == "U_feed" ~ - E.dot,
#       matnames == "U_EIOU" ~ - E.dot,
#       TRUE ~ E.dot
#     )
#   ) %>%
#   group_by(Country, Method, Energy.type, Last.stage, Year, Product) %>%
#   summarise(
#     total_consumption = sum(E.dot)
#   ) %>%
#   print()



# imports_by_product <- function(){
#
# }






# Imports by product, year, country
# imports <- AB_tidy_data %>%
#   filter(str_detect(Flow, "Imports")) %>%
#   rename(Imports = E.dot) %>%
#   print()

# Share of imported products by product, year, country
# share_imports_by_product <- total_consumption_by_product %>%
#   left_join(imports, by = c("Country", "Method", "Energy.type", "Last.stage", "Year", "Product")) %>%
#   select(-Ledger.side, -matnames, -Flow.aggregation.point, -Flow, -Unit) %>%
#   mutate(
#     Share_Imports = case_when(
#       is.na(Imports) ~ 0,
#       !is.na(Imports) ~ Imports / total_consumption
#     )
#   ) %>%
#   print()


# Specifying imported products in Y, U_EIOU, U_feed

# defining_imported_products <- AB_tidy_data %>%
#   filter((matnames == "Y" | matnames == "U_feed" | matnames == "U_EIOU"), ! str_detect(Flow, "Exports")) %>%
#   left_join(
#     share_imports_by_product, by = c("Country", "Year", "Product", "Method", "Energy.type", "Last.stage")
#   ) %>%
#   mutate(
#     Domestic = E.dot * (1 - Share_Imports),
#     Imported = E.dot * Share_Imports
#   ) %>%
#   select(-E.dot, -total_consumption, -Imports, -Share_Imports) %>%
#   pivot_longer(cols = c("Domestic", "Imported"), names_to = "Origin", values_to = "E.dot") %>%
  # mutate(
  #   Product = case_when(
  #     Origin == "Imported" ~ paste0(Product, " [Imported]"),
  #     TRUE ~ Product
  #   )
  # ) %>%
  # relocate(Origin, .after = Product) %>%
  # filter(E.dot != 0) %>%
  # print()




















# This function specifies the multiregional U matrix using the GMA assumption
specify_MR_U_gma <- function(){
  # (1) Differentiating domestically produced and imported products in the U matrix flows


  # (2) Creating MR-Y, MR-U_EIOU, MR-U_feed by loading a trade matrix. Create some helpers (functions) to help create the trade matrix.
}






# This function differentiates products


# Transform to gma function
transform_to_gma <- function() {
  return("hello world")

  # (1) Create MR-R matrix data frame
  #MR_R <- specify_MR_R()

  # (2) Creating MR-V matrix data frame
  #MR_V <- specify_MR_V()

  # (3) Creating MR-Y matrix data frame, with GMA assumption
  #MR_Y <- specify_MR_Y_gma()

  # (4) Creating MR-U matrix data frame, with GMA assumption
  #MR_U <- specify_MR_U_gma()

}




# Transform to bta function
transform_to_bta <- function() {
  return("hello world")
}


# Transform to dta function

transform_to_dta <- function() {
  return("hello world")
}



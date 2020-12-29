
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
    filter(.data[[matnames]] == R_matrix) %>%
    mutate(
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
    filter(.data[[matnames]] == V_matrix, ! str_detect(.data[[flow]], imports)) %>%
    mutate(
      "{flow}" := paste0("{", .data[[country]], "}_", .data[[flow]]),
      "{product}" := paste0("{", .data[[country]], "}_", .data[[product]]),
      "{country}":= aggregate_country_name
    )

  return(MR_V)
}


# This function specifies the multiregional Y matrix using the GMA assumption
specify_MR_Y_gma <- function(){

  # (1) Creating Y, U_EIOU, U_feed data frames separating imports and exports


  # (2) Creating MR-Y, MR-U_EIOU, MR-U_feed by loading a trade matrix. Create some helpers (functions) to help create the trade matrix.

}


# This function specifies the multiregional U matrix using the GMA assumption
specify_MR_U_gma <- function(){
  # (1) Creating Y, U_EIOU, U_feed data frames separating imports and exports


  # (2) Creating MR-Y, MR-U_EIOU, MR-U_feed by loading a trade matrix. Create some helpers (functions) to help create the trade matrix.
}






# This function differentiates products


# Transform to gma function
transform_to_gma <- function() {
  return("hello world")

  # (1) Create MR-R matrix data frame
  MR_R <- specify_MR_R()

  # (2) Creating MR-V matrix data frame
  MR_V <- specify_MR_V()

  # (3) Creating MR-Y matrix data frame, with GMA assumption
  MR_Y <- specify_MR_Y_gma()

  # (4) Creating MR-U matrix data frame, with GMA assumption
  MR_U <- specify_MR_U_gma()

}




# Transform to bta function
transform_to_bta <- function() {
  return("hello world")
}


# Transform to dta function

transform_to_dta <- function() {
  return("hello world")
}



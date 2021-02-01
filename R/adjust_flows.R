
convert_fuel_gasoline_into_motor_gasoline <- function(.tidy_iea_df,
                                                      product = IEATools::iea_cols$product,
                                                      flow = IEATools::iea_cols$flow,
                                                      e_dot = IEATools::iea_cols$e_dot){

  .tidy_iea_df %>%
    dplyr::mutate(
      "{product}" := dplyr::case_when(
        .data[[product]] == "Gasoline type jet fuel" ~ "Motor gasoline excl. biofuels",
        TRUE ~ .data[[product]]
      ),
      "{flow}" := dplyr::case_when(
        stringr::str_detect(.data[[flow]], "Gasoline type jet fuel") ~ stringr::str_replace(.data[[flow]], "Gasoline type jet fuel", "Motor gasoline excl. biofuels"),
        TRUE ~ .data[[flow]]
      )
    ) %>%
    matsindf::group_by_everything_except(e_dot) %>%
    dplyr::summarise(
      "{e_dot}" := sum(.data[[e_dot]])
    )
}


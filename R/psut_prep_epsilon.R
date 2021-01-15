# Ideally all this should go to the IEATools package.

extract_S_units_from_tidy <- function(.tidy_iea_df,
                                      # Column names in .tidy_iea_df
                                      ledger_side = IEATools::iea_cols$ledger_side,
                                      flow_aggregation_point = IEATools::iea_cols$flow_aggregation_point,
                                      flow = IEATools::iea_cols$flow,
                                      product = IEATools::iea_cols$product,
                                      e_dot = IEATools::iea_cols$e_dot,
                                      unit = IEATools::iea_cols$unit,
                                      # Row and product types
                                      product_type = IEATools::row_col_types$product,
                                      unit_type = IEATools::row_col_types$unit,
                                      # Output column name
                                      s_units = IEATools::psut_cols$s_units,
                                      # Intermediate column names
                                      .val = ".val",
                                      .rowtype = ".rowtype",
                                      .coltype = ".coltype"){
  grouping_vars <- matsindf::everything_except(.tidy_iea_df, ledger_side, flow_aggregation_point, flow, product, e_dot, unit)
  matsindf::verify_cols_missing(.tidy_iea_df, c(s_units, .val, .rowtype, .coltype))
  .tidy_iea_df %>%
    dplyr::group_by(!!!grouping_vars) %>%
    dplyr::select(!!!grouping_vars, .data[[product]], .data[[unit]]) %>%
    dplyr::do(unique(.data)) %>%
    dplyr::mutate(
      "{.val}" := 1,
      "{s_units}" := s_units,
      "{.rowtype}" := product_type,
      "{.coltype}" := unit_type
    ) %>%
    matsindf::collapse_to_matrices(matnames = s_units, matvals = .val,
                                   rownames = product, colnames = unit,
                                   rowtypes = .rowtype, coltypes = .coltype) %>%
    dplyr::rename(
      "{s_units}" := .data[[.val]]
    ) %>%
    dplyr::ungroup()
}




add_psut_matnames_epsilon <- function(.tidy_iea_df,
                              # Input columns
                              ledger_side = IEATools::iea_cols$ledger_side,
                              flow_aggregation_point = IEATools::iea_cols$flow_aggregation_point,
                              flow = IEATools::iea_cols$flow,
                              product = IEATools::iea_cols$product,
                              e_dot = IEATools::iea_cols$e_dot,
                              supply = IEATools::ledger_sides$supply,
                              consumption = IEATools::ledger_sides$consumption,
                              production = IEATools::tpes_flows$production,
                              resources = IEATools::tpes_flows$resources,
                              # Input identifiers for supply, consumption, and EIOU
                              eiou = IEATools::tfc_compare_flows$energy_industry_own_use,
                              neg_supply_in_fd = c(IEATools::tpes_flows$exports,
                                                   IEATools::tpes_flows$international_aviation_bunkers,
                                                   IEATools::tpes_flows$international_marine_bunkers,
                                                   IEATools::tpes_flows$stock_changes,
                                                   IEATools::tfc_compare_flows$losses,
                                                   IEATools::tfc_compare_flows$statistical_differences),
                              # Output column
                              matnames = IEATools::mat_meta_cols$matnames,
                              # Output identifiers for
                              # use matrix excluding EIOU (U_feed),
                              # use matrix energy industry own use items (U_EIOU),
                              # make (V), and
                              # final demand (Y)
                              # matrices.
                              R = IEATools::psut_cols$R,
                              U_feed = IEATools::psut_cols$U_feed,
                              U_EIOU = IEATools::psut_cols$U_eiou,
                              V = IEATools::psut_cols$V,
                              Y = IEATools::psut_cols$Y,
                              epsilon = "Epsilon"){
  matsindf::verify_cols_missing(.tidy_iea_df, matnames)

  .tidy_iea_df %>%
    dplyr::mutate(
      "{matnames}" := dplyr::case_when(
        stringr::str_detect(.data[[ledger_side]], epsilon) ~ epsilon,
        # All Consumption items belong in the final demand (Y) matrix.
        .data[[ledger_side]] == consumption ~ Y,
        # All production items belong in the resources (R) matrix.
        .data[[flow]] %>% starts_with_any_of(c(production, resources)) ~ R,
        # All other positive values on the Supply side of the ledger belong in the make (V) matrix.
        .data[[ledger_side]] == supply & .data[[e_dot]] > 0 ~ V,
        # Negative values on the supply side of the ledger with Flow == "Energy industry own use"
        # are put into the U_EIOU matrix
        .data[[ledger_side]] == supply & .data[[e_dot]] <= 0 & !!as.name(flow_aggregation_point) == eiou ~ U_EIOU,
        # Negative values on the supply side that have Flow %in% neg_supply_in_fd go in the final demand matrix
        .data[[ledger_side]] == supply & .data[[e_dot]] <= 0 & starts_with_any_of(!!as.name(flow), neg_supply_in_fd) ~ Y,
        # All other negative values on the Supply side of the ledger belong in the use matrix
        # that excludes EIOU (U_feed).
        .data[[ledger_side]] == supply & .data[[e_dot]] <= 0 ~ U_feed,
        # Identify any places where our logic is faulty.
        TRUE ~ NA_character_
      )
    )
}





add_row_col_meta_epsilon <- function(.tidy_iea_df,
                             # Column names for Product and Flow
                             product = IEATools::iea_cols$product,
                             flow = IEATools::iea_cols$flow,
                             # Name of the input column containing matrix names
                             matnames = IEATools::mat_meta_cols$matnames,
                             # Expected matrix names in the matnames column
                             U = IEATools::psut_cols$U,
                             U_EIOU = IEATools::psut_cols$U_eiou,
                             R = IEATools::psut_cols$R,
                             V = IEATools::psut_cols$V,
                             Y = IEATools::psut_cols$Y,
                             epsilon = "Epsilon",
                             # Row and column Type identifiers
                             industry_type = IEATools::row_col_types$industry,
                             product_type = IEATools::row_col_types$product,
                             sector_type = IEATools::row_col_types$sector,
                             resource_type = IEATools::row_col_types$resource,
                             # Output columns
                             rownames = IEATools::mat_meta_cols$rownames,
                             colnames = IEATools::mat_meta_cols$colnames,
                             rowtypes = IEATools::mat_meta_cols$rowtypes,
                             coltypes = IEATools::mat_meta_cols$coltypes){
  matsindf::verify_cols_missing(.tidy_iea_df, c(rownames, colnames, rowtypes, coltypes))
  .tidy_iea_df %>%
    dplyr::mutate(
      "{rownames}" := dplyr::case_when(
        startsWith(.data[[matnames]], U) ~ .data[[product]],
        .data[[matnames]] == R ~ .data[[flow]],
        .data[[matnames]] == V ~ .data[[flow]],
        .data[[matnames]] == Y ~ .data[[product]],
        .data[[matnames]] == epsilon ~ .data[[product]],
        TRUE ~ NA_character_
      ),
      !!as.name(colnames) := dplyr::case_when(
        startsWith(.data[[matnames]], U) ~ .data[[flow]],
        .data[[matnames]] == V ~ .data[[product]],
        .data[[matnames]] == R ~ .data[[product]],
        .data[[matnames]] == Y ~ .data[[flow]],
        .data[[matnames]] == epsilon ~ .data[[flow]],
        TRUE ~ NA_character_
      ),
      !!as.name(rowtypes) := dplyr::case_when(
        startsWith(.data[[matnames]], U) ~ product_type,
        .data[[matnames]] == R ~ resource_type,
        .data[[matnames]] == V ~ industry_type,
        .data[[matnames]] == Y ~ product_type,
        .data[[matnames]] == epsilon ~ product_type,
        TRUE ~ NA_character_
      ),
      !!as.name(coltypes) := dplyr::case_when(
        startsWith(.data[[matnames]], U) ~ industry_type,
        .data[[matnames]] == R ~ product_type,
        .data[[matnames]] == V ~ product_type,
        .data[[matnames]] == Y ~ sector_type,
        .data[[matnames]] == epsilon ~ sector_type,
        TRUE ~ NA_character_
      )
    )
}




collapse_to_tidy_psut_epsilon <- function(.tidy_iea_df,
                                  # Names of input columns
                                  ledger_side = IEATools::iea_cols$ledger_side,
                                  flow_aggregation_point = IEATools::iea_cols$flow_aggregation_point,
                                  flow = IEATools::iea_cols$flow,
                                  product = IEATools::iea_cols$product,
                                  e_dot = IEATools::iea_cols$e_dot,
                                  unit = IEATools::iea_cols$unit,
                                  epsilon = "Epsilon",
                                  matnames = IEATools::mat_meta_cols$matnames,
                                  rownames = IEATools::mat_meta_cols$rownames,
                                  colnames = IEATools::mat_meta_cols$colnames,
                                  rowtypes = IEATools::mat_meta_cols$rowtypes,
                                  coltypes = IEATools::mat_meta_cols$coltypes,
                                  # Name of output column of matrices
                                  matvals = IEATools::psut_cols$matvals){
  matsindf::verify_cols_missing(.tidy_iea_df, matvals)
  .tidy_iea_df %>%
    dplyr::mutate(
      # All values in the matrices must be positive - BUT BALANCING TERMS.
      "{e_dot}" := case_when(
        .data[[matnames]] == epsilon ~ .data[[e_dot]],
        TRUE ~ abs(.data[[e_dot]])
      )
    ) %>%
    dplyr::mutate(
      # Eliminate columns that we no longer need.
      # Set to NULL in mutate, because if the columns are missing,
      # perhaps because the caller already deleted them,
      # no errors are given.
      "{ledger_side}" := NULL,
      "{flow_aggregation_point}" := NULL,
      "{unit}" := NULL,
      "{flow}" := NULL,
      "{product}" := NULL
    ) %>%
    # We assume that everything remaining is a metadata column.
    matsindf::group_by_everything_except(e_dot, rownames, colnames, rowtypes, coltypes) %>%
    # Now we can collapse!
    matsindf::collapse_to_matrices(matnames = matnames, matvals = e_dot,
                                   rownames = rownames, colnames = colnames,
                                   rowtypes = rowtypes, coltypes = coltypes) %>%
    dplyr::rename(
      "{matvals}" := .data[[e_dot]]
    ) %>%
    dplyr::ungroup()
}






prep_psut_epsilon <- function(.tidy_iea_df,
                      year = IEATools::iea_cols$year,
                      ledger_side = IEATools::iea_cols$ledger_side,
                      flow_aggregation_point = IEATools::iea_cols$flow_aggregation_point,
                      flow = IEATools::iea_cols$flow,
                      product = IEATools::iea_cols$product,
                      e_dot = IEATools::iea_cols$e_dot,
                      unit = IEATools::iea_cols$unit,
                      supply = IEATools::ledger_sides$supply,
                      consumption = IEATools::ledger_sides$consumption,
                      matnames = IEATools::mat_meta_cols$matnames,
                      rownames = IEATools::mat_meta_cols$rownames,
                      colnames = IEATools::mat_meta_cols$colnames,
                      rowtypes = IEATools::mat_meta_cols$rowtypes,
                      coltypes = IEATools::mat_meta_cols$coltypes,
                      matvals = IEATools::psut_cols$matvals,
                      R = IEATools::psut_cols$R,
                      U_eiou = IEATools::psut_cols$U_eiou,
                      U_feed = IEATools::psut_cols$U_feed,
                      r_eiou = IEATools::psut_cols$r_eiou,
                      U = IEATools::psut_cols$U,
                      V = IEATools::psut_cols$V,
                      Y = IEATools::psut_cols$Y,
                      s_units = IEATools::psut_cols$s_units){
  if (nrow(.tidy_iea_df) == 0) {
    # We can get a no-row data frame for .tidy_iea_df.
    # If so, we should return a no-row data frame with empty columns added.
    meta_columns <- meta_cols(.tidy_iea_df,
                              return_names = TRUE,
                              not_meta = c(ledger_side, flow_aggregation_point, flow, product, e_dot, unit))
    out <- .tidy_iea_df %>%
      dplyr::select(!!!meta_columns, !!year)
    # Make a tibble with no rows for the remainder of the columns,
    # R, U_eiou, U_feed, V, Y, S_units, Epsilon (7 in total)
    # Use 1.1 for the value so that columns are created as double type columns.
    mats_cols <- as.list(rep(1.1, 7)) %>%
      magrittr::set_names(c(R, U_eiou, U_feed, V, Y, s_units, Epsilon)) %>%
      as.data.frame()
    # Eliminate the row in the data frame
    zero_length_mats_cols <- mats_cols[0, ]
    # Join to out
    return(dplyr::bind_cols(out, zero_length_mats_cols))
  }

  # We actually have some rows in .tidy_iea_df, so work with them
  S_units <- extract_S_units_from_tidy(.tidy_iea_df,
                                       product = product,
                                       unit = unit)
  # Bundle functions together
  Temp <- .tidy_iea_df %>%
    # Add matrix names
    add_psut_matnames_epsilon(ledger_side = ledger_side, supply = supply, consumption = consumption) %>%
    # Add additional metadata
    add_row_col_meta_epsilon(flow = flow, product = product, matnames = matnames)
  Collapsed <- Temp %>%
    # Now collapse to matrices
    collapse_to_tidy_psut_epsilon(e_dot = e_dot, matnames = matnames, matvals = matvals, rownames = rownames, colnames = colnames,
                          rowtypes = rowtypes, coltypes = coltypes)
  # Get a list of matrix names for future use
  matrix_names <- Collapsed[[matnames]] %>%
    unique() %>%
    # We add U and r_eiou later, so append them here.
    append(U) %>%
    append(r_eiou)
  # Spread to put each matrix into its own column
  CollapsedSpread <- Collapsed %>%
    tidyr::spread(key = matnames, value = matvals) %>%
    dplyr::mutate(
      # Add the U matrix.
      "{U}" := matsbyname::sum_byname(.data[[U_feed]], .data[[U_eiou]]),
      # Add r_EIOU matrices
      # Create r_EIOU, a matrix that identifies the ratio of EIOU to total energy used.
      "{r_eiou}" := matsbyname::quotient_byname(.data[[U_eiou]], U) %>%
        matsbyname::replaceNaN_byname(val = 0)
    ) %>%
    # Rearrange columns to get more-natural locations for the U and r_EIOU matrices.
    dplyr::relocate(.data[[U]], .after = .data[[U_feed]]) %>%
    dplyr::relocate(.data[[r_eiou]], .after = .data[[U]])

  CollapsedSpread %>%
    # Add the S_units matrix and return
    dplyr::full_join(S_units, by = matsindf::everything_except(CollapsedSpread, matrix_names, .symbols = FALSE))
}


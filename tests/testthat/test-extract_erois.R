###########################################################
context("Extracting EROIs")
###########################################################


test_that("Extract tidy product EROIs works", {

  EIOU_mats <- Recca::UKEnergy2000mats %>%
    dplyr::filter(Last.stage == "Final", Energy.type == "E") %>%
    tidyr::pivot_wider(names_from = "matrix.name", values_from = "matrix") %>%
    Recca::calc_io_mats() %>%
    Recca::calc_E_EIOU()

  EROI_mats <- EIOU_mats %>% Recca::calc_erois()

  tidy_product_EROIs <- EROI_mats %>%
    dplyr::mutate(
      Method = "PCM"
    ) %>%
    extract_tidy_product_erois()

# Checking a few values:
  tidy_product_EROIs %>%
    dplyr::filter(
      Type == "Gross",
      Boundary == "All",
      Product == "Diesel"
    ) %>%
    magrittr::extract2("EROI") %>%
    expect_equal(5.09969733020006)

  tidy_product_EROIs %>%
    dplyr::filter(
      Type == "Net",
      Boundary == "All",
      Product == "Diesel"
    ) %>%
    magrittr::extract2("EROI") %>%
    expect_equal(4.09969733020006)

  tidy_product_EROIs %>%
    dplyr::filter(
      Type == "Gross",
      Boundary == "All",
      Product == "NG - Wells"
    ) %>%
    magrittr::extract2("EROI") %>%
    expect_equal(19.6173481664719)

  tidy_product_EROIs %>%
    dplyr::filter(
      Type == "Net",
      Boundary == "All",
      Product == "NG - Wells"
    ) %>%
    magrittr::extract2("EROI") %>%
    expect_equal(18.6173481664719)

  tidy_product_EROIs %>%
    dplyr::filter(
      Type == "Gross",
      Boundary == "Feedstock",
      Product == "Petrol"
    ) %>%
    magrittr::extract2("EROI") %>%
    expect_equal(5.84598162410927)

  tidy_product_EROIs %>%
    dplyr::filter(
      Type == "Net",
      Boundary == "Feedstock",
      Product == "Petrol"
    ) %>%
    magrittr::extract2("EROI") %>%
    expect_equal(4.84598162410927)

  tidy_product_EROIs %>%
    dplyr::filter(
      Type == "Gross",
      Boundary == "Feedstock",
      Product == "NG"
    ) %>%
    magrittr::extract2("EROI") %>%
    expect_equal(Inf)
})



test_that("Extract tidy industry EROIs works", {

  EIOU_mats <- Recca::UKEnergy2000mats %>%
    dplyr::filter(Last.stage == "Final", Energy.type == "E") %>%
    tidyr::pivot_wider(names_from = "matrix.name", values_from = "matrix") %>%
    Recca::calc_io_mats() %>%
    Recca::calc_E_EIOU()

  EROI_mats <- EIOU_mats %>% Recca::calc_erois()

  tidy_industry_EROIs <- EROI_mats %>%
    dplyr::mutate(
      Method = "PCM"
    ) %>%
    extract_tidy_industry_erois()


  # Checking a few values:
  tidy_industry_EROIs %>%
    dplyr::filter(
      Type == "Gross",
      Boundary == "All",
      Industry_name == "Elect. grid"
    ) %>%
    magrittr::extract2("EROI") %>%
    expect_equal(6.57766701757502)

  tidy_industry_EROIs %>%
    dplyr::filter(
      Type == "Net",
      Boundary == "All",
      Industry_name == "Elect. grid"
    ) %>%
    magrittr::extract2("EROI") %>%
    expect_equal(5.57766701757502)

  tidy_industry_EROIs %>%
    dplyr::filter(
      Type == "Gross",
      Boundary == "All",
      Industry_name == "Oil refineries"
    ) %>%
    magrittr::extract2("EROI") %>%
    expect_equal(5.09969733020006)

  tidy_industry_EROIs %>%
    dplyr::filter(
      Type == "Net",
      Boundary == "All",
      Industry_name == "Oil refineries"
    ) %>%
    magrittr::extract2("EROI") %>%
    expect_equal(4.09969733020006)

  tidy_industry_EROIs %>%
    dplyr::filter(
      Type == "Gross",
      Boundary == "Feedstock",
      Industry_name == "Diesel dist."
    ) %>%
    magrittr::extract2("EROI") %>%
    expect_equal(5.16426718119679)

  tidy_industry_EROIs %>%
    dplyr::filter(
      Type == "Net",
      Boundary == "Feedstock",
      Industry_name == "Diesel dist."
    ) %>%
    magrittr::extract2("EROI") %>%
    expect_equal(4.16426718119679)
})

###########################################################
context("Transform helpers")
###########################################################

#### Testing helper functions ####

test_that("calc_total_consumption_by_product works", {

  A_B_path <- system.file("extdata/A_B_data_full_2018_format.csv", package = "ECCTools")

  AB_data <- A_B_path %>%
    IEATools::load_tidy_iea_df() %>%
    IEATools::specify_all()

  total_consumption_by_product <- AB_data %>%
    IEATools::add_psut_matnames() %>%
    calc_total_consumption_by_product()

  expect_equal(dim(total_consumption_by_product), c(21, 8))

  expect_equal(total_consumption_by_product %>%
                 dplyr::filter(
                   Country == "A",
                   Product == "Blast furnace gas"
                 ) %>%
                 dplyr::select(Total_Consumption_By_Product) %>%
                 dplyr::pull(),
               850)

  expect_equal(total_consumption_by_product %>%
                 dplyr::filter(
                   Country == "A",
                   Product == "Crude oil"
                 ) %>%
                 dplyr::select(Total_Consumption_By_Product) %>%
                 dplyr::pull(),
               5000)

  expect_equal(total_consumption_by_product %>%
                 dplyr::filter(
                   Country == "A",
                   Product == "Crude oil [from Resources]"
                 ) %>%
                 dplyr::select(Total_Consumption_By_Product) %>%
                 dplyr::pull(),
               8500)

  expect_equal(total_consumption_by_product %>%
                 dplyr::filter(
                   Country == "B",
                   Product == "Crude oil"
                 ) %>%
                 dplyr::select(Total_Consumption_By_Product) %>%
                 dplyr::pull(),
               3000)
})



test_that("calc_imports_by_product works", {
  A_B_path <- system.file("extdata/A_B_data_full_2018_format.csv", package = "ECCTools")

  AB_data <- A_B_path %>%
    IEATools::load_tidy_iea_df() %>%
    IEATools::specify_all()

  imports_by_product <- AB_data %>%
    calc_imports_by_product()

  expect_equal(imports_by_product %>%
                 dplyr::filter(
                   Country == "A",
                   Flow == "Imports [of Coke oven coke]",
                   Product == "Coke oven coke"
                 ) %>%
                 dplyr::select(Imports) %>%
                 dplyr::pull(),
               600)

  expect_equal(imports_by_product %>%
                 dplyr::filter(
                   Country == "B" &
                     stringr::str_detect(Product, "Crude oil")
                 ) %>%
                 dplyr::select(Imports) %>%
                 dplyr::pull(),
               3000)
})



test_that("calc_share_imports_by_product works", {
  A_B_path <- system.file("extdata/A_B_data_full_2018_format.csv", package = "ECCTools")

  AB_data <- A_B_path %>%
    IEATools::load_tidy_iea_df() %>%
    IEATools::specify_all()

  share_imports_by_product <- AB_data %>%
    IEATools::add_psut_matnames() %>%
    calc_share_imports_by_products()

  # Check all shares <= 1
  expect_equal(share_imports_by_product %>%
                 dplyr::filter(Share_Imports_By_Product > 1) %>%
                 dplyr::select(Country) %>%
                 dplyr::pull() %>%
                 length(),
               0)

  # Check specific values
  expect_equal(share_imports_by_product %>%
                 dplyr::filter(
                   Country == "A",
                   Product == "Coke oven coke"
                 ) %>%
                 dplyr::select(Share_Imports_By_Product) %>%
                 dplyr::pull(),
               0.6)


  expect_equal(share_imports_by_product %>%
                 dplyr::filter(
                   Country == "A",
                   Product == "Heat"
                 ) %>%
                 dplyr::select(Share_Imports_By_Product) %>%
                 dplyr::pull(),
               0)

  expect_equal(share_imports_by_product %>%
                 dplyr::filter(
                   Country == "B",
                   Product == "Natural gas"
                 ) %>%
                 dplyr::select(Share_Imports_By_Product) %>%
                 dplyr::pull(),
               1)
})



test_that("calc_global_exports works", {

  A_B_path <- system.file("extdata/A_B_data_full_2018_format.csv", package = "ECCTools")

  AB_data <- A_B_path %>%
    IEATools::load_tidy_iea_df() %>%
    IEATools::specify_all()

  global_exports_per_product <- AB_data %>%
    calc_global_exports()

  expect_equal(global_exports_per_product %>%
                 dplyr::filter(Product == "Coke oven coke") %>%
                 dplyr::select(Global_Exports_By_Product) %>%
                 dplyr::pull(),
               400)

  expect_equal(global_exports_per_product %>%
                 dplyr::filter(Product == "Crude oil") %>%
                 dplyr::select(Global_Exports_By_Product) %>%
                 dplyr::pull(),
               3500)
})



test_that("calc_share_exports_by_product works", {

  A_B_path <- system.file("extdata/A_B_data_full_2018_format.csv", package = "ECCTools")

  AB_data <- A_B_path %>%
    IEATools::load_tidy_iea_df() %>%
    IEATools::specify_all()

  share_global_exports_per_product <- AB_data %>%
    IEATools::add_psut_matnames() %>%
    calc_share_exports_by_product()

  expect_equal(dim(share_global_exports_per_product),
               c(4, 7))

  expect_equal(share_global_exports_per_product %>%
                 dplyr::filter(
                   Provenience == "A"
                 ) %>%
                 dplyr::select(Share_Exports_By_Product) %>%
                 dplyr::pull(),
               c(1, 1, 1))

  expect_equal(share_global_exports_per_product %>%
                 dplyr::filter(
                   Provenience == "B",
                   Product == "Coke oven coke"
                 ) %>%
                 dplyr::select(Share_Exports_By_Product) %>%
                 dplyr::pull(),
               1)
})


test_that("calc_national_production_by_product works", {

  A_B_path <- system.file("extdata/A_B_data_full_2018_format.csv", package = "ECCTools")

  AB_data <- A_B_path %>%
    IEATools::load_tidy_iea_df() %>%
    IEATools::specify_all()

  national_production_by_product <- AB_data %>%
    IEATools::add_psut_matnames() %>%
    calc_national_production_by_product()

  national_production_by_product %>%
    dplyr::filter(Country == "A" & Product == "Crude oil") %>%
    magrittr::extract2("National_Production_By_Product") %>%
    expect_equal(8500)

  national_production_by_product %>%
    dplyr::filter(Country == "A" & Product == "Coke oven coke") %>%
    magrittr::extract2("National_Production_By_Product") %>%
    expect_equal(400)

  national_production_by_product %>%
    dplyr::filter(Country == "A" & Product == "Heat") %>%
    magrittr::extract2("National_Production_By_Product") %>%
    expect_equal(250)

  national_production_by_product %>%
    dplyr::filter(Country == "A" & Product == "Natural gas [from Resources]") %>%
    magrittr::extract2("National_Production_By_Product") %>%
    expect_equal(4000)

  national_production_by_product %>%
    dplyr::filter(Country == "A" & Product == "Natural gas") %>%
    magrittr::extract2("National_Production_By_Product") %>%
    expect_equal(4100)

  national_production_by_product %>%
    dplyr::filter(Country == "B" & Product == "Electricity") %>%
    magrittr::extract2("National_Production_By_Product") %>%
    expect_equal(1000)

  national_production_by_product %>%
    dplyr::filter(Country == "B" & Product == "Blast furnace gas") %>%
    magrittr::extract2("National_Production_By_Product") %>%
    expect_equal(850)

  national_production_by_product %>%
    dplyr::filter(Country == "B" & Product == "Crude oil") %>%
    magrittr::extract2("National_Production_By_Product") %>%
    length() %>%
    expect_equal(0)
})



test_that("calc_global_production_by_product works", {

  A_B_path <- system.file("extdata/A_B_data_full_2018_format.csv", package = "ECCTools")

  AB_data <- A_B_path %>%
    IEATools::load_tidy_iea_df() %>%
    IEATools::specify_all()

  global_production_by_product <- AB_data %>%
    IEATools::add_psut_matnames() %>%
    calc_global_production_by_product()

  global_production_by_product %>%
    dplyr::filter(Product == "Crude oil") %>%
    magrittr::extract2("Global_Production_By_Product") %>%
    expect_equal(8500)

  global_production_by_product %>%
    dplyr::filter(Product == "Blast furnace gas") %>%
    magrittr::extract2("Global_Production_By_Product") %>%
    expect_equal(1700)

  global_production_by_product %>%
    dplyr::filter(Product == "Coke oven coke") %>%
    magrittr::extract2("Global_Production_By_Product") %>%
    expect_equal(1800)

  global_production_by_product %>%
    dplyr::filter(Product == "Heat") %>%
    magrittr::extract2("Global_Production_By_Product") %>%
    expect_equal(750)

  global_production_by_product %>%
    dplyr::filter(Product == "Natural gas") %>%
    magrittr::extract2("Global_Production_By_Product") %>%
    expect_equal(4100)
})


test_that("calc_share_global_production_by_product works", {

  A_B_path <- system.file("extdata/A_B_data_full_2018_format.csv", package = "ECCTools")

  AB_data <- A_B_path %>%
    IEATools::load_tidy_iea_df() %>%
    IEATools::specify_all()

  share_production_by_product <- AB_data %>%
    IEATools::add_psut_matnames() %>%
    calc_share_global_production_by_product()

  share_production_by_product %>%
    dplyr::filter(Producing_Country == "A", Product == "Crude oil") %>%
    magrittr::extract2("Share_Global_Production_By_Product") %>%
    expect_equal(1)

  share_production_by_product %>%
    dplyr::filter(Producing_Country == "A", Product == "Blast furnace gas") %>%
    magrittr::extract2("Share_Global_Production_By_Product") %>%
    expect_equal(0.5)

  share_production_by_product %>%
    dplyr::filter(Producing_Country == "A", Product == "Electricity") %>%
    magrittr::extract2("Share_Global_Production_By_Product") %>%
    expect_equal(0.7619048,
                 tolerance = 0.0001)

  share_production_by_product %>%
    dplyr::filter(Producing_Country == "A", Product == "Crude oil") %>%
    magrittr::extract2("Share_Global_Production_By_Product") %>%
    expect_equal(1)

  share_production_by_product %>%
    dplyr::filter(Producing_Country == "B", Product == "Kerosene type jet fuel excl. biofuels") %>%
    magrittr::extract2("Share_Global_Production_By_Product") %>%
    expect_equal(0.3636364,
                 tolerance = 0.0001)

  share_production_by_product %>%
    dplyr::filter(Producing_Country == "B", Product == "Coke oven coke") %>%
    magrittr::extract2("Share_Global_Production_By_Product") %>%
    expect_equal(0.7777778,
                 tolerance = 0.0001)

  share_production_by_product %>%
    dplyr::group_by(Method, Energy.type, Last.stage, Year, Product, Unit) %>%
    dplyr::summarise(
      total_share_by_product = sum(Share_Global_Production_By_Product)
    ) %>%
    dplyr::mutate(
      is_equal_unity = total_share_by_product == 1
    ) %>%
    magrittr::extract2("is_equal_unity") %>%
    all() %>%
    expect_true()
})

###########################################################
context("Transforming PSUT")
###########################################################


# Testing Domestic Technology Assumption functions

test_that("find_list_dta_observations works", {

  A_B_path <- file.path("../../inst/A_B_data_full_2018_format.csv")

  AB_data <- A_B_path %>%
    IEATools::load_tidy_iea_df() %>%
    IEATools::specify_all() %>%
    add_psut_matnames_epsilon()

  # Checking that the DTA assumption can only be applied to country A:
  expect_equal(AB_data %>% find_list_dta_observations(), "A_PCM_E_Final_2018")

  # Now, adding observations
  # Now, add more observations.
  to_add_1 <- AB_data %>%
    dplyr::mutate(
      Year = 1989
    ) %>%
    dplyr::filter(! Product %in% c("Coking coal", "Crude oil", "Natural gas", "Coking coal [of Coal mines]", "Crude oil [of Oil and gas extraction]", "Natural gas [of Oil and gas extraction]")) %>%
    dplyr::add_row(Country = "A", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 1989, Ledger.side = "Consumption", Flow.aggregation.point = "Industry", Flow = "a weird industry", Product = "A_very_weird_product", Unit = "ktoe", E.dot = 200, matnames = "Y") %>%
    dplyr::add_row(Country = "A", Method = "A_weird_method", Energy.type = "E", Last.stage = "Final", Year = 1989, Ledger.side = "Supply", Flow.aggregation.point = "Industry", Flow = "a weird industry", Product = "A_very_weird_product", Unit = "ktoe", E.dot = 200, matnames = "V")

  to_add_2 <- AB_data %>%
    dplyr::mutate(
      Method = "crash-test-metod"
    ) %>%
    dplyr::filter(! Product %in% c("Coking coal", "Crude oil", "Natural gas", "Coking coal [of Coal mines]", "Crude oil [of Oil and gas extraction]", "Natural gas [of Oil and gas extraction]"))

  testing <- AB_data %>% dplyr::bind_rows(to_add_1, to_add_2)

  # Finding dta observations
  list_dta_observations <- testing %>%
    find_list_dta_observations()

  # There should be 4 of them
  expect_equal(length(list_dta_observations), 4)

  # All should be in this list
  are_observations_correct <- list_dta_observations %in% c("A_crash-test-metod_E_Final_2018", "A_PCM_E_Final_2018", "B_crash-test-metod_E_Final_2018", "B_PCM_E_Final_1989")

  is_list_correct <- ! (FALSE %in% are_observations_correct)

  expect_true(is_list_correct)
})



test_that("transform_to_dta works", {

  A_B_path <- file.path("../../inst/A_B_data_full_2018_format.csv")

  AB_data <- A_B_path %>%
    IEATools::load_tidy_iea_df() %>%
    IEATools::specify_all() %>%
    add_psut_matnames_epsilon()


  # Adding observations
  to_add_1 <- AB_data %>%
    dplyr::mutate(
      Year = 1989
    ) %>%
    dplyr::filter(! Product %in% c("Coking coal", "Crude oil", "Natural gas", "Coking coal [of Coal mines]", "Crude oil [of Oil and gas extraction]", "Natural gas [of Oil and gas extraction]")) %>%
    dplyr::add_row(Country = "A", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 1989, Ledger.side = "Consumption", Flow.aggregation.point = "Industry", Flow = "a weird industry", Product = "A_very_weird_product", Unit = "ktoe", E.dot = 200, matnames = "Y") %>%
    dplyr::add_row(Country = "A", Method = "A_weird_method", Energy.type = "E", Last.stage = "Final", Year = 1989, Ledger.side = "Supply", Flow.aggregation.point = "Industry", Flow = "a weird industry", Product = "A_very_weird_product", Unit = "ktoe", E.dot = 200, matnames = "V")

  to_add_2 <- AB_data %>%
    dplyr::mutate(
      Method = "crash-test-metod"
    ) %>%
    dplyr::filter(! Product %in% c("Coking coal", "Crude oil", "Natural gas", "Coking coal [of Coal mines]", "Crude oil [of Oil and gas extraction]", "Natural gas [of Oil and gas extraction]"))

  testing <- AB_data %>% dplyr::bind_rows(to_add_1, to_add_2)

  # Calculating dta
  testing_dta <- testing %>%
    transform_to_dta()

  # Checking that there are no imports when Epsilon is not in ledger_side
  n <- testing_dta %>%
    dplyr::filter(stringr::str_detect(.data[["Flow"]], "Imports")) %>%
    dplyr::filter(stringr::str_detect(.data[["Ledger.side"]], "Epsilon")) %>%
    dplyr::summarise(
      n = dplyr::n()
    ) %>%
    dplyr::pull()

  expect_equal(n, 2)

  # Checking that all relevant observations have a dta outcome
  list_observations_included <- testing_dta %>%
    tidyr::expand(tidyr::nesting(Country, Method, Energy.type, Last.stage, Year)) %>%
    tidyr::unite(Observations_included, Country, Method, Energy.type, Last.stage, Year, sep = "_") %>%
    dplyr::pull()

  expect_equal(length(list_observations_included), 4)

  # All should be in this list
  are_observations_correct <- list_observations_included %in% c("A_crash-test-metod_E_Final_2018", "A_PCM_E_Final_2018", "B_crash-test-metod_E_Final_2018", "B_PCM_E_Final_1989")

  is_list_correct <- ! (FALSE %in% are_observations_correct)

  expect_true(is_list_correct)

})




# Testing Global Market Assumption functions

# Specifying R matrix
test_that("specify_MR_R works", {

  A_B_path <- file.path("../../inst/A_B_data_full_2018_format.csv")

  AB_data <- A_B_path %>%
    IEATools::load_tidy_iea_df() %>%
    specify_all_revisited()

  AB_resources_MR <- AB_data %>%
    add_psut_matnames_epsilon() %>%
    specify_MR_R()


  expect_equal(dim(AB_resources_MR)[1], 3)

  expect_equal(AB_resources_MR %>%
                 dplyr::filter(Product == "{A}_Coking coal") %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               5000)

  expect_equal(AB_resources_MR %>%
                 dplyr::filter(Product == "{A}_Crude oil") %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               8500)

  expect_equal(AB_resources_MR %>%
                dplyr::filter(Flow.aggregation.point != "Total primary energy supply") %>%
                dim(),
               c(0, 12))

  expect_equal(AB_resources_MR %>%
                 dplyr::filter(! stringr::str_detect(Flow, "Resources")) %>%
                 dim(),
               c(0, 12))

  expect_equal(AB_resources_MR %>%
                 dplyr::filter(stringr::str_detect(Flow, "\\{B\\}") | stringr::str_detect(Product, "\\{B\\}")) %>%
                 dim(),
               c(0, 12))
})


# Specifying V matrix
test_that("specify_MR_V works", {

  A_B_path <- file.path("../../inst/A_B_data_full_2018_format.csv")

  # We wheck what happens when we add an epsilon flow to the supply side.
  AB_data <- A_B_path %>%
    IEATools::load_tidy_iea_df() %>%
    specify_all_revisited() %>%
    tibble::add_row(
      Country = "A",
      Method = "PCM",
      Energy.type = "E",
      Last.stage = "Final",
      Year = 2018,
      Ledger.side = "Supply",
      Flow.aggregation.point = "Total primary energy supply",
      Flow = "Stock changes [of Coke oven coke]",
      Product = "Coke oven coke",
      Unit = "ktoe",
      E.dot = -100
    ) %>%
    tibble::add_row(
      Country = "A",
      Method = "PCM",
      Energy.type = "E",
      Last.stage = "Final",
      Year = 2018,
      Ledger.side = "Supply",
      Flow.aggregation.point = "Total primary energy supply",
      Flow = "Stock changes [of a very odd product]",
      Product = "a very odd product",
      Unit = "ktoe",
      E.dot = +100
    )

  AB_supply_MR <- AB_data %>%
    stock_changes_to_epsilon() %>%
    add_psut_matnames_epsilon() %>%
    specify_MR_V()


  expect_equal(dim(AB_supply_MR),
               c(19, 12))


  expect_equal(AB_supply_MR %>%
                 dplyr::filter(
                   Flow == "{A}_Oil refineries",
                   Product == "{A}_Heat"
                 ) %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               50)

  expect_equal(AB_supply_MR %>%
                 dplyr::filter(
                   Flow == "{A}_Blast furnaces",
                   Product == "{A}_Blast furnace gas"
                 ) %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               850)

  expect_equal(AB_supply_MR %>%
                 dplyr::filter(
                   Flow == "{B}_Oil refineries",
                   Product == "{B}_Heat"
                 ) %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               200)

  expect_equal(AB_supply_MR %>%
                 dplyr::filter(stringr::str_detect(Flow, "Imports")) %>%
                 dim(),
               c(0, 12))

  expect_equal(AB_supply_MR %>%
                 dplyr::filter(
                   Flow == "{A}_Stock changes [of a very odd product]",
                   Product == "{A}_a very odd product"
                 ) %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               -100)

  expect_equal(AB_supply_MR %>%
                 dplyr::filter(
                   Flow == "{A}_Stock changes [of Coke oven coke]",
                   Product == "{A}_Coke oven coke"
                 ) %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull() %>%
                 length(),
               0)
})




#### Helper functions ####

test_that("calc_total_consumption_by_product works", {

  A_B_path <- file.path("../../inst/A_B_data_full_2018_format.csv")

  AB_data <- A_B_path %>%
    IEATools::load_tidy_iea_df() %>%
    specify_all_revisited()

  total_consumption_by_product <- AB_data %>%
    add_psut_matnames_epsilon() %>%
    calc_total_consumption_by_product()

  expect_equal(dim(total_consumption_by_product), c(21, 8))

  expect_equal(total_consumption_by_product %>%
                 dplyr::filter(
                   Country == "A",
                   Product == "Blast furnace gas"
                 ) %>%
                 dplyr::select(Total_Consumption_From_Func) %>%
                 dplyr::pull(),
               850)

  expect_equal(total_consumption_by_product %>%
                 dplyr::filter(
                   Country == "A",
                   Product == "Crude oil [of Oil and gas extraction]"
                 ) %>%
                 dplyr::select(Total_Consumption_From_Func) %>%
                 dplyr::pull(),
               5000)

  expect_equal(total_consumption_by_product %>%
                 dplyr::filter(
                   Country == "A",
                   Product == "Crude oil"
                 ) %>%
                 dplyr::select(Total_Consumption_From_Func) %>%
                 dplyr::pull(),
               8500)

  expect_equal(total_consumption_by_product %>%
                 dplyr::filter(
                   Country == "B",
                   Product == "Crude oil [of Oil and gas extraction]"
                 ) %>%
                 dplyr::select(Total_Consumption_From_Func) %>%
                 dplyr::pull(),
               3000)
})



test_that("calc_imports_by_product works", {
  A_B_path <- file.path("../../inst/A_B_data_full_2018_format.csv")

  AB_data <- A_B_path %>%
    IEATools::load_tidy_iea_df() %>%
    specify_all_revisited()

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
  A_B_path <- file.path("../../inst/A_B_data_full_2018_format.csv")

  AB_data <- A_B_path %>%
    IEATools::load_tidy_iea_df() %>%
    specify_all_revisited()

  share_imports_by_product <- AB_data %>%
    add_psut_matnames_epsilon() %>%
    calc_share_imports_by_products()

  # Check all shares <= 1
  expect_equal(share_imports_by_product %>%
                 dplyr::filter(Share_Imports_From_Func > 1) %>%
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
                 dplyr::select(Share_Imports_From_Func) %>%
                 dplyr::pull(),
               0.6)


  expect_equal(share_imports_by_product %>%
                 dplyr::filter(
                   Country == "A",
                   Product == "Heat"
                 ) %>%
                 dplyr::select(Share_Imports_From_Func) %>%
                 dplyr::pull(),
               0)

  expect_equal(share_imports_by_product %>%
                 dplyr::filter(
                   Country == "B",
                   Product == "Natural gas [of Oil and gas extraction]"
                 ) %>%
                 dplyr::select(Share_Imports_From_Func) %>%
                 dplyr::pull(),
               1)
})



test_that("specify_imported_products works", {

  A_B_path <- file.path("../../inst/A_B_data_full_2018_format.csv")

  AB_data <- A_B_path %>%
    IEATools::load_tidy_iea_df() %>%
    specify_all_revisited()

  defined_imported_products <- AB_data %>%
    add_psut_matnames_epsilon() %>%
    specify_imported_products()


  # Check that there is only Y and U flows in resulting data frame.
  expect_equal(defined_imported_products %>%
                 dplyr::filter(! matnames %in% c("Y", "U_feed", "U_EIOU")) %>%
                 dplyr::select(Country) %>%
                 dplyr::pull() %>%
                 length(),
               0)

  # Checking length of data frame
  expect_equal(defined_imported_products %>%
                 dim(),
               c(73, 13))

  # Check specific values
  expect_equal(defined_imported_products %>%
                 dplyr::filter(
                   Country == "A",
                   Flow == "Residential",
                   Product == "Coke oven coke",
                   Origin == "Domestic"
                 ) %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               48)

  expect_equal(defined_imported_products %>%
                 dplyr::filter(
                   Country == "A",
                   Flow == "Residential",
                   Product == "Coke oven coke",
                   Origin == "Imported"
                 ) %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               72)

  expect_equal(defined_imported_products %>%
                 dplyr::filter(
                   Country == "A",
                   Flow == "Coal mines",
                   Product == "Electricity",
                   Origin == "Domestic"
                 ) %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               -20)

  expect_equal(defined_imported_products %>%
                 dplyr::filter(
                   Country == "A",
                   Flow == "Coal mines",
                   Product == "Electricity",
                   Origin == "Imported"
                 ) %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull() %>%
                 length(),
               0)

  expect_equal(defined_imported_products %>%
                 dplyr::filter(
                   Country == "B",
                   Flow == "Blast furnaces",
                   Product == "Natural gas [of Oil and gas extraction]",
                   Origin == "Imported"
                 ) %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               -150)


  # We add a stock changes flow that will go to the Epsilon matrix for a more comprehensive test.
  AB_data <- A_B_path %>%
    IEATools::load_tidy_iea_df() %>%
    specify_all_revisited() %>%
    tibble::add_row(
      Country = "A",
      Method = "PCM",
      Energy.type = "E",
      Last.stage = "Final",
      Year = 2018,
      Ledger.side = "Supply",
      Flow.aggregation.point = "Total primary energy supply",
      Flow = "Stock changes [of Coke oven coke]",
      Product = "Coke oven coke",
      Unit = "ktoe",
      E.dot = -100
    ) %>%
    tibble::add_row(
      Country = "A",
      Method = "PCM",
      Energy.type = "E",
      Last.stage = "Final",
      Year = 2018,
      Ledger.side = "Supply",
      Flow.aggregation.point = "Total primary energy supply",
      Flow = "Stock changes [of a very odd product]",
      Product = "a very odd product",
      Unit = "ktoe",
      E.dot = +100
    )

  defined_imported_products <- AB_data %>%
    stock_changes_to_epsilon() %>%
    add_psut_matnames_epsilon() %>%
    specify_imported_products()

  # Checks.
  expect_equal(defined_imported_products %>%
                 dplyr::filter(matnames == "Epsilon") %>%
                 dplyr::select(Country) %>%
                 dplyr::pull() %>%
                 length(),
               2)

  expect_equal(defined_imported_products %>%
                 dplyr::filter(matnames == "Epsilon",
                               Product == "Coke oven coke",
                               Origin == "Domestic") %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               45.45455,
               tolerance = 0.0001)

  expect_equal(defined_imported_products %>%
                 dplyr::filter(matnames == "Epsilon",
                               Product == "Coke oven coke",
                               Origin == "Imported") %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               (100-45.45455),
               tolerance = 0.0001)

  expect_equal(defined_imported_products %>%
                 dplyr::filter(
                   Country == "A",
                   Flow == "Residential",
                   Product == "Coke oven coke",
                   Origin == "Domestic"
                 ) %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               54.54546,
               tolerance = 0.0001)
})



test_that("calc_global_exports works", {

  A_B_path <- file.path("../../inst/A_B_data_full_2018_format.csv")

  AB_data <- A_B_path %>%
    IEATools::load_tidy_iea_df() %>%
    specify_all_revisited()

  global_exports_per_product <- AB_data %>%
    calc_global_exports()

  expect_equal(global_exports_per_product %>%
                 dplyr::filter(Product == "Coke oven coke") %>%
                 dplyr::select(Total_Exports_From_Func) %>%
                 dplyr::pull(),
               400)

  expect_equal(global_exports_per_product %>%
                 dplyr::filter(Product == "Crude oil [of Oil and gas extraction]") %>%
                 dplyr::select(Total_Exports_From_Func) %>%
                 dplyr::pull(),
               3500)
})



test_that("calc_share_exports_by_product works", {

  A_B_path <- file.path("../../inst/A_B_data_full_2018_format.csv")

  AB_data <- A_B_path %>%
    IEATools::load_tidy_iea_df() %>%
    specify_all_revisited()

  share_global_exports_per_product <- AB_data %>%
    add_psut_matnames_epsilon() %>%
    calc_share_exports_by_product()

  expect_equal(dim(share_global_exports_per_product),
               c(4, 7))

  expect_equal(share_global_exports_per_product %>%
                 dplyr::filter(
                   Provenience == "A"
                 ) %>%
                 dplyr::select(Share_Exports_From_Func) %>%
                 dplyr::pull(),
               c(1, 1, 1))

  expect_equal(share_global_exports_per_product %>%
                 dplyr::filter(
                   Provenience == "B",
                   Product == "Coke oven coke"
                 ) %>%
                 dplyr::select(Share_Exports_From_Func) %>%
                 dplyr::pull(),
               1)
})


test_that("specify_MR_Y_U_gma works", {

  A_B_path <- file.path("../../inst/A_B_data_full_2018_format.csv")

  AB_data <- A_B_path %>%
    IEATools::load_tidy_iea_df() %>%
    specify_all_revisited()

  AB_MR_Y_U_gma <- AB_data %>%
    add_psut_matnames_epsilon() %>%
    specify_MR_Y_U_gma()

  # Check that there is only Y and U flows in resulting data frame.
  expect_equal(AB_MR_Y_U_gma %>%
                 dplyr::filter(! matnames %in% c("Y", "U_feed", "U_EIOU")) %>%
                 dplyr::select(Country) %>%
                 dplyr::pull() %>%
                 length(),
               0)

  # Checking length of data frame
  expect_equal(AB_MR_Y_U_gma %>%
                 dim(),
               c(73, 12))

  # Check specific values
  expect_equal(AB_MR_Y_U_gma %>%
                 dplyr::filter(
                   Flow == "{A}_Residential",
                   Product == "{A}_Coke oven coke"
                 ) %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               48)

  expect_equal(AB_MR_Y_U_gma %>%
                 dplyr::filter(
                   Flow == "{A}_Residential",
                   Product == "{B}_Coke oven coke"
                 ) %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               72)

  expect_equal(AB_MR_Y_U_gma %>%
                 dplyr::filter(
                   Flow == "{A}_Coal mines",
                   Product == "{A}_Electricity"
                 ) %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               -20)

  expect_equal(AB_MR_Y_U_gma %>%
                 dplyr::filter(
                   Flow == "{A}_Coal mines",
                   Product == "{B}_Electricity"
                 ) %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull() %>%
                 length(),
               0)

  expect_equal(AB_MR_Y_U_gma %>%
                 dplyr::filter(
                   Flow == "{B}_Blast furnaces",
                   Product == "{A}_Natural gas [of Oil and gas extraction]"
                 ) %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               -150)

  expect_equal(AB_MR_Y_U_gma %>%
                 dplyr::filter(
                   Flow == "{B}_Road",
                   Product == "{A}_Natural gas [of Oil and gas extraction]"
                 ) %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               180)

  # We add a stock changes flow that will go to the Epsilon matrix for a more comprehensive test.
  AB_data <- A_B_path %>%
    IEATools::load_tidy_iea_df() %>%
    specify_all_revisited() %>%
    tibble::add_row(
      Country = "A",
      Method = "PCM",
      Energy.type = "E",
      Last.stage = "Final",
      Year = 2018,
      Ledger.side = "Supply",
      Flow.aggregation.point = "Total primary energy supply",
      Flow = "Stock changes [of Coke oven coke]",
      Product = "Coke oven coke",
      Unit = "ktoe",
      E.dot = -100
    ) %>%
    tibble::add_row(
      Country = "A",
      Method = "PCM",
      Energy.type = "E",
      Last.stage = "Final",
      Year = 2018,
      Ledger.side = "Supply",
      Flow.aggregation.point = "Total primary energy supply",
      Flow = "Stock changes [of a very odd product]",
      Product = "a very odd product",
      Unit = "ktoe",
      E.dot = +100
    )

  AB_MR_Y_U_gma <- AB_data %>%
    stock_changes_to_epsilon() %>%
    add_psut_matnames_epsilon() %>%
    specify_MR_Y_U_gma()

  # Checks.
  expect_equal(AB_MR_Y_U_gma %>%
                 dplyr::filter(matnames == "Epsilon") %>%
                 dplyr::select(Country) %>%
                 dplyr::pull() %>%
                 length(),
               2)

  expect_equal(AB_MR_Y_U_gma %>%
                 dplyr::filter(matnames == "Epsilon",
                               Flow == "{A}_Stock changes [of Coke oven coke]",
                               Product == "{A}_Coke oven coke") %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               45.45455,
               tolerance = 0.0001)

  expect_equal(AB_MR_Y_U_gma %>%
                 dplyr::filter(matnames == "Epsilon",
                               Flow == "{A}_Stock changes [of Coke oven coke]",
                               Product == "{B}_Coke oven coke") %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               (100-45.45455),
               tolerance = 0.0001)

  expect_equal(AB_MR_Y_U_gma %>%
                 dplyr::filter(
                   Flow == "{A}_Residential",
                   Product == "{A}_Coke oven coke"
                 ) %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               54.54546,
               tolerance = 0.0001)
})



test_that("transform_to_gma works", {
  A_B_path <- file.path("../../inst/A_B_data_full_2018_format.csv")

  AB_data <- A_B_path %>%
    IEATools::load_tidy_iea_df() %>%
    specify_all_revisited()


  AB_MR_PSUT_gma <- AB_data %>%
    add_psut_matnames_epsilon() %>%
    transform_to_gma()


  expect_equal(dim(AB_MR_PSUT_gma),
                   c(94, 12))


  # Checking R matrix flows
  expect_equal(AB_MR_PSUT_gma %>%
                 dplyr::filter(Product == "{A}_Coking coal",
                               matnames == "R") %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               5000)

  expect_equal(AB_MR_PSUT_gma %>%
                 dplyr::filter(Product == "{A}_Crude oil",
                               matnames == "R") %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               8500)

  expect_equal(AB_MR_PSUT_gma %>%
                 dplyr::filter(Flow.aggregation.point != "Total primary energy supply",
                               matnames == "R") %>%
                 dim(),
               c(0, 12))

  expect_equal(AB_MR_PSUT_gma %>%
                 dplyr::filter(! stringr::str_detect(Flow, "Resources"),
                               matnames == "R") %>%
                 dim(),
               c(0, 12))

  expect_equal(AB_MR_PSUT_gma %>%
                 dplyr::filter(matnames == "R" & (stringr::str_detect(Flow, "\\{B\\}") | stringr::str_detect(Product, "\\{B\\}"))) %>%
                 dim(),
               c(0, 12))


  # Checking V matrix flows
  expect_equal(AB_MR_PSUT_gma %>%
                 dplyr::filter(
                   Flow == "{A}_Oil refineries",
                   Product == "{A}_Heat",
                   matnames == "V"
                 ) %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               50)

  expect_equal(AB_MR_PSUT_gma %>%
                 dplyr::filter(
                   Flow == "{A}_Blast furnaces",
                   Product == "{A}_Blast furnace gas",
                   matnames == "V"
                 ) %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               850)

  expect_equal(AB_MR_PSUT_gma %>%
                 dplyr::filter(
                   Flow == "{B}_Oil refineries",
                   Product == "{B}_Heat",
                   matnames == "V"
                 ) %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               200)

  expect_equal(AB_MR_PSUT_gma %>%
                 dplyr::filter(stringr::str_detect(Flow, "Imports")) %>%
                 dim(),
               c(0, 12))


  # Checking U and Y matrix flows

  # Check specific values
  expect_equal(AB_MR_PSUT_gma %>%
                 dplyr::filter(
                   Flow == "{A}_Residential",
                   Product == "{A}_Coke oven coke",
                   matnames == "Y"
                 ) %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               48)

  expect_equal(AB_MR_PSUT_gma %>%
                 dplyr::filter(
                   Flow == "{A}_Residential",
                   Product == "{B}_Coke oven coke",
                   matnames == "Y"
                 ) %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               72)

  expect_equal(AB_MR_PSUT_gma %>%
                 dplyr::filter(
                   Flow == "{A}_Coal mines",
                   Product == "{A}_Electricity",
                   matnames == "U_EIOU"
                 ) %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               -20)

  expect_equal(AB_MR_PSUT_gma %>%
                 dplyr::filter(
                   Flow == "{A}_Coal mines",
                   Product == "{B}_Electricity",
                   matnames == "U_EIOU"
                 ) %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull() %>%
                 length(),
               0)

  expect_equal(AB_MR_PSUT_gma %>%
                 dplyr::filter(
                   Flow == "{B}_Blast furnaces",
                   Product == "{A}_Natural gas [of Oil and gas extraction]",
                   matnames == "U_EIOU"
                 ) %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               -150)

  expect_equal(AB_MR_PSUT_gma %>%
                 dplyr::filter(
                   Flow == "{B}_Road",
                   Product == "{A}_Natural gas [of Oil and gas extraction]",
                   matnames == "Y"
                 ) %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               180)


  # Then just check with stock changes and Epsilon matrix too.
  AB_data <- A_B_path %>%
    IEATools::load_tidy_iea_df() %>%
    specify_all_revisited() %>%
    tibble::add_row(
      Country = "A",
      Method = "PCM",
      Energy.type = "E",
      Last.stage = "Final",
      Year = 2018,
      Ledger.side = "Supply",
      Flow.aggregation.point = "Total primary energy supply",
      Flow = "Stock changes [of Coke oven coke]",
      Product = "Coke oven coke",
      Unit = "ktoe",
      E.dot = -100
    ) %>%
    tibble::add_row(
      Country = "A",
      Method = "PCM",
      Energy.type = "E",
      Last.stage = "Final",
      Year = 2018,
      Ledger.side = "Supply",
      Flow.aggregation.point = "Total primary energy supply",
      Flow = "Stock changes [of a very odd product]",
      Product = "a very odd product",
      Unit = "ktoe",
      E.dot = +100
    )

  AB_MR_PSUT_gma <- AB_data %>%
    stock_changes_to_epsilon() %>%
    add_psut_matnames_epsilon() %>%
    transform_to_gma()

  # Length should be now 97.
  expect_equal(dim(AB_MR_PSUT_gma),
               c(97, 12))


  # Testing a couple of values
  expect_equal(AB_MR_PSUT_gma %>%
                 dplyr::filter(matnames == "Epsilon") %>%
                 dplyr::select(Country) %>%
                 dplyr::pull() %>%
                 length(),
               3)

  expect_equal(AB_MR_PSUT_gma %>%
                 dplyr::filter(
                   Flow == "{A}_Stock changes [of a very odd product]",
                   Product == "{A}_a very odd product",
                   matnames == "Epsilon"
                 ) %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               -100)

  expect_equal(AB_MR_PSUT_gma %>%
                 dplyr::filter(matnames == "Epsilon",
                               Flow == "{A}_Stock changes [of Coke oven coke]",
                               Product == "{A}_Coke oven coke") %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               45.45455,
               tolerance = 0.0001)

  expect_equal(AB_MR_PSUT_gma %>%
                 dplyr::filter(matnames == "Epsilon",
                               Flow == "{A}_Stock changes [of Coke oven coke]",
                               Product == "{B}_Coke oven coke") %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               (100-45.45455),
               tolerance = 0.0001)

  expect_equal(AB_MR_PSUT_gma %>%
                 dplyr::filter(
                   Flow == "{A}_Residential",
                   Product == "{A}_Coke oven coke",
                   matnames == "Y"
                 ) %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               54.54546,
               tolerance = 0.0001)

})



# Testing Bilateral Trade Assumption functions

test_that("calc_bilateral_trade_matrix_df_gma works", {

  A_B_path <- file.path("../../inst/A_B_data_full_2018_format.csv")

  AB_data <- A_B_path %>%
    IEATools::load_tidy_iea_df() %>%
    specify_all_revisited()

  bilateral_trade_matrix_df_gma <- AB_data %>%
    add_psut_matnames_epsilon() %>%
    calc_bilateral_trade_matrix_df_gma()

  # Check length
  expect_equal(dim(bilateral_trade_matrix_df_gma), c(8, 8))

  expect_equal(bilateral_trade_matrix_df_gma %>%
                 dplyr::filter(
                   Country == "A",
                   Product == "Coke oven coke") %>%
                 dplyr::select(Share_Exports_From_Func) %>%
                 dplyr::pull(),
               1)

  expect_equal(bilateral_trade_matrix_df_gma %>%
                 dplyr::filter(
                   Country == "A",
                   Product == "Natural gas [of Oil and gas extraction]") %>%
                 dplyr::select(Share_Exports_From_Func) %>%
                 dplyr::pull(),
               0)

  expect_equal(bilateral_trade_matrix_df_gma %>%
                 dplyr::filter(
                   Country == "B",
                   Product == "Natural gas [of Oil and gas extraction]") %>%
                 dplyr::select(Share_Exports_From_Func) %>%
                 dplyr::pull(),
               1)

  expect_equal(bilateral_trade_matrix_df_gma %>%
                 dplyr::filter(
                   Country == "B",
                   Product == "Coke oven coke") %>%
                 dplyr::select(Share_Exports_From_Func) %>%
                 dplyr::pull(),
               0)
})



test_that("specify_MR_Y_U_bta works", {

  A_B_path <- file.path("../../inst/A_B_data_full_2018_format.csv")

  AB_data <- A_B_path %>%
    IEATools::load_tidy_iea_df() %>%
    specify_all_revisited()

  MR_Y_U_bta <- AB_data %>%
    add_psut_matnames_epsilon() %>%
    specify_MR_Y_U_bta()

  # In theory, we should get exactly the same outcome as when running the gma assumption here.
  MR_Y_U_gma <- AB_data %>%
    add_psut_matnames_epsilon() %>%
    specify_MR_Y_U_gma()

  # Nice. Same data frames.
  expect_equal(MR_Y_U_bta, MR_Y_U_gma)


  # Now, let's see if we feed a particular trade matrix!
  # Now, say that we have a gap - country B provenience isn't in there!

  dummy_AB_trade_matrix <- AB_data %>%
    add_psut_matnames_epsilon() %>%
    calc_bilateral_trade_matrix_df_gma() %>%
    dplyr::filter(Provenience != "B")

  MR_Y_U_bta <- AB_data %>%
    add_psut_matnames_epsilon() %>%
    specify_MR_Y_U_bta(bilateral_trade_matrix_df = dummy_AB_trade_matrix)

  # Nice, again same data frames.
  expect_equal(MR_Y_U_bta %>%
                 dplyr::arrange(Country, Method, Energy.type, Last.stage, Year, Ledger.side, Flow.aggregation.point, Flow, Product, Unit, matnames),
               MR_Y_U_gma %>%
                 dplyr::arrange(Country, Method, Energy.type, Last.stage, Year, Ledger.side, Flow.aggregation.point, Flow, Product, Unit, matnames))


  # Now, say we modify the bilateral trade matrix, so that everything comes from country A!

  second_dummy_AB_trade_matrix <- AB_data %>%
    add_psut_matnames_epsilon() %>%
    calc_bilateral_trade_matrix_df_gma() %>%
    dplyr::filter(Provenience != "B") %>%
    tibble::add_row(
      Provenience = "A",
      Country = "A",
      Method = "PCM",
      Energy.type = "E",
      Last.stage = "Final",
      Year = 2018,
      Product = "Coke oven coke",
      Share_Exports_From_Func = 1
    )

  MR_Y_U_bta <- AB_data %>%
    add_psut_matnames_epsilon() %>%
    specify_MR_Y_U_bta(bilateral_trade_matrix_df = second_dummy_AB_trade_matrix)

  # Checking data frames are NOT the same
  expect_true(FALSE %in% (MR_Y_U_bta == MR_Y_U_gma))

  # Further, checking particular values
  expect_equal(MR_Y_U_bta %>%
                 dplyr::filter(
                   Flow == "{A}_Residential",
                   Product == "{A}_Coke oven coke"
                 ) %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               c(48, 72))

  expect_equal(MR_Y_U_bta %>%
                 dplyr::filter(
                   Flow == "{A}_Road",
                   Product == "{A}_Coke oven coke"
                 ) %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               c(32, 48))

  expect_equal(MR_Y_U_bta %>%
                 dplyr::filter(
                   Flow == "{A}_Blast furnaces",
                   Product == "{A}_Coke oven coke"
                 ) %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               c(-320, -480))
})



# test_that("transform_to_bta works", {
#
# })

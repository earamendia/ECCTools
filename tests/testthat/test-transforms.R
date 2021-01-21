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

  AB_data <- A_B_path %>%
    IEATools::load_tidy_iea_df() %>%
    specify_all_revisited()

  AB_supply_MR <- AB_data %>%
    add_psut_matnames_epsilon() %>%
    specify_MR_V()


  expect_equal(dim(AB_supply_MR),
               c(18, 12))


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
})


# Specifying U and Y matrices
# test_that("specify_MR_Y_U_gma works", {
#
#
#
#
#
# })




# Whole transform_to_gma function works
# test_that("transform_to_gma works"{
#
# })



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
#   relocate(Origin, .after = Product) %>%
#   filter(E.dot != 0) %>%
#   print()


# Creating trade matrices data frame
# total_exports <- AB_tidy_data %>%
#   filter(str_detect(Flow, "Exports")) %>%
#   mutate(
#     E.dot = abs(E.dot)
#   ) %>%
#   group_by(Method, Energy.type, Last.stage, Year, Ledger.side, Flow.aggregation.point, Product) %>%
#   summarise(
#     Total_Exports = sum(E.dot)
#   ) %>%
#   print()


# This is not yet a matrix....
# share_exports <- AB_tidy_data %>%
#   filter(str_detect(Flow, "Exports")) %>%
#   mutate(
#     E.dot = abs(E.dot)
#   ) %>%
#   left_join(total_exports, by = c("Method", "Energy.type", "Last.stage", "Year", "Ledger.side", "Flow.aggregation.point", "Product")) %>%
#   mutate(
#     Share_Exports = E.dot / Total_Exports
#   ) %>%
#   rename(Provenience = Country) %>%
#   print()


# So here we create a data frame equivalent to a matrix with n*n countries. Also A to A etc. Missing values are implicitly 0 shares.
# share_exports_by_origin_destination <- share_exports %>%
#   expand_grid(Country = AB_tidy_data %>%
#                 expand(Country) %>%
#                 pull()) %>%
#   mutate(
#     Share_Exports = case_when(
#       Provenience == Country ~ 0,
#       TRUE ~ Share_Exports
#     )
#   ) %>%
#   relocate(Country, .after = Provenience) %>%
#   select(- Ledger.side, -Flow.aggregation.point, -Flow, -Unit, -E.dot, -matnames, -Total_Exports) %>%
#   print()
#
#
# testing <- share_exports_by_origin_destination %>%
#   add_row(Provenience = "C",
#           Country = "B",
#           Method = "PCM",
#           Energy.type = "E",
#           Last.stage = "Final",
#           Year = 2018,
#           Product = "Natural gas [of Oil and gas extraction]",
#           Share_Exports = 0.5) %>%
#   print()

# Testing the trade matrix

# share_exports_by_origin_destination %>%
#   group_by(Country, Method, Energy.type, Last.stage, Year, Product) %>%
#   summarise(sum_shares = sum(Share_Exports)) %>%
#   print()
#
#
#
# test_that("calc_imports_by_product works"{
#
#
# })
#
#
# test_that("calc_share_imports_by_products works"{
#
#
# })







# Testing Bilateral Trade Assumption functions





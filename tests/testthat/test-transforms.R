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



# Helper functions
# Specify all etc.






# Testing Bilateral Trade Assumption functions





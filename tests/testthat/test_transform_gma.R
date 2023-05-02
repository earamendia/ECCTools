###########################################################
context("Transforming GMA")
###########################################################

# Testing Global Market Assumption functions

# Specifying R matrix
test_that("specify_MR_R works", {

  A_B_path <- system.file("extdata/A_B_data_full_2018_format.csv", package = "ECCTools")

  AB_data <- A_B_path %>%
    IEATools::load_tidy_iea_df(unit_val = "ktoe") %>%
    IEATools::specify_all()

  AB_resources_MR <- AB_data %>%
    IEATools::add_psut_matnames(R_includes_all_exogenous_flows = FALSE) %>%
    specify_MR_R()


  expect_equal(dim(AB_resources_MR)[1], 3)

  expect_equal(AB_resources_MR %>%
                 dplyr::filter(Product == "{A}_Coking coal [from Resources]") %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               5000)

  expect_equal(AB_resources_MR %>%
                 dplyr::filter(Product == "{A}_Crude oil [from Resources]") %>%
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

  A_B_path <- system.file("extdata/A_B_data_full_2018_format.csv", package = "ECCTools")

  # We wheck what happens when we add an balancing flow to the supply side.
  AB_data <- A_B_path %>%
    IEATools::load_tidy_iea_df(unit_val = "ktoe") %>%
    IEATools::specify_all() %>%
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
    IEATools::add_psut_matnames(R_includes_all_exogenous_flows = FALSE) %>%
    stock_changes_to_balancing() %>%
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


test_that("specify_imported_products works", {

  A_B_path <- system.file("extdata/A_B_data_full_2018_format.csv", package = "ECCTools")

  AB_data <- A_B_path %>%
    IEATools::load_tidy_iea_df(unit_val = "ktoe") %>%
    IEATools::specify_all()

  defined_imported_products <- AB_data %>%
    IEATools::add_psut_matnames(R_includes_all_exogenous_flows = FALSE) %>%
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
               c(76, 13))

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
                   Product == "Natural gas",
                   Origin == "Imported"
                 ) %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               -150)


  # We add a stock changes flow that will go to the Balancing matrix for a more comprehensive test.
  AB_data <- A_B_path %>%
    IEATools::load_tidy_iea_df(unit_val = "ktoe") %>%
    IEATools::specify_all() %>%
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
    IEATools::add_psut_matnames(R_includes_all_exogenous_flows = FALSE) %>%
    stock_changes_to_balancing() %>%
    specify_imported_products()

  # Checks.
  expect_equal(defined_imported_products %>%
                 dplyr::filter(matnames == "B") %>%
                 dplyr::select(Country) %>%
                 dplyr::pull() %>%
                 length(),
               2)

  expect_equal(defined_imported_products %>%
                 dplyr::filter(matnames == "B",
                               Product == "Coke oven coke",
                               Origin == "Domestic") %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               45.45455,
               tolerance = 0.0001)

  expect_equal(defined_imported_products %>%
                 dplyr::filter(matnames == "B",
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


test_that("specify_MR_Y_U_gma works", {

  A_B_path <- system.file("extdata/A_B_data_full_2018_format.csv", package = "ECCTools")

  AB_data <- A_B_path %>%
    IEATools::load_tidy_iea_df(unit_val = "ktoe") %>%
    IEATools::specify_all()

  AB_MR_Y_U_gma <- AB_data %>%
    IEATools::add_psut_matnames(R_includes_all_exogenous_flows = FALSE) %>%
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
               c(76, 12))

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
                   Product == "{A}_Natural gas"
                 ) %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               -150)

  expect_equal(AB_MR_Y_U_gma %>%
                 dplyr::filter(
                   Flow == "{B}_Road",
                   Product == "{A}_Natural gas"
                 ) %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               180)

  # We add a stock changes flow that will go to the Balancing matrix for a more comprehensive test.
  AB_data <- A_B_path %>%
    IEATools::load_tidy_iea_df(unit_val = "ktoe") %>%
    IEATools::specify_all() %>%
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
    IEATools::add_psut_matnames(R_includes_all_exogenous_flows = FALSE) %>%
    stock_changes_to_balancing() %>%
    specify_MR_Y_U_gma()

  # Checks.
  expect_equal(AB_MR_Y_U_gma %>%
                 dplyr::filter(matnames == "B") %>%
                 dplyr::select(Country) %>%
                 dplyr::pull() %>%
                 length(),
               2)

  expect_equal(AB_MR_Y_U_gma %>%
                 dplyr::filter(matnames == "B",
                               Flow == "{A}_Stock changes [of Coke oven coke]",
                               Product == "{A}_Coke oven coke") %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               45.45455,
               tolerance = 0.0001)

  expect_equal(AB_MR_Y_U_gma %>%
                 dplyr::filter(matnames == "B",
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
  A_B_path <- system.file("extdata/A_B_data_full_2018_format.csv", package = "ECCTools")

  AB_data <- A_B_path %>%
    IEATools::load_tidy_iea_df(unit_val = "ktoe") %>%
    IEATools::specify_all()


  AB_MR_PSUT_gma <- AB_data %>%
    IEATools::add_psut_matnames(R_includes_all_exogenous_flows = FALSE) %>%
    transform_to_gma()


  expect_equal(dim(AB_MR_PSUT_gma),
               c(97, 12))


  # Checking R matrix flows
  expect_equal(AB_MR_PSUT_gma %>%
                 dplyr::filter(Product == "{A}_Coking coal [from Resources]",
                               matnames == "R") %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               5000)

  expect_equal(AB_MR_PSUT_gma %>%
                 dplyr::filter(Product == "{A}_Crude oil [from Resources]",
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
                   Product == "{A}_Natural gas",
                   matnames == "U_EIOU"
                 ) %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               -150)

  expect_equal(AB_MR_PSUT_gma %>%
                 dplyr::filter(
                   Flow == "{B}_Road",
                   Product == "{A}_Natural gas",
                   matnames == "Y"
                 ) %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               180)


  # Then just check with stock changes and Balancing matrix too.
  AB_data <- A_B_path %>%
    IEATools::load_tidy_iea_df(unit_val = "ktoe") %>%
    IEATools::specify_all() %>%
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
    IEATools::add_psut_matnames(R_includes_all_exogenous_flows = FALSE) %>%
    stock_changes_to_balancing() %>%
    transform_to_gma()

  # Length should be now 97.
  expect_equal(dim(AB_MR_PSUT_gma),
               c(100, 12))


  # Testing a couple of values
  expect_equal(AB_MR_PSUT_gma %>%
                 dplyr::filter(matnames == "B") %>%
                 dplyr::select(Country) %>%
                 dplyr::pull() %>%
                 length(),
               3)

  expect_equal(AB_MR_PSUT_gma %>%
                 dplyr::filter(
                   Flow == "{A}_Stock changes [of a very odd product]",
                   Product == "{A}_a very odd product",
                   matnames == "B"
                 ) %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               -100)

  expect_equal(AB_MR_PSUT_gma %>%
                 dplyr::filter(matnames == "B",
                               Flow == "{A}_Stock changes [of Coke oven coke]",
                               Product == "{A}_Coke oven coke") %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               45.45455,
               tolerance = 0.0001)

  expect_equal(AB_MR_PSUT_gma %>%
                 dplyr::filter(matnames == "B",
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



###########################################################
context("Cleaning up PSUT")
###########################################################

# Testing convert_to_net_trade function:
test_that("convert_to_net_trade works", {

  # Path to dummy AB data
  A_B_path <- system.file("extdata/A_B_data_full_2018_format.csv", package = "ECCTools")

  # Loading AB_data
  AB_data <- A_B_path %>%
    IEATools::load_tidy_iea_df()

  # Adding a couple of rows for having both imports and exports
  dummy_net_trade <- tibble::tribble(
    ~Country, ~Method, ~Energy.type, ~Last.stage, ~Year, ~Ledger.side, ~Flow.aggregation.point, ~Flow, ~Product, ~Unit, ~E.dot,
    "A", "PCM", "E", "Final", 2018, "Supply", "Total primary energy supply", "Imports", "Coking coal", "ktoe", 840,
    "A", "PCM", "E", "Final", 2018, "Supply", "Total primary energy supply", "Imports", "Natural gas", "ktoe", 1100,
    "A", "PCM", "E", "Final", 2018, "Supply", "Total primary energy supply", "Imports", "Crude oil", "ktoe", 4000,
    "B", "PCM", "E", "Final", 2018, "Supply", "Total primary energy supply", "Imports", "Coke oven coke", "ktoe", 600,
    "A", "PCM", "E", "Final", 2018, "Supply", "Total primary energy supply", "Exports", "Coke oven coke", "ktoe", -1400,
  )

  AB_net_trade_testing <- AB_data %>%
    dplyr::bind_rows(dummy_net_trade) %>%
    IEATools::specify_all()


  # First test, that shouldn't change the data frame at all
  test_unchanged_df <- convert_to_net_trade(AB_data %>%
                                              IEATools::specify_all())

  checking_row_equality <- dplyr::left_join(AB_data %>%
                                              IEATools::specify_all(),
                                            test_unchanged_df,
                                     by = c("Country", "Method", "Energy.type", "Last.stage", "Year", "Ledger.side", "Flow.aggregation.point", "Flow", "Product", "Unit")) %>%
    dplyr::mutate(
      diff = E.dot.x - E.dot.y
    ) %>%
    dplyr::summarise(max_diff = max(abs(diff))) %>%
    dplyr::pull()

  expect_equal(checking_row_equality, 0)


  # Second test, that should change the data frame where there are both imports and exports
  test_changed_df <- convert_to_net_trade(AB_net_trade_testing)

  checking_row_equality <- dplyr::left_join(AB_data %>%
                                              IEATools::specify_all(),
                                            test_changed_df,
                                     by = c("Country", "Method", "Energy.type", "Last.stage", "Year", "Ledger.side", "Flow.aggregation.point", "Flow", "Product", "Unit")) %>%
    dplyr::mutate(
      diff = E.dot.x - E.dot.y
    ) %>%
    dplyr::filter(! (stringr::str_detect(Flow, "Imports") | stringr::str_detect(Flow, "Exports"))) %>%
    dplyr::summarise(max_diff = max(abs(diff))) %>%
    dplyr::pull()

  expect_equal(checking_row_equality, 0)


  check_trade <- test_changed_df %>%
    dplyr::filter(stringr::str_detect(Flow, "Imports") | stringr::str_detect(Flow, "Exports"))

  expect_equal(check_trade[[1, "E.dot"]], -800)
  expect_equal(check_trade[[1, "Flow"]], "Exports [of Coke oven coke]")
  expect_equal(check_trade[[2, "E.dot"]], -1160)
  expect_equal(check_trade[[2, "Flow"]], "Exports [of Coking coal]")
  expect_equal(check_trade[[3, "E.dot"]], 500)
  expect_equal(check_trade[[3, "Flow"]], "Imports [of Crude oil]")
  expect_equal(check_trade[[4, "E.dot"]], -1400)
  expect_equal(check_trade[[4, "Flow"]], "Exports [of Natural gas]")
  expect_equal(check_trade[[5, "E.dot"]], 200)
  expect_equal(check_trade[[5, "Flow"]], "Imports [of Coke oven coke]")
  expect_equal(check_trade[[8, "E.dot"]], 2500)
  expect_equal(check_trade[[8, "Flow"]], "Imports [of Natural gas]")
})


# Testing stat_diffs_to_balancing:
test_that("stat_diffs_to_balancing works",{

  A_B_path <- system.file("extdata/A_B_data_full_2018_format_stat_diffs_stock_changes.csv", package = "ECCTools")

  AB_data <- A_B_path %>%
    IEATools::load_tidy_iea_df()

  AB_data_specified <- AB_data %>%
    IEATools::specify_all()

  AB_data_stat_diffs_to_balancing <- AB_data_specified %>%
    IEATools::add_psut_matnames() %>%
    stat_diffs_to_balancing()

  expect_equal(AB_data_stat_diffs_to_balancing[1,] %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               500)

  stat_diffs_flows <- AB_data_stat_diffs_to_balancing %>%
    dplyr::filter(Flow == "Statistical differences")

  expect_equal(stat_diffs_flows[1,] %>%
                 dplyr::select(matnames) %>%
                 dplyr::pull(),
               "B")

  expect_equal(stat_diffs_flows[1,] %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               10)

  expect_equal(stat_diffs_flows[2,] %>%
                 dplyr::select(matnames) %>%
                 dplyr::pull(),
               "B")

  expect_equal(stat_diffs_flows[2,] %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               -10)

})


# Testing stock_changes_to_balancing:
test_that("stock_changes_to_balancing works",{

  A_B_path <- system.file("extdata/A_B_data_full_2018_format_stat_diffs_stock_changes.csv", package = "ECCTools")

  AB_data <- A_B_path %>%
    IEATools::load_tidy_iea_df()

  AB_data_specified <- AB_data %>%
    IEATools::specify_all()

  expect_true(AB_data_specified %>%
                IEATools::tidy_iea_df_balanced())

  AB_data_stock_changes_to_balancing <- AB_data_specified %>%
    IEATools::add_psut_matnames() %>%
    stock_changes_to_balancing()

  expect_true(AB_data_stock_changes_to_balancing %>%
                IEATools::tidy_iea_df_balanced())


  expect_equal(AB_data_stock_changes_to_balancing[1,] %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               500)

  stock_changes_flows <- AB_data_stock_changes_to_balancing %>%
    dplyr::filter(
      stringr::str_detect(.data[["Flow"]], "Stock changes")
      )

  expect_equal(stock_changes_flows[1,] %>%
                 dplyr::select(matnames) %>%
                 dplyr::pull(),
               "B")

  expect_equal(stock_changes_flows[1,] %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               -20)

  expect_equal(stock_changes_flows[2,] %>%
                 dplyr::select(matnames) %>%
                 dplyr::pull(),
               "B")

  expect_equal(stock_changes_flows[2,] %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               10)

  expect_equal(stock_changes_flows[3,] %>%
                 dplyr::select(matnames) %>%
                 dplyr::pull(),
               "B")

  expect_equal(stock_changes_flows[4,] %>%
                 dplyr::select(matnames) %>%
                 dplyr::pull(),
               "B")
})


test_that("Checking the sum_R_V argument of the calc_io_mats works well", {

  A_B_path <- system.file("extdata/A_B_data_full_2018_format_stat_diffs_stock_changes.csv", package = "ECCTools")

  AB_data <- A_B_path %>%
    IEATools::load_tidy_iea_df()

  AB_data_specified <- AB_data %>%
    IEATools::specify_all() %>%
    tibble::add_row(
      Country = c("A", "A", "A"),
      Method = c("PCM", "PCM", "PCM"),
      Energy.type = c("E", "E", "E"),
      Last.stage = c("Final", "Final", "Final"),
      Year = c(2018, 2018, 2018),
      Ledger.side = c("Consumption", "Consumption", "Consumption"),
      Flow.aggregation.point = c("Transport", "Transport", "Transport"),
      Flow = c("Road", "Road", "Road"),
      Product = c("Anthracite", "Hard coal (if no detail)", "Brown coal (if no detail)"),
      Unit = c("ktoe", "ktoe", "ktoe"),
      E.dot = c(-50, -40, -10)
    )

  AB_data_stock_changes_to_balancing <- AB_data_specified %>%
    IEATools::add_psut_matnames() %>%
    stock_changes_to_balancing()

  # Here we add a test: the calc_io_mats() function from Recca must work fine when using the sum_R_V method

  AB_data_specified_io_Y_U_method <- AB_data_specified %>%
    IEATools::prep_psut() %>%
    Recca::calc_io_mats()

  AB_data_specified_io_R_V_method <- AB_data_specified %>%
    IEATools::prep_psut() %>%
    Recca::calc_io_mats(method_q_calculation = "sum_R_V_cols")

  AB_data_stock_changes_to_balancing_io_Y_U_method <- AB_data_stock_changes_to_balancing %>%
    IEATools::prep_psut() %>%
    Recca::calc_io_mats()

  AB_data_stock_changes_to_balancing_io_R_V_method <- AB_data_stock_changes_to_balancing %>%
    IEATools::prep_psut() %>%
    Recca::calc_io_mats(method_q_calculation = "sum_R_V_cols")

  # Without Balancing, sum_Y_U method
  expect_equal(AB_data_specified_io_Y_U_method$q[[1]][["Anthracite", 1]], 50)
  expect_equal(AB_data_specified_io_Y_U_method$q[[1]][["Hard coal (if no detail)", 1]], 40)
  expect_equal(AB_data_specified_io_Y_U_method$q[[1]][["Brown coal (if no detail)", 1]], 30)
  expect_equal(AB_data_specified_io_Y_U_method$q[[1]][["Coking coal", 1]], 5020)

  # Without Balancing, sum_R_V method
  expect_equal(AB_data_specified_io_R_V_method$q[[1]][["Anthracite", 1]], 20)
  expect_equal(AB_data_specified_io_R_V_method$q[[1]][["Hard coal (if no detail)", 1]], 20)
  expect_error(AB_data_specified_io_R_V_method$q[[1]][["Brown coal (if no detail)", 1]])
  expect_equal(AB_data_specified_io_R_V_method$q[[1]][["Coking coal", 1]], 5000)

  # With Balancing, sum_Y_U method
  expect_equal(AB_data_stock_changes_to_balancing_io_Y_U_method$q[[1]][["Anthracite", 1]], 50)
  expect_equal(AB_data_stock_changes_to_balancing_io_Y_U_method$q[[1]][["Hard coal (if no detail)", 1]], 40)
  expect_equal(AB_data_stock_changes_to_balancing_io_Y_U_method$q[[1]][["Brown coal (if no detail)", 1]], 20)
  expect_equal(AB_data_stock_changes_to_balancing_io_Y_U_method$q[[1]][["Coking coal", 1]], 5000)

  # With Balancing, sum_R_V method
  expect_error(AB_data_stock_changes_to_balancing_io_R_V_method$q[[1]][["Anthracite", 1]])
  expect_equal(AB_data_stock_changes_to_balancing_io_R_V_method$q[[1]][["Hard coal (if no detail)", 1]], 10)
  expect_error(AB_data_stock_changes_to_balancing_io_R_V_method$q[[1]][["Brown coal (if no detail)", 1]])
  expect_equal(AB_data_stock_changes_to_balancing_io_R_V_method$q[[1]][["Coking coal", 1]], 5000)
})


test_that("Convert fuel gasoline works", {

  # Path to dummy AB data
  A_B_path <- system.file("extdata/A_B_data_full_2018_format.csv", package = "ECCTools")

  # Loading AB_data
  AB_data <- A_B_path %>%
    IEATools::load_tidy_iea_df() %>%
    tibble::add_row(
      Country = "A", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 2018, Product = "Gasoline type jet fuel", Ledger.side = "Consumption", Flow.aggregation.point = "Industry", Flow = "Iron and steel", Unit = "ktoe", E.dot = 20
    ) %>%
    tibble::add_row(
      Country = "A", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 2018, Product = "Gasoline type jet fuel", Ledger.side = "Supply", Flow.aggregation.point = "Total primary energy supply", Flow = "Exports", Unit = "ktoe", E.dot = 50
    ) %>%
    tibble::add_row(
      Country = "A", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 2018, Product = "Gasoline type jet fuel", Ledger.side = "Supply", Flow.aggregation.point = "Total primary energy supply", Flow = "Imports", Unit = "ktoe", E.dot = 100
    ) %>%
    IEATools::specify_all()


  AB_data %>%
    dplyr::filter(Product == "Gasoline type jet fuel") %>%
    nrow() %>%
    expect_equal(3)

  test_gathering <- AB_data %>%
    convert_fuel_gasoline_into_motor_gasoline()

  test_gathering %>%
    dplyr::filter(Product == "Motor gasoline excl. biofuels", Flow == "Iron and steel", Country == "A") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(520)

  test_gathering %>%
    dplyr::filter(Product == "Motor gasoline excl. biofuels", Flow == "Imports [of Motor gasoline excl. biofuels]", Country == "A") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(100)

  test_gathering %>%
    dplyr::filter(Product == "Motor gasoline excl. biofuels", Flow == "Exports [of Motor gasoline excl. biofuels]", Country == "A") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(50)

  test_gathering %>%
    dplyr::filter(Product == "Gasoline type jet fuel") %>%
    nrow() %>%
    expect_equal(0)
})

###########################################################
context("IEA energy balance")
###########################################################

test_that("calc_tidy_iea_df_balance works correctly for 2018 data", {
  Ebal_2018 <- IEATools::load_tidy_iea_df(IEATools::sample_iea_data_path(2018)) %>%
    IEATools::calc_tidy_iea_df_balances()
  expect_false(all(Ebal_2018$balance_OK))
  expect_true(Ebal_2018 %>% dplyr::filter(Country == "GHA", Year == 1971, Product == "Aviation gasoline") %>% magrittr::extract2("balance_OK"))
  expect_false(Ebal_2018 %>% dplyr::filter(Country == "GHA", Year == 1971, Product == "Electricity") %>% magrittr::extract2("balance_OK"))
  expect_true(Ebal_2018 %>% dplyr::filter(Country == "ZAF", Year == 1971, Product == "Hydro") %>% magrittr::extract2("balance_OK"))
  expect_false(Ebal_2018 %>% dplyr::filter(Country == "ZAF", Year == 2000, Product == "Other bituminous coal") %>% magrittr::extract2("balance_OK"))
})


test_that("calc_tidy_iea_df_balance works correctly for 2019 data", {
  Ebal_2019 <- IEATools::load_tidy_iea_df(IEATools::sample_iea_data_path(2019)) %>%
    IEATools::calc_tidy_iea_df_balances()
  expect_false(all(Ebal_2019$balance_OK))
  expect_true(Ebal_2019 %>% dplyr::filter(Country == "GHA", Year == 1971, Product == "Aviation gasoline") %>% magrittr::extract2("balance_OK"))
  expect_false(Ebal_2019 %>% dplyr::filter(Country == "GHA", Year == 1971, Product == "Electricity") %>% magrittr::extract2("balance_OK"))
  # This one was OK in the 2018 data, but is off by 1e-4 in the 2019 data.
  expect_false(Ebal_2019 %>% dplyr::filter(Country == "ZAF", Year == 1971, Product == "Hydro") %>% magrittr::extract2("balance_OK"))
  expect_false(Ebal_2019 %>% dplyr::filter(Country == "ZAF", Year == 2000, Product == "Other bituminous coal") %>% magrittr::extract2("balance_OK"))
})


test_that("add_balancing_vector works", {

  # Path to dummy AB data
  A_B_path <- system.file("extdata/A_B_data_full_2018_format.csv", package = "ECCTools")

  # Loading AB_data
  AB_data <- A_B_path %>%
    IEATools::load_tidy_iea_df()

  AB_tidy_data <- AB_data %>%
    IEATools::specify_all()

  expect_true(AB_data %>%
                IEATools::tidy_iea_df_balanced())

  AB_tidy_MR <- AB_data %>%
    IEATools::add_psut_matnames() %>%
    transform_to_gma()

  expect_false(AB_tidy_MR %>%
                 IEATools::calc_tidy_iea_df_balances() %>%
                 magrittr::extract2("balance_OK") %>%
                 all())

  AB_tidy_MR_with_balancing <- AB_tidy_MR %>%
    add_balancing_vector()

  expect_true(AB_tidy_MR_with_balancing %>%
                IEATools::calc_tidy_iea_df_balances() %>%
                magrittr::extract2("balance_OK") %>%
                all())
})


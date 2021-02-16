###########################################################
context("Cleaning up PSUT")
###########################################################

# Testing convert_to_net_trade function:
test_that("convert_to_net_trade works", {

  # Path to dummy AB data
  A_B_path <- system.file("A_B_data_full_2018_format.csv", package = "ECCTools")

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


# Testing stat_diffs_to_epsilon:
test_that("stat_diffs_to_epsilon works",{

  A_B_path <- system.file("A_B_data_full_2018_format_stat_diffs_stock_changes.csv", package = "ECCTools")

  AB_data <- A_B_path %>%
    IEATools::load_tidy_iea_df()

  AB_data_specified <- AB_data %>%
    IEATools::specify_all()

  AB_data_stat_diffs_to_epsilon <- AB_data_specified %>%
    add_psut_matnames_epsilon() %>%
    stat_diffs_to_epsilon()

  expect_equal(AB_data_stat_diffs_to_epsilon[1,] %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               500)

  stat_diffs_flows <- AB_data_stat_diffs_to_epsilon %>%
    dplyr::filter(Flow == "Statistical differences")

  expect_equal(stat_diffs_flows[1,] %>%
                 dplyr::select(matnames) %>%
                 dplyr::pull(),
               "Epsilon")

  expect_equal(stat_diffs_flows[1,] %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               10)

  expect_equal(stat_diffs_flows[2,] %>%
                 dplyr::select(matnames) %>%
                 dplyr::pull(),
               "Epsilon")

  expect_equal(stat_diffs_flows[2,] %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               -10)

})


# Testing stock_changes_to_epsilon:
test_that("stock_changes_to_epsilon works",{

  A_B_path <- system.file("A_B_data_full_2018_format_stat_diffs_stock_changes.csv", package = "ECCTools")

  AB_data <- A_B_path %>%
    IEATools::load_tidy_iea_df()

  AB_data_specified <- AB_data %>%
    IEATools::specify_all()

  expect_true(AB_data_specified %>%
                IEATools::tidy_iea_df_balanced())

  AB_data_stock_changes_to_epsilon <- AB_data_specified %>%
    IEATools::add_psut_matnames() %>%
    stock_changes_to_epsilon()

  expect_true(AB_data_stock_changes_to_epsilon %>%
                IEATools::tidy_iea_df_balanced())


  expect_equal(AB_data_stock_changes_to_epsilon[1,] %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               500)

  stock_changes_flows <- AB_data_stock_changes_to_epsilon %>%
    dplyr::filter(
      stringr::str_detect(.data[["Flow"]], "Stock changes")
      )

  expect_equal(stock_changes_flows[1,] %>%
                 dplyr::select(matnames) %>%
                 dplyr::pull(),
               "Epsilon")

  expect_equal(stock_changes_flows[1,] %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               -20)

  expect_equal(stock_changes_flows[2,] %>%
                 dplyr::select(matnames) %>%
                 dplyr::pull(),
               "Epsilon")

  expect_equal(stock_changes_flows[2,] %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               10)

  expect_equal(stock_changes_flows[3,] %>%
                 dplyr::select(matnames) %>%
                 dplyr::pull(),
               "Epsilon")

  expect_equal(stock_changes_flows[4,] %>%
                 dplyr::select(matnames) %>%
                 dplyr::pull(),
               "Epsilon")
})


test_that("Checking the sum_R_V argument of the calc_io_mats works well", {

  A_B_path <- system.file("A_B_data_full_2018_format_stat_diffs_stock_changes.csv", package = "ECCTools")

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

  AB_data_stock_changes_to_epsilon <- AB_data_specified %>%
    IEATools::add_psut_matnames() %>%
    stock_changes_to_epsilon()

  # Here we add a test: the calc_io_mats() function from Recca must work fine when using the sum_R_V method

  AB_data_specified_io_Y_U_method <- AB_data_specified %>%
    IEATools::prep_psut() %>%
    Recca::calc_io_mats()

  AB_data_specified_io_R_V_method <- AB_data_specified %>%
    IEATools::prep_psut() %>%
    Recca::calc_io_mats(method_q_calculation = "sum_R_V_cols")

  AB_data_stock_changes_to_epsilon_io_Y_U_method <- AB_data_stock_changes_to_epsilon %>%
    IEATools::prep_psut() %>%
    Recca::calc_io_mats()

  AB_data_stock_changes_to_epsilon_io_R_V_method <- AB_data_stock_changes_to_epsilon %>%
    IEATools::prep_psut() %>%
    Recca::calc_io_mats(method_q_calculation = "sum_R_V_cols")

  # Without Epsilon, sum_Y_U method
  expect_equal(AB_data_specified_io_Y_U_method$q[[1]][["Anthracite", 1]], 50)
  expect_equal(AB_data_specified_io_Y_U_method$q[[1]][["Hard coal (if no detail)", 1]], 40)
  expect_equal(AB_data_specified_io_Y_U_method$q[[1]][["Brown coal (if no detail)", 1]], 30)
  expect_equal(AB_data_specified_io_Y_U_method$q[[1]][["Coking coal", 1]], 5020)

  # Without Epsilon, sum_R_V method
  expect_equal(AB_data_specified_io_R_V_method$q[[1]][["Anthracite", 1]], 20)
  expect_equal(AB_data_specified_io_R_V_method$q[[1]][["Hard coal (if no detail)", 1]], 20)
  expect_error(AB_data_specified_io_R_V_method$q[[1]][["Brown coal (if no detail)", 1]])
  expect_equal(AB_data_specified_io_R_V_method$q[[1]][["Coking coal", 1]], 5000)

  # With Epsilon, sum_Y_U method
  expect_equal(AB_data_stock_changes_to_epsilon_io_Y_U_method$q[[1]][["Anthracite", 1]], 50)
  expect_equal(AB_data_stock_changes_to_epsilon_io_Y_U_method$q[[1]][["Hard coal (if no detail)", 1]], 40)
  expect_equal(AB_data_stock_changes_to_epsilon_io_Y_U_method$q[[1]][["Brown coal (if no detail)", 1]], 20)
  expect_equal(AB_data_stock_changes_to_epsilon_io_Y_U_method$q[[1]][["Coking coal", 1]], 5000)

  # With Epsilon, sum_R_V method
  expect_error(AB_data_stock_changes_to_epsilon_io_R_V_method$q[[1]][["Anthracite", 1]])
  expect_equal(AB_data_stock_changes_to_epsilon_io_R_V_method$q[[1]][["Hard coal (if no detail)", 1]], 10)
  expect_error(AB_data_stock_changes_to_epsilon_io_R_V_method$q[[1]][["Brown coal (if no detail)", 1]])
  expect_equal(AB_data_stock_changes_to_epsilon_io_R_V_method$q[[1]][["Coking coal", 1]], 5000)
})



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
    specify_all_revisited()


  # First test, that shouldn't change the data frame at all
  test_unchanged_df <- convert_to_net_trade(AB_data %>%
                                              specify_all_revisited())

  checking_row_equality <- dplyr::left_join(AB_data %>%
                                              specify_all_revisited(),
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

  checking_row_equality <- dplyr::left_join(AB_data, test_changed_df,
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
  expect_equal(check_trade[[2, "Flow"]], "Exports")
  expect_equal(check_trade[[3, "E.dot"]], -1400)
  expect_equal(check_trade[[4, "E.dot"]], 500)
  expect_equal(check_trade[[4, "Flow"]], "Imports")
  expect_equal(check_trade[[8, "E.dot"]], 200)
  expect_equal(check_trade[[8, "Flow"]], "Imports")
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

  AB_data_stock_changes_to_epsilon <- AB_data_specified %>%
    add_psut_matnames_epsilon() %>%
    stock_changes_to_epsilon()


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


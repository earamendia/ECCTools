###########################################################
context("Cleaning up PSUT")
###########################################################

test_that("convert_to_net_trade works", {

  # Path to dummy AB data
  A_B_path <- file.path("../../inst/A_B_data_full_2018_format.csv")

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
    dplyr::bind_rows(dummy_net_trade)


  # First test, that shouldn't change the data frame at all
  test_unchanged_df <- convert_to_net_trade(AB_data)

  checking_row_equality <- dplyr::left_join(AB_data, test_unchanged_df,
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
    dplyr::filter(! Flow %in% c("Imports", "Exports")) %>%
    dplyr::summarise(max_diff = max(abs(diff))) %>%
    dplyr::pull()

  expect_equal(checking_row_equality, 0)


  check_trade <- test_changed_df %>%
    dplyr::filter(Flow %in% c("Imports", "Exports"))

  expect_equal(check_trade[[1, "E.dot"]], -800)
  expect_equal(check_trade[[1, "Flow"]], "Exports")
  expect_equal(check_trade[[2, "E.dot"]], -1160)
  expect_equal(check_trade[[2, "Flow"]], "Exports")
  expect_equal(check_trade[[3, "E.dot"]], -1400)
  expect_equal(check_trade[[4, "E.dot"]], 500)
  expect_equal(check_trade[[4, "Flow"]], "Imports")
  expect_equal(check_trade[[8, "E.dot"]], 200)
  expect_equal(check_trade[[8, "Flow"]], "Imports")
})


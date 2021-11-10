###########################################################
context("Testing elect/heat flows")
###########################################################

test_that("specify_elect_heat_renewables works",{

  # Path to dummy AB data
  A_B_path <- system.file("extdata/A_B_data_full_2018_format.csv", package = "ECCTools")

  # Loading AB_data
  AB_data <- A_B_path %>%
    IEATools::load_tidy_iea_df()

  # Specifying AB data
  tidy_AB_data <- AB_data %>%
    IEATools::specify_all()

  # The code should NOT change the default data frame
  res <- tidy_AB_data %>%
    specify_elect_heat_renewables()
  expect_true(all(res == tidy_AB_data))


  # Now, preparing proper test.
  tidy_AB_data_with_adjusted_flows <- tidy_AB_data %>%
    tibble::add_row(
      Country = "A", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 2018, Ledger.side = "Supply", Flow.aggregation.point = "Energy industry own use",
      Flow = "Main activity producer electricity plants", Product = "Natural gas", Unit = "ktoe", E.dot = -200
    ) %>%
    tibble::add_row(
      Country = "A", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 2018, Ledger.side = "Supply", Unit = "ktoe",
      Flow.aggregation.point = "Transformation processes", Flow = "Main activity producer electricity plants", Product = "Hydro", E.dot = -15
    ) %>%
    tibble::add_row(
      Country = "A", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 2018, Ledger.side = "Supply", Unit = "ktoe",
      Flow.aggregation.point = "Transformation processes", Flow = "Main activity producer electricity plants", Product = "Solar photovoltaics", E.dot = -10
    ) %>%
    tibble::add_row(
      Country = "A", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 2018, Ledger.side = "Supply", Unit = "ktoe",
      Flow.aggregation.point = "Transformation processes", Flow = "Main activity producer electricity plants", Product = "Solar thermal", E.dot = -25
    ) %>%
    tibble::add_row(
      Country = "A", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 2018, Ledger.side = "Supply", Unit = "ktoe",
      Flow.aggregation.point = "Transformation processes", Flow = "Main activity producer electricity plants", Product = "Geothermal", E.dot = -30
    ) %>%
    tibble::add_row(
      Country = "A", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 2018, Ledger.side = "Supply", Unit = "ktoe",
      Flow.aggregation.point = "Transformation processes", Flow = "Main activity producer electricity plants", Product = "Wind", E.dot = -5
    ) %>%
    tibble::add_row(
      Country = "A", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 2018, Ledger.side = "Supply", Unit = "ktoe",
      Flow.aggregation.point = "Transformation processes", Flow = "Main activity producer electricity plants", Product = "Tide, wave and ocean", E.dot = -2
    )

  res2 <- tidy_AB_data_with_adjusted_flows %>%
    specify_elect_heat_renewables()

  # Testing series:
  # First, doing Main activity producer electricity plants flows
  res2 %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Transformation processes", Flow == "Main activity producer electricity plants", Product == "Electricity") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(3156.75)

  res2 %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Transformation processes", Flow == "Renewable energy plants", Product == "Electricity") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(43.25)

  res2 %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Energy industry own use", Flow == "Renewable energy plants", Product == "Electricity") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(-0.8109375)

  res2 %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Energy industry own use", Flow == "Renewable energy plants", Product == "Natural gas") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(-2.703125)

  res2 %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Energy industry own use", Flow == "Main activity producer electricity plants", Product == "Electricity") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(-60+0.8109375)

  res2 %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Energy industry own use", Flow == "Main activity producer electricity plants", Product == "Natural gas") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(-200+2.703125)

  res2 %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Transformation processes", Flow == "Renewable energy plants", Product == "Hydro") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(-15)

  res2 %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Transformation processes", Flow == "Renewable energy plants", Product == "Solar photovoltaics") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(-10)

  res2 %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Transformation processes", Flow == "Renewable energy plants", Product == "Solar thermal") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(-25)

  res2 %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Transformation processes", Flow == "Renewable energy plants", Product == "Geothermal") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(-30)

  res2 %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Transformation processes", Flow == "Renewable energy plants", Product == "Wind") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(-5)

  res2 %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Transformation processes", Flow == "Renewable energy plants", Product == "Tide, wave and ocean") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(-2)

  # Second, main activity producer CHP plants flows

  # Third, main activity producer heat plants flows

})



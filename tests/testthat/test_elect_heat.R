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


  a <- tidy_AB_data %>%
    dplyr::left_join(res, by = c("Country", "Method", "Energy.type", "Last.stage", "Ledger.side", "Flow.aggregation.point", "Flow", "Product", "Unit")) %>%
    dplyr::mutate(
      err = abs(E.dot.x - E.dot.y)
    )

  expect_true(all(a$err == 0))


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
    ) %>%
    # Now, electricity plants:
    tibble::add_row(
      Country = "A", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 2018, Ledger.side = "Supply", Unit = "ktoe",
      Flow.aggregation.point = "Transformation processes", Flow = "Main activity producer heat plants", Product = "Natural gas", E.dot = -200
    ) %>%
    tibble::add_row(
      Country = "A", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 2018, Ledger.side = "Supply", Unit = "ktoe",
      Flow.aggregation.point = "Transformation processes", Flow = "Main activity producer heat plants", Product = "Heat", E.dot = 150
    ) %>%
    tibble::add_row(
      Country = "A", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 2018, Ledger.side = "Supply", Unit = "ktoe",
      Flow.aggregation.point = "Transformation processes", Flow = "Main activity producer heat plants", Product = "Solar thermal", E.dot = -10
    ) %>%
    tibble::add_row(
      Country = "A", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 2018, Ledger.side = "Supply", Unit = "ktoe",
      Flow.aggregation.point = "Transformation processes", Flow = "Main activity producer heat plants", Product = "Geothermal", E.dot = -15
    ) %>%
    tibble::add_row(
      Country = "A", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 2018, Ledger.side = "Supply", Unit = "ktoe",
      Flow.aggregation.point = "Energy industry own use", Flow = "Main activity producer heat plants", Product = "Electricity", E.dot = -50
    ) %>%
    tibble::add_row(
      Country = "A", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 2018, Ledger.side = "Supply", Unit = "ktoe",
      Flow.aggregation.point = "Energy industry own use", Flow = "Main activity producer heat plants", Product = "Natural gas", E.dot = -20
    ) %>%
    # Now, CHP plants:
    tibble::add_row(
      Country = "A", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 2018, Ledger.side = "Supply", Unit = "ktoe",
      Flow.aggregation.point = "Transformation processes", Flow = "Main activity producer CHP plants", Product = "Natural gas", E.dot = -200
    ) %>%
    tibble::add_row(
      Country = "A", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 2018, Ledger.side = "Supply", Unit = "ktoe",
      Flow.aggregation.point = "Transformation processes", Flow = "Main activity producer CHP plants", Product = "Electricity", E.dot = 100
    ) %>%
    tibble::add_row(
      Country = "A", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 2018, Ledger.side = "Supply", Unit = "ktoe",
      Flow.aggregation.point = "Transformation processes", Flow = "Main activity producer CHP plants", Product = "Heat", E.dot = 75
    ) %>%
    tibble::add_row(
      Country = "A", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 2018, Ledger.side = "Supply", Unit = "ktoe",
      Flow.aggregation.point = "Transformation processes", Flow = "Main activity producer CHP plants", Product = "Geothermal", E.dot = -50
    ) %>%
    tibble::add_row(
      Country = "A", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 2018, Ledger.side = "Supply", Unit = "ktoe",
      Flow.aggregation.point = "Energy industry own use", Flow = "Main activity producer CHP plants", Product = "Blast furnace gas", E.dot = -150
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
    dplyr::filter(Country == "A", Flow.aggregation.point == "Energy industry own use", Flow == "Main activity producer electricity plants", Product == "Electricity") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(-60+0.8109375)

  res2 %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Energy industry own use", Flow == "Main activity producer electricity plants", Product == "Natural gas") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(-200+2.703125)


  # Second, main activity producer CHP plants flows
  res2 %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Transformation processes", Flow == "Main activity producer CHP plants", Product == "Electricity") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(100-2.857143)

  res2 %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Transformation processes", Flow == "Main activity producer CHP plants", Product == "Heat") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(75-10.71429, tolerance = 1e-5)

  res2 %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Energy industry own use", Flow == "Main activity producer CHP plants", Product == "Blast furnace gas") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(-150+11.63266, tol = 1e-5)


  # Third, main activity producer heat plants flows
  res2 %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Transformation processes", Flow == "Main activity producer heat plants", Product == "Heat") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(150-17.5)

  res2 %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Energy industry own use", Flow == "Main activity producer heat plants", Product == "Electricity") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(-50+5.83333, tol = 1e-5)

  res2 %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Energy industry own use", Flow == "Main activity producer heat plants", Product == "Natural gas") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(-20+2.3333, tol = 1e-5)


  # Last, doing these new Renewable energy plants flows:
  res2 %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Transformation processes", Flow == "Renewable energy plants", Product == "Electricity [from Renewables]") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(43.25+2.857143)

  res2 %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Transformation processes", Flow == "Renewable energy plants", Product == "Heat [from Renewables]") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(17.5+10.71429, tol = 1e-5)

  res2 %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Energy industry own use", Flow == "Renewable energy plants", Product == "Electricity") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(-0.8109375-5.83333, tol = 1e-5)

  res2 %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Energy industry own use", Flow == "Renewable energy plants", Product == "Natural gas") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(-2.703125-2.33333, tol = 1e-5)

  res2 %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Energy industry own use", Flow == "Renewable energy plants", Product == "Blast furnace gas") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(-11.63266, tol = 1e-5)

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
    expect_equal(-35)

  res2 %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Transformation processes", Flow == "Renewable energy plants", Product == "Geothermal") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(-95)

  res2 %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Transformation processes", Flow == "Renewable energy plants", Product == "Wind") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(-5)

  res2 %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Transformation processes", Flow == "Renewable energy plants", Product == "Tide, wave and ocean") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(-2)
})


test_that("specify_elect_heat_fossil_fuels function works", {

  # Path to dummy AB data
  A_B_path <- system.file("extdata/A_B_data_full_2018_format.csv", package = "ECCTools")

  # Loading AB_data
  AB_data <- A_B_path %>%
    IEATools::load_tidy_iea_df()

  # Specifying AB data
  tidy_AB_data <- AB_data %>%
    IEATools::specify_all()

  tidy_AB_data_specified <- tidy_AB_data %>%
    tibble::add_row(
      Country = "A", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 2018, Ledger.side = "Supply", Unit = "ktoe",
      Flow.aggregation.point = "Transformation processes", Flow = "Main activity producer electricity plants", Product = "Industrial waste", E.dot = -1000
    ) %>%
    tibble::add_row(
      Country = "A", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 2018, Ledger.side = "Supply", Unit = "ktoe",
      Flow.aggregation.point = "Transformation processes", Flow = "Main activity producer heat plants", Product = "Heat", E.dot = 1000
    ) %>%
    tibble::add_row(
      Country = "A", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 2018, Ledger.side = "Supply", Unit = "ktoe",
      Flow.aggregation.point = "Transformation processes", Flow = "Main activity producer heat plants", Product = "Natural gas", E.dot = -200
    ) %>%
    tibble::add_row(
      Country = "A", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 2018, Ledger.side = "Supply", Unit = "ktoe",
      Flow.aggregation.point = "Transformation processes", Flow = "Main activity producer heat plants", Product = "Fuel oil", E.dot = -300
    ) %>%
    tibble::add_row(
      Country = "A", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 2018, Ledger.side = "Supply", Unit = "ktoe",
      Flow.aggregation.point = "Transformation processes", Flow = "Main activity producer heat plants", Product = "Industrial waste", E.dot = -15
    ) %>%
    tibble::add_row(
      Country = "A", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 2018, Ledger.side = "Supply", Unit = "ktoe",
      Flow.aggregation.point = "Energy industry own use", Flow = "Main activity producer heat plants", Product = "Natural gas", E.dot = -100
    ) %>%
    tibble::add_row(
      Country = "A", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 2018, Ledger.side = "Supply", Unit = "ktoe",
      Flow.aggregation.point = "Transformation processes", Flow = "Main activity producer CHP plants", Product = "Heat", E.dot = 1000
    ) %>%
    tibble::add_row(
      Country = "A", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 2018, Ledger.side = "Supply", Unit = "ktoe",
      Flow.aggregation.point = "Transformation processes", Flow = "Main activity producer CHP plants", Product = "Electricity", E.dot = 500
    ) %>%
    tibble::add_row(
      Country = "A", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 2018, Ledger.side = "Supply", Unit = "ktoe",
      Flow.aggregation.point = "Transformation processes", Flow = "Main activity producer CHP plants", Product = "Natural gas", E.dot = -200
    ) %>%
    tibble::add_row(
      Country = "A", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 2018, Ledger.side = "Supply", Unit = "ktoe",
      Flow.aggregation.point = "Transformation processes", Flow = "Main activity producer CHP plants", Product = "Fuel oil", E.dot = -300
    ) %>%
    tibble::add_row(
      Country = "A", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 2018, Ledger.side = "Supply", Unit = "ktoe",
      Flow.aggregation.point = "Transformation processes", Flow = "Main activity producer CHP plants", Product = "Industrial waste", E.dot = -15
    ) %>%
    tibble::add_row(
      Country = "A", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 2018, Ledger.side = "Supply", Unit = "ktoe",
      Flow.aggregation.point = "Energy industry own use", Flow = "Main activity producer CHP plants", Product = "Natural gas", E.dot = -100
    )



  # does it run?
  res <- tidy_AB_data_specified %>%
    specify_elect_heat_fossil_fuels()


  # Checking flows are doing what they should do:
  # Doing Main activity producer electricity plants:
  res %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Transformation processes", Flow == "Main activity producer electricity plants [from Other products]", Product == "Industrial waste") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(-1000)

  res %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Transformation processes", Flow == "Main activity producer electricity plants [from Other products]", Product == "Electricity [from Other products]") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(744.1859, tol = 1e-3)

  res %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Transformation processes", Flow == "Main activity producer electricity plants [from Other products]", Product == "Heat [from Other products]") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(46.51162, tol = 1e-3)

  res %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Energy industry own use", Flow == "Main activity producer electricity plants [from Other products]", Product == "Electricity") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(-13.95349, tol = 1e-5)

  res %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Transformation processes", Flow == "Main activity producer electricity plants [from Coal products]", Product == "Coking coal") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(-2600)

  res %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Transformation processes", Flow == "Main activity producer electricity plants [from Coal products]", Product == "Electricity [from Coal products]") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(2009.302, tol = 1e-5)

  res %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Transformation processes", Flow == "Main activity producer electricity plants [from Coal products]", Product == "Heat [from Coal products]") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(125.5814, tol = 1e-5)

  res %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Energy industry own use", Flow == "Main activity producer electricity plants [from Coal products]", Product == "Electricity") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(-37.67442, tol = 1e-5)

  res %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Transformation processes", Flow == "Main activity producer electricity plants [from Natural gas]", Product == "Natural gas") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(-600)

  res %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Transformation processes", Flow == "Main activity producer electricity plants [from Natural gas]", Product == "Electricity [from Natural gas]") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(446.5117, tol = 1e-5)

  res %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Transformation processes", Flow == "Main activity producer electricity plants [from Natural gas]", Product == "Heat [from Natural gas]") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(27.90698, tol = 1e-5)

  res %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Energy industry own use", Flow == "Main activity producer electricity plants [from Natural gas]", Product == "Electricity") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(-8.372094, tol = 1e-5)


  # Then doing Main activity producer heat plants:
  res %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Transformation processes", Flow == "Main activity producer heat plants [from Other products]", Product == "Industrial waste") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(-15)

  res %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Transformation processes", Flow == "Main activity producer heat plants [from Other products]", Product == "Heat [from Other products]") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(29.12621, tol = 1e-5)

  res %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Energy industry own use", Flow == "Main activity producer heat plants [from Other products]", Product == "Natural gas") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(-2.912621, tol = 1e-5)

  res %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Transformation processes", Flow == "Main activity producer heat plants [from Natural gas]", Product == "Natural gas") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(-200)

  res %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Transformation processes", Flow == "Main activity producer heat plants [from Natural gas]", Product == "Heat [from Natural gas]") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(388.3495, tol = 1e-5)

  res %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Energy industry own use", Flow == "Main activity producer heat plants [from Natural gas]", Product == "Natural gas") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(-38.83495, tol = 1e-5)

  res %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Transformation processes", Flow == "Main activity producer heat plants [from Oil products]", Product == "Fuel oil") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(-300)

  res %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Transformation processes", Flow == "Main activity producer heat plants [from Oil products]", Product == "Heat [from Oil products]") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(582.5243, tol = 1e-5)

  res %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Energy industry own use", Flow == "Main activity producer heat plants [from Oil products]", Product == "Natural gas") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(-58.25243, tol = 1e-5)

  # Then doing Main activity producer CHP plants:
  res %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Transformation processes", Flow == "Main activity producer CHP plants [from Other products]", Product == "Industrial waste") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(-15)

  res %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Transformation processes", Flow == "Main activity producer CHP plants [from Other products]", Product == "Heat [from Other products]") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(29.12621, tol = 1e-5)

  res %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Transformation processes", Flow == "Main activity producer CHP plants [from Other products]", Product == "Electricity [from Other products]") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(14.56311, tol = 1e-5)

  res %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Energy industry own use", Flow == "Main activity producer CHP plants [from Other products]", Product == "Natural gas") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(-2.912621, tol = 1e-5)

  res %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Transformation processes", Flow == "Main activity producer CHP plants [from Natural gas]", Product == "Natural gas") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(-200)

  res %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Transformation processes", Flow == "Main activity producer CHP plants [from Natural gas]", Product == "Heat [from Natural gas]") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(388.3495, tol = 1e-5)

  res %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Transformation processes", Flow == "Main activity producer CHP plants [from Natural gas]", Product == "Electricity [from Natural gas]") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(194.1748, tol = 1e-5)

  res %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Energy industry own use", Flow == "Main activity producer CHP plants [from Natural gas]", Product == "Natural gas") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(-38.83495, tol = 1e-5)

  res %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Transformation processes", Flow == "Main activity producer CHP plants [from Oil products]", Product == "Fuel oil") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(-300)

  res %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Transformation processes", Flow == "Main activity producer CHP plants [from Oil products]", Product == "Heat [from Oil products]") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(582.5243, tol = 1e-5)

  res %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Transformation processes", Flow == "Main activity producer CHP plants [from Oil products]", Product == "Electricity [from Oil products]") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(291.2621, tol = 1e-5)

  res %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Energy industry own use", Flow == "Main activity producer CHP plants [from Oil products]", Product == "Natural gas") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(-58.25243, tol = 1e-5)
})




test_that("specify_other_elec_heat_production works", {

  # Path to dummy AB data
  A_B_path <- system.file("extdata/A_B_data_full_2018_format.csv", package = "ECCTools")

  # Loading AB_data
  AB_data <- A_B_path %>%
    IEATools::load_tidy_iea_df()

  # Specifying AB data
  tidy_AB_data <- AB_data %>%
    IEATools::specify_all()

  # Add flows
  tidy_AB_data_added_flows <- tidy_AB_data %>%
    tibble::add_row(
      Country = "A", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 2018, Ledger.side = "Supply", Unit = "ktoe",
      Flow.aggregation.point = "Transformation processes", Flow = "Main activity producer electricity plants", Product = "Industrial waste", E.dot = -1000
    ) %>%
    tibble::add_row(
      Country = "A", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 2018, Ledger.side = "Supply", Unit = "ktoe",
      Flow.aggregation.point = "Transformation processes", Flow = "Main activity producer heat plants", Product = "Heat", E.dot = 1000
    ) %>%
    tibble::add_row(
      Country = "A", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 2018, Ledger.side = "Supply", Unit = "ktoe",
      Flow.aggregation.point = "Transformation processes", Flow = "Main activity producer heat plants", Product = "Natural gas", E.dot = -200
    ) %>%
    tibble::add_row(
      Country = "A", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 2018, Ledger.side = "Supply", Unit = "ktoe",
      Flow.aggregation.point = "Transformation processes", Flow = "Main activity producer heat plants", Product = "Fuel oil", E.dot = -300
    ) %>%
    tibble::add_row(
      Country = "A", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 2018, Ledger.side = "Supply", Unit = "ktoe",
      Flow.aggregation.point = "Transformation processes", Flow = "Main activity producer heat plants", Product = "Industrial waste", E.dot = -15
    ) %>%
    tibble::add_row(
      Country = "A", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 2018, Ledger.side = "Supply", Unit = "ktoe",
      Flow.aggregation.point = "Energy industry own use", Flow = "Main activity producer heat plants", Product = "Natural gas", E.dot = -100
    ) %>%
    tibble::add_row(
      Country = "A", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 2018, Ledger.side = "Supply", Unit = "ktoe",
      Flow.aggregation.point = "Transformation processes", Flow = "Main activity producer CHP plants", Product = "Heat", E.dot = 1000
    ) %>%
    tibble::add_row(
      Country = "A", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 2018, Ledger.side = "Supply", Unit = "ktoe",
      Flow.aggregation.point = "Transformation processes", Flow = "Main activity producer CHP plants", Product = "Electricity", E.dot = 500
    ) %>%
    tibble::add_row(
      Country = "A", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 2018, Ledger.side = "Supply", Unit = "ktoe",
      Flow.aggregation.point = "Transformation processes", Flow = "Main activity producer CHP plants", Product = "Natural gas", E.dot = -200
    ) %>%
    tibble::add_row(
      Country = "A", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 2018, Ledger.side = "Supply", Unit = "ktoe",
      Flow.aggregation.point = "Transformation processes", Flow = "Main activity producer CHP plants", Product = "Fuel oil", E.dot = -300
    ) %>%
    tibble::add_row(
      Country = "A", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 2018, Ledger.side = "Supply", Unit = "ktoe",
      Flow.aggregation.point = "Transformation processes", Flow = "Main activity producer CHP plants", Product = "Industrial waste", E.dot = -15
    ) %>%
    tibble::add_row(
      Country = "A", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 2018, Ledger.side = "Supply", Unit = "ktoe",
      Flow.aggregation.point = "Energy industry own use", Flow = "Main activity producer CHP plants", Product = "Natural gas", E.dot = -100
    ) %>%
    # Adding specific flows for the other elec/heat production flows
    # Chemical heat for elec production
    tibble::add_row(
      Country = "A", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 2018, Ledger.side = "Supply", Unit = "ktoe",
      Flow.aggregation.point = "Transformation processes", Flow = "Chemical heat for electricity production", Product = "Electricity", E.dot = 26
    ) %>%
    tibble::add_row(
      Country = "A", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 2018, Ledger.side = "Supply", Unit = "ktoe",
      Flow.aggregation.point = "Transformation processes", Flow = "Chemical heat for electricity production", Product = "Heat", E.dot = -32
    ) %>%
    # Electric boilers
    tibble::add_row(
      Country = "A", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 2018, Ledger.side = "Supply", Unit = "ktoe",
      Flow.aggregation.point = "Transformation processes", Flow = "Electric boilers", Product = "Electricity", E.dot = -47
    ) %>%
    tibble::add_row(
      Country = "A", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 2018, Ledger.side = "Supply", Unit = "ktoe",
      Flow.aggregation.point = "Transformation processes", Flow = "Electric boilers", Product = "Heat", E.dot = 39
    ) %>%
    # Heat pumps
    tibble::add_row(
      Country = "A", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 2018, Ledger.side = "Supply", Unit = "ktoe",
      Flow.aggregation.point = "Transformation processes", Flow = "Heat pumps", Product = "Electricity", E.dot = -59
    ) %>%
    tibble::add_row(
      Country = "A", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 2018, Ledger.side = "Supply", Unit = "ktoe",
      Flow.aggregation.point = "Transformation processes", Flow = "Heat pumps", Product = "Heat", E.dot = 82
    ) %>%
    # Manufacture of heat
    tibble::add_row(
      Country = "B", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 2018, Ledger.side = "Supply", Unit = "ktoe",
      Flow.aggregation.point = "Transformation processes", Flow = "Manufacture [of Heat]", Product = "Heat [from Resources]", E.dot = -101
    ) %>%
    tibble::add_row(
      Country = "B", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 2018, Ledger.side = "Supply", Unit = "ktoe",
      Flow.aggregation.point = "Transformation processes", Flow = "Manufacture [of Heat]", Product = "Heat", E.dot = 101
    )

  res <- tidy_AB_data_added_flows %>%
    specify_elect_heat_fossil_fuels() %>%
    specify_other_elec_heat_production()

  #### Keeping a couple of tests to check that specify elect heat from fossil fuels function still works
  # Checking flows are doing what they should do:
  # Doing Main activity producer electricity plants:
  res %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Transformation processes", Flow == "Main activity producer electricity plants [from Other products]", Product == "Industrial waste") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(-1000)

  res %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Transformation processes", Flow == "Main activity producer electricity plants [from Other products]", Product == "Electricity [from Other products]") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(744.1859, tol = 1e-3)

  res %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Transformation processes", Flow == "Main activity producer electricity plants [from Natural gas]", Product == "Natural gas") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(-600)

  res %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Transformation processes", Flow == "Main activity producer electricity plants [from Natural gas]", Product == "Heat [from Natural gas]") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(27.90698, tol = 1e-5)

  res %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Energy industry own use", Flow == "Main activity producer electricity plants [from Natural gas]", Product == "Electricity") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(-8.372094, tol = 1e-5)

  res %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Energy industry own use", Flow == "Main activity producer heat plants [from Other products]", Product == "Natural gas") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(-2.912621, tol = 1e-5)

  res %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Transformation processes", Flow == "Main activity producer heat plants [from Natural gas]", Product == "Natural gas") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(-200)

  res %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Transformation processes", Flow == "Main activity producer heat plants [from Natural gas]", Product == "Heat [from Natural gas]") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(388.3495, tol = 1e-5)

  res %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Energy industry own use", Flow == "Main activity producer heat plants [from Oil products]", Product == "Natural gas") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(-58.25243, tol = 1e-5)

  res %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Transformation processes", Flow == "Main activity producer CHP plants [from Other products]", Product == "Industrial waste") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(-15)

  res %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Transformation processes", Flow == "Main activity producer CHP plants [from Other products]", Product == "Heat [from Other products]") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(29.12621, tol = 1e-5)

  res %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Transformation processes", Flow == "Main activity producer CHP plants [from Other products]", Product == "Electricity [from Other products]") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(14.56311, tol = 1e-5)

  res %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Transformation processes", Flow == "Main activity producer CHP plants [from Oil products]", Product == "Heat [from Oil products]") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(582.5243, tol = 1e-5)

  res %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Energy industry own use", Flow == "Main activity producer CHP plants [from Oil products]", Product == "Natural gas") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(-58.25243, tol = 1e-5)


  # And running specific tests for the specify_other_elec_heat_production() function
  # Chemical heat for electricity production
  res %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Transformation processes", Flow == "Chemical heat for electricity production", Product == "Electricity") %>%
    nrow() %>%
    expect_equal(0)
  res %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Transformation processes", Flow == "Chemical heat for electricity production", Product == "Electricity [from Other processes]") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(26)
  res %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Transformation processes", Flow == "Chemical heat for electricity production", Product == "Heat") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(-32)
  # Electric boilers
  res %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Transformation processes", Flow == "Electric boilers", Product == "Heat") %>%
    nrow() %>%
    expect_equal(0)
  res %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Transformation processes", Flow == "Electric boilers", Product == "Heat [from Other processes]") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(39)
  res %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Transformation processes", Flow == "Electric boilers", Product == "Electricity") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(-47)
  # Heat pumps
  res %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Transformation processes", Flow == "Heat pumps", Product == "Heat") %>%
    nrow() %>%
    expect_equal(0)
  res %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Transformation processes", Flow == "Heat pumps", Product == "Heat [from Other processes]") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(82)
  res %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Transformation processes", Flow == "Heat pumps", Product == "Electricity") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(-59)
  # Manufacture of heat
  res %>%
    dplyr::filter(Country == "B", Flow.aggregation.point == "Transformation processes", Flow == "Manufacture [of Heat]", Product == "Heat [from Resources]") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(-101)
  res %>%
    dplyr::filter(Country == "B", Flow.aggregation.point == "Transformation processes", Flow == "Manufacture [of Heat]", Product == "Heat [from Other processes]") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(101)
})


test_that("specify_elect_heat_markets works",{

  # Path to dummy AB data
  A_B_path <- system.file("extdata/A_B_data_full_2018_format.csv", package = "ECCTools")

  # Loading AB_data
  AB_data <- A_B_path %>%
    IEATools::load_tidy_iea_df()

  # Specifying AB data
  tidy_AB_data <- AB_data %>%
    IEATools::specify_all()

  # This here should not change the data frame:
  tidy_AB_data_1 <- tidy_AB_data %>%
    specify_elect_heat_markets()

  a <- tidy_AB_data %>%
    dplyr::left_join(tidy_AB_data_1, by = c("Country", "Method", "Energy.type", "Last.stage", "Ledger.side", "Flow.aggregation.point", "Flow", "Product", "Unit")) %>%
    dplyr::mutate(
      err = abs(E.dot.x - E.dot.y)
    )

  expect_true(all(a$err == 0))


  # Now, making actual tests regarding values
  res <- AB_data %>%
    tibble::add_row(
      Country = "A", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 2018, Ledger.side = "Supply", Unit = "ktoe",
      Flow.aggregation.point = "Transformation processes", Flow = "Chemical heat for electricity production", Product = "Electricity", E.dot = 26
    ) %>%
    tibble::add_row(
      Country = "A", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 2018, Ledger.side = "Supply", Unit = "ktoe",
      Flow.aggregation.point = "Transformation processes", Flow = "Chemical heat for electricity production", Product = "Heat", E.dot = -32
    ) %>%
    # Electric boilers
    tibble::add_row(
      Country = "A", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 2018, Ledger.side = "Supply", Unit = "ktoe",
      Flow.aggregation.point = "Transformation processes", Flow = "Electric boilers", Product = "Electricity", E.dot = -47
    ) %>%
    tibble::add_row(
      Country = "A", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 2018, Ledger.side = "Supply", Unit = "ktoe",
      Flow.aggregation.point = "Transformation processes", Flow = "Electric boilers", Product = "Heat", E.dot = 39
    ) %>%
    # Heat pumps
    tibble::add_row(
      Country = "A", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 2018, Ledger.side = "Supply", Unit = "ktoe",
      Flow.aggregation.point = "Transformation processes", Flow = "Heat pumps", Product = "Electricity", E.dot = -59
    ) %>%
    tibble::add_row(
      Country = "A", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 2018, Ledger.side = "Supply", Unit = "ktoe",
      Flow.aggregation.point = "Transformation processes", Flow = "Heat pumps", Product = "Heat", E.dot = 82
    ) %>%
    # Manufacture of heat
    tibble::add_row(
      Country = "B", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 2018, Ledger.side = "Supply", Unit = "ktoe",
      Flow.aggregation.point = "Transformation processes", Flow = "Manufacture [of Heat]", Product = "Heat [from Resources]", E.dot = -101
    ) %>%
    tibble::add_row(
      Country = "B", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 2018, Ledger.side = "Supply", Unit = "ktoe",
      Flow.aggregation.point = "Transformation processes", Flow = "Manufacture [of Heat]", Product = "Heat", E.dot = 101
    ) %>%
    specify_elect_heat_renewables() %>%
    specify_elect_heat_fossil_fuels() %>%
    specify_other_elec_heat_production() %>%
    specify_elect_heat_markets()

  # Checking electricity market flows:
  res %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Transformation processes",
                  Flow == "Electricity market", Product == "Electricity") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(3226)

  res %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Transformation processes",
                  Flow == "Electricity market", Product == "Electricity [from Coal products]") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(-2618.18182, tol = 1e-5)

  res %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Transformation processes",
                  Flow == "Electricity market", Product == "Electricity [from Natural gas]") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(-581.81818, tol = 1e-5)

  res %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Transformation processes",
                  Flow == "Electricity market", Product == "Electricity [from Other processes]") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(-26, tol = 1e-5)

  res %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Transformation processes",
                  Flow == "Chemical heat for electricity production", Product == "Electricity [from Other processes]") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(26, tol = 1e-5)

  # Checking heat market flows:
  res %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Transformation processes",
                  Flow == "Heat market", Product == "Heat") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(371)

  res %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Transformation processes",
                  Flow == "Heat market", Product == "Heat [from Coal products]") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(-163.63636, tol = 1e-5)

  res %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Transformation processes",
                  Flow == "Heat market", Product == "Heat [from Natural gas]") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(-36.36364, tol = 1e-5)

  res %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Transformation processes",
                  Flow == "Heat market", Product == "Heat [from Other processes]") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(-171, tol = 1e-5)

  res %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Transformation processes",
                  Flow == "Electric boilers", Product == "Heat [from Other processes]") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(39, tol = 1e-5)

  res %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Transformation processes",
                  Flow == "Electric boilers", Product == "Electricity") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(-47, tol = 1e-5)

  res %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Transformation processes",
                  Flow == "Heat pumps", Product == "Heat [from Other processes]") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(82, tol = 1e-5)

  res %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Transformation processes",
                  Flow == "Heat pumps", Product == "Electricity") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(-59, tol = 1e-5)

  # Checking EIOU flows for electricity and heat:
  res %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Energy industry own use",
                  Flow == "Oil and gas extraction", Product == "Electricity") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(-200)

  res %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Energy industry own use",
                  Flow == "Oil refineries", Product == "Heat") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(-100)
})


test_that("specify_elect_heat_nuclear works", {

  # Path to dummy AB data
  A_B_path <- system.file("extdata/A_B_data_full_2018_format.csv", package = "ECCTools")

  # Loading AB_data
  AB_data <- A_B_path %>%
    IEATools::load_tidy_iea_df()

  # Specifying AB data
  tidy_AB_data <- AB_data %>%
    tibble::add_row(
      Country = "A",
      Method = "PCM",
      Energy.type = "E",
      Last.stage = "Final",
      Year = 2018,
      Ledger.side = "Supply",
      Flow.aggregation.point = "Transformation processes",
      Flow = "Nuclear industry",
      Product = "Electricity",
      Unit = "ktoe",
      E.dot = 200
    ) %>%
    tibble::add_row(
      Country = "A",
      Method = "PCM",
      Energy.type = "E",
      Last.stage = "Final",
      Year = 2018,
      Ledger.side = "Supply",
      Flow.aggregation.point = "Transformation processes",
      Flow = "Nuclear industry",
      Product = "Heat",
      Unit = "ktoe",
      E.dot = 100
    ) %>%
    tibble::add_row(
      Country = "A",
      Method = "PCM",
      Energy.type = "E",
      Last.stage = "Final",
      Year = 2018,
      Ledger.side = "Supply",
      Flow.aggregation.point = "Transformation processes",
      Flow = "Nuclear industry",
      Product = "Electricity",
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
      Flow.aggregation.point = "Transformation processes",
      Flow = "Nuclear industry",
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
      Flow.aggregation.point = "Transformation processes",
      Flow = "Nuclear industry",
      Product = "Coke oven coke",
      Unit = "ktoe",
      E.dot = 100
    )

  # Now, results:
  res1 <- tidy_AB_data %>%
    specify_elect_heat_nuclear()

  res1 %>%
    dplyr::filter(Flow == "Nuclear industry") %>%
    dplyr::filter(Product == "Heat") %>%
    nrow() %>%
    expect_equal(0)

  res1 %>%
    dplyr::filter(Flow == "Nuclear industry") %>%
    dplyr::filter(Product == "Electricity") %>%
    nrow() %>%
    expect_equal(1)

  res1 %>%
    dplyr::filter(Flow == "Nuclear industry") %>%
    dplyr::filter(Product == "Electricity [from Nuclear]") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(200)

  res1 %>%
    dplyr::filter(Flow == "Nuclear industry") %>%
    dplyr::filter(Product == "Heat [from Nuclear]") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(100)

  res1 %>%
    dplyr::filter(Flow == "Nuclear industry") %>%
    nrow() %>%
    expect_equal(5)

  # Creating the market:
  res2 <- tidy_AB_data %>%
    specify_elect_heat_renewables() %>%
    specify_elect_heat_fossil_fuels() %>%
    specify_elect_heat_nuclear() %>%
    specify_elect_heat_markets()

  # Checking electricity market flows:
  res2 %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Transformation processes",
                  Flow == "Electricity market", Product == "Electricity") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(3400)

  res2 %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Transformation processes",
                  Flow == "Electricity market", Product == "Electricity [from Coal products]") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(-2618.18182, tol = 1e-5)

  res2 %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Transformation processes",
                  Flow == "Electricity market", Product == "Electricity [from Natural gas]") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(-581.81818, tol = 1e-5)

  # Add tests specific to nuclear industry:
  res2 %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Transformation processes",
                  Flow == "Electricity market", Product == "Electricity [from Nuclear]") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(-200)

  # Checking heat market flows:
  res2 %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Transformation processes",
                  Flow == "Heat market", Product == "Heat") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(300)

  res2 %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Transformation processes",
                  Flow == "Heat market", Product == "Heat [from Coal products]") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(-163.63636, tol = 1e-5)

  res2 %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Transformation processes",
                  Flow == "Heat market", Product == "Heat [from Natural gas]") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(-36.36364, tol = 1e-5)

  # Add tests specific to nuclear industry:
  res2 %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Transformation processes",
                  Flow == "Heat market", Product == "Heat [from Nuclear]") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(-100)

  # Checking EIOU flows for electricity and heat:
  res2 %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Energy industry own use",
                  Flow == "Oil and gas extraction", Product == "Electricity") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(-200)

  res2 %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Energy industry own use",
                  Flow == "Oil refineries", Product == "Heat") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(-100)
})



# test_that("specify_elect_heat_eiou_flows works", {
#
#   # Path to dummy AB data
#   A_B_path <- system.file("extdata/A_B_data_full_2018_format.csv", package = "ECCTools")
#
#   # Loading AB_data
#   AB_data <- A_B_path %>%
#     IEATools::load_tidy_iea_df()
#
#   # Specifying AB data
#   tidy_AB_data <- AB_data %>%
#     IEATools::specify_all()
#
#   tidy_AB_data_1 <- tidy_AB_data %>%
#     specify_elect_heat_eiou_flows()
#
#   # Should be equal, and it is!!
#   expect_equal(tidy_AB_data, tidy_AB_data_1)
#
#   # Now, more complex tests
#   res <- AB_data %>%
#     IEATools::specify_all() %>%
#     tibble::add_row(
#       Country = "A", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 2018, Ledger.side = "Supply", Unit = "ktoe",
#       Flow.aggregation.point = "Transformation processes", Flow = "Main activity producer electricity plants", Product = "Industrial waste", E.dot = -100
#     ) %>%
#     tibble::add_row(
#       Country = "A", Method = "PCM", Energy.type = "E", Last.stage = "Final", Year = 2018, Ledger.side = "Supply", Unit = "ktoe",
#       Flow.aggregation.point = "Transformation processes", Flow = "Main activity producer electricity plants", Product = "Solar photovoltaics", E.dot = -10
#     ) %>%
#     specify_elect_heat_renewables() %>%
#     specify_elect_heat_fossil_fuels() %>%
#     specify_elect_heat_eiou_flows()
#
#   # Testing values:
#   res %>%
#     dplyr::filter(
#       Country == "A", Flow.aggregation.point == "Energy industry own use", Flow == "Main activity producer electricity plants",
#       Product == ""
#     ) %>%
#     magrittr::extract2("E.dot") %>%
#     expect_equal()
# })

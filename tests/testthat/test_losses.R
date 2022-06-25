
test_that("specify_losses_as_industry works",{

  # Path to data
  A_B_path <- system.file("extdata/A_B_data_full_2018_format.csv", package = "ECCTools")

  # Loading data
  AB_data_with_losses <- A_B_path %>%
    IEATools::load_tidy_iea_df() %>%
    # Adding losses of coke oven coke
    tibble::add_row(
      Country = "A",
      Method = "PCM",
      Energy.type = "E",
      Last.stage = "Final",
      Year = 2018,
      Ledger.side = "Supply",
      Flow.aggregation.point = "TFC compare",
      Flow = "Losses",
      Product = "Coke oven coke",
      Unit = "ktoe",
      E.dot = -20
    ) %>%
    # Adding losses of Electricity
    tibble::add_row(
      Country = "A",
      Method = "PCM",
      Energy.type = "E",
      Last.stage = "Final",
      Year = 2018,
      Ledger.side = "Supply",
      Flow.aggregation.point = "TFC compare",
      Flow = "Losses",
      Product = "Electricity",
      Unit = "ktoe",
      E.dot = -50
    ) %>%
    # Adding losses of Heat
    tibble::add_row(
      Country = "A",
      Method = "PCM",
      Energy.type = "E",
      Last.stage = "Final",
      Year = 2018,
      Ledger.side = "Supply",
      Flow.aggregation.point = "TFC compare",
      Flow = "Losses",
      Product = "Heat",
      Unit = "ktoe",
      E.dot = -10
    ) %>%
    # Balancing this with more supply
    # Coke oven coke
    tibble::add_row(
      Country = "A",
      Method = "PCM",
      Energy.type = "E",
      Last.stage = "Final",
      Year = 2018,
      Ledger.side = "Supply",
      Flow.aggregation.point = "Transformation processes",
      Flow = "A balancing industry",
      Product = "Coke oven coke",
      Unit = "ktoe",
      E.dot = 20
    ) %>%
    # Electricity
    tibble::add_row(
      Country = "A",
      Method = "PCM",
      Energy.type = "E",
      Last.stage = "Final",
      Year = 2018,
      Ledger.side = "Supply",
      Flow.aggregation.point = "Transformation processes",
      Flow = "A balancing industry",
      Product = "Electricity",
      Unit = "ktoe",
      E.dot = 50
    ) %>%
    # Heat
    tibble::add_row(
      Country = "A",
      Method = "PCM",
      Energy.type = "E",
      Last.stage = "Final",
      Year = 2018,
      Ledger.side = "Supply",
      Flow.aggregation.point = "Transformation processes",
      Flow = "A balancing industry",
      Product = "Heat",
      Unit = "ktoe",
      E.dot = 10
    )


  tidy_AB_data_losses <- AB_data_with_losses %>%
    IEATools::specify_all() %>%
    specify_elect_heat_renewables() %>%
    specify_elect_heat_renewables() %>%
    specify_elect_heat_fossil_fuels() %>%
    specify_elect_heat_nuclear() %>%
    specify_other_elec_heat_production() %>%
    specify_elect_heat_markets() %>%
    IEATools::add_psut_matnames()


  res_dta <- tidy_AB_data_losses %>%
    specify_losses_as_industry()

  # TESTING RESULTS:

  # Balanced
  res_dta %>%
    IEATools::tidy_iea_df_balanced() %>%
    expect_true()

  # Number of rows
  res_dta %>% nrow() %>%
    expect_equal(137)

  # Actual values
  res_dta %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Transformation processes", Flow == "Losses [of Coke oven coke]", Product == "Coke oven coke", matnames == "V") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(400)
  res_dta %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Transformation processes", Flow == "Losses [of Coke oven coke]", Product == "Coke oven coke [before Losses]", matnames == "U_feed") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(-420)

  res_dta %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Transformation processes", Flow == "Losses [of Electricity]", Product == "Electricity", matnames == "V") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(3200)
  res_dta %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Transformation processes", Flow == "Losses [of Electricity]", Product == "Electricity [before Losses]", matnames == "U_feed") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(-3250)

  res_dta %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Transformation processes", Flow == "Losses [of Heat]", Product == "Heat", matnames == "V") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(250)
  res_dta %>%
    dplyr::filter(Country == "A", Flow.aggregation.point == "Transformation processes", Flow == "Losses [of Heat]", Product == "Heat [before Losses]", matnames == "U_feed") %>%
    magrittr::extract2("E.dot") %>%
    expect_equal(-260)


  # Checking what happens if there are more losses than domestic supply...! (I.e. the rest of supply comes from imports or so...)
  AB_data_big_losses <- A_B_path %>%
    IEATools::load_tidy_iea_df() %>%
    # Adding losses of coke oven coke
    tibble::add_row(
      Country = "A",
      Method = "PCM",
      Energy.type = "E",
      Last.stage = "Final",
      Year = 2018,
      Ledger.side = "Supply",
      Flow.aggregation.point = "TFC compare",
      Flow = "Losses",
      Product = "Coke oven coke",
      Unit = "ktoe",
      E.dot = -600
    )

  tidy_AB_data_losses <- AB_data_big_losses %>%
    IEATools::specify_all() %>%
    specify_elect_heat_renewables() %>%
    specify_elect_heat_renewables() %>%
    specify_elect_heat_fossil_fuels() %>%
    specify_elect_heat_nuclear() %>%
    specify_other_elec_heat_production() %>%
    specify_elect_heat_markets() %>%
    IEATools::add_psut_matnames()

  res_dta <- tidy_AB_data_losses %>%
    specify_losses_as_industry() %>%
    expect_error()
})






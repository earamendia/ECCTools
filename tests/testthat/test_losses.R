
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

  # Testing

  # Balanced
  res_dta %>%
    tidy_iea_df_balanced() %>%
    expect_true()

  # Number of rows
  res_dta %>% nrow() %>%
    expect_equal()

  # Actual values
  res_dta %>%
    dplyr::filter() %>%
    magrittr::extract2("E.dot") %>%
    expect_equal()



  # Checking what happens if values become negative:


})






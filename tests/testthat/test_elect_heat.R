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



})

###########################################################
context("Specifying PSUT flows")
###########################################################


# Testing the function that gathers "Main activity producer" and "Autoproducer" flows/industries.
test_that("gather_producer_autoproducer works", {

  A_B_path <- file.path("../../inst/A_B_data_full_2018_format_testing.csv")

  AB_data <- A_B_path %>%
    IEATools::load_tidy_iea_df()

  test <- AB_data %>%
    IEATools::specify_primary_production() %>%
    IEATools::specify_production_to_resources() %>%
    gather_producer_autoproducer()


  main_activity_flows <- test %>%
    dplyr::filter(stringr::str_detect(Flow, "Main activity"))

  expect_equal(length(main_activity_flows[["E.dot"]]), 26)

  expect_equal(main_activity_flows[["E.dot"]][[1]], -90)
  expect_equal(main_activity_flows[["E.dot"]][[3]], 160)
  expect_equal(main_activity_flows[["E.dot"]][[9]], 100)
  expect_equal(main_activity_flows[["E.dot"]][[24]], -847)
})



# Testing the function that routes the "Pumped storage" EIOU flow to "Main activity producer electricity plants."
test_that("route_pumped_storage works", {

  A_B_path <- file.path("../../inst/A_B_data_full_2018_format_testing.csv")

  AB_data <- A_B_path %>%
    IEATools::load_tidy_iea_df()

  test <- AB_data %>%
    IEATools::specify_primary_production() %>%
    IEATools::specify_production_to_resources() %>%
    gather_producer_autoproducer() %>%
    route_pumped_storage()

  remaining_number_storage_plants <- test %>%
    dplyr::filter(Flow == "Pumped storage plants") %>%
    dplyr::pull()

  expect_equal(length(remaining_number_storage_plants), 0)

  former_pumped_storage <- test %>%
    dplyr::filter(Flow == "Main activity producer electricity plants",
                  stringr::str_detect(Product, "Hard coal"),
                  Flow.aggregation.point == "Energy industry own use",
                  Country == "A")

  expect_equal(former_pumped_storage[["E.dot"]][[1]], -299)

})



# Testing the function that splits the "Own use in electricity, CHP and heat plants" EIOU flow into the three main producer activities (electricity, CHP and heat)
test_that("route_own_use_elect_chp_heat works", {

  A_B_path <- file.path("../../inst/A_B_data_full_2018_format_testing.csv")

  AB_data <- A_B_path %>%
    IEATools::load_tidy_iea_df()

  test <- AB_data %>%
    IEATools::specify_primary_production() %>%
    IEATools::specify_production_to_resources() %>%
    gather_producer_autoproducer() %>%
    route_pumped_storage() %>%
    route_own_use_elect_chp_heat()

  main_activity_eiou <- test %>%
    dplyr::filter(stringr::str_detect(Flow, "Main activity"), Country == "A", Flow.aggregation.point == "Energy industry own use")

  expect_equal(length(main_activity_eiou$Country), 9)

  expect_equal(main_activity_eiou[["E.dot"]][[1]], -5.82, 0.0001)
  expect_lt(main_activity_eiou[["E.dot"]][[2]] - (-3.492), 0.0001)
  expect_lt(main_activity_eiou[["E.dot"]][[5]] - (-55.555), 0.0001)
  expect_lt(main_activity_eiou[["E.dot"]][[6]] - (947.1481), 0.0001)
  expect_lt(main_activity_eiou[["E.dot"]][[9]] - (-11.111), 0.0001)

})



# Testing the function that adds a nuclear industry to the PSUT by adding some Transformation processes flows,
# and modifying the Main activity producer electricity and CHP plants
test_that("add_nuclear_industry works", {

  A_B_path <- file.path("../../inst/A_B_data_full_2018_format_testing.csv")

  AB_data <- A_B_path %>%
    IEATools::load_tidy_iea_df()

  test <- AB_data %>%
    IEATools::specify_primary_production() %>%
    IEATools::specify_production_to_resources() %>%
    gather_producer_autoproducer() %>%
    route_pumped_storage() %>%
    route_own_use_elect_chp_heat() %>%
    add_nuclear_industry()

  # One test on total length
  #expect_equal(length(test[["E.dot"]]), 140)

  # One test on length of nuclear industry flows


  # Some tests on actual values of nuclear industry flows

  # Some tests on actual values of elec & CHP plants
})





# test_that("route_non_specified_eiou works", {
#
#
#
#
#
#
# })



# test_that("route_non_specified_tp works", {
#
#
#
#
#
#
# })



# test_that("route_non_specified_flows works", {
#
# Do the same tests but combine the two above functions.
#
#
#
#
# })






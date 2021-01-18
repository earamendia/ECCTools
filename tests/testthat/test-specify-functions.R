###########################################################
context("Specifying PSUT flows")
###########################################################

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

  expect_equal(length(main_activity_flows[["E.dot"]]), 25)

  expect_equal(main_activity_flows[["E.dot"]][[1]], -90)
  expect_equal(main_activity_flows[["E.dot"]][[2]], 160)
  expect_equal(main_activity_flows[["E.dot"]][[8]], 100)
  expect_equal(main_activity_flows[["E.dot"]][[23]], -847)
})





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



# test_that("route_own_use_elect_chp_heat works", {
#
#
#
#
# })




# test_that("add_nuclear_industry works", {
#
#
#
#
#
#
# })





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
#
#
#
#
#
# })






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

  expect_equal(main_activity_flows %>%
                 dplyr::filter(
                   Country == "A",
                   Flow.aggregation.point == "Transformation processes",
                   Flow == "Main activity producer CHP plants",
                   Product == "Brown coal (if no detail) [of Coal mines]"
                   ) %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               -90)

  expect_equal(main_activity_flows %>%
                 dplyr::filter(
                   Country == "A",
                   Flow.aggregation.point == "Transformation processes",
                   Flow == "Main activity producer CHP plants",
                   Product == "Hard coal (if no detail) [of Coal mines]"
                 ) %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               160)

  expect_equal(main_activity_flows %>%
                 dplyr::filter(
                   Country == "A",
                   Flow.aggregation.point == "Transformation processes",
                   Flow == "Main activity producer electricity plants",
                   Product == "Hard coal (if no detail) [of Coal mines]"
                 ) %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               100)

  expect_equal(main_activity_flows %>%
                 dplyr::filter(
                   Country == "B",
                   Flow.aggregation.point == "Transformation processes",
                   Flow == "Main activity producer heat plants",
                   Product == "Brown coal (if no detail) [of Coal mines]"
                 ) %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               -847)
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

  expect_equal(former_pumped_storage %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               -299)

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

  expect_equal(main_activity_eiou %>%
                 dplyr::filter(
                   Country == "A",
                   Flow.aggregation.point == "Energy industry own use",
                   Flow == "Main activity producer CHP plants",
                   Product == "Anthracite [of Coal mines]") %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               -5.82,
               tolerance = 0.0001)

  expect_equal(main_activity_eiou %>%
                 dplyr::filter(
                   Country == "A",
                   Flow.aggregation.point == "Energy industry own use",
                   Flow == "Main activity producer CHP plants",
                   Product == "Electricity") %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               -3.492,
               tolerance = 0.0001)

  expect_equal(main_activity_eiou %>%
                 dplyr::filter(
                   Country == "A",
                   Flow.aggregation.point == "Energy industry own use",
                   Flow == "Main activity producer electricity plants",
                   Product == "Electricity") %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               -55.5555,
            tolerance = 0.0001)

  expect_equal(main_activity_eiou %>%
                 dplyr::filter(
                   Country == "A",
                   Flow.aggregation.point == "Energy industry own use",
                   Flow == "Main activity producer electricity plants",
                   Product == "Hard coal (if no detail) [of Coal mines]") %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               -947.1481,
            tolerance = 0.0001)

  expect_equal(main_activity_eiou %>%
                 dplyr::filter(
                   Country == "A",
                   Flow.aggregation.point == "Energy industry own use",
                   Flow == "Main activity producer heat plants",
                   Product == "Hard coal (if no detail) [of Coal mines]") %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               -11.1111,
               tolerance = 0.0001)

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
  expect_equal(length(test[["E.dot"]]), 140)

  # One test on length of nuclear industry flows
  expect_equal(test %>%
                 dplyr::filter(Flow == "Nuclear industry") %>%
                 dplyr::pull() %>%
                 length(),
               2)

  expect_equal(test %>%
                 dplyr::filter(Product == "Nuclear") %>%
                 dplyr::pull() %>%
                 length(),
               1)

  # Some tests on actual values of elec & CHP plants
  expect_equal(test %>%
                 dplyr::filter(Country == "A",
                               Flow.aggregation.point == "Transformation processes",
                               Flow == "Main activity producer electricity plants",
                               Product == "Electricity") %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               3163.7)

  expect_equal(test %>%
                 dplyr::filter(Country == "A",
                               Flow.aggregation.point == "Transformation processes",
                               Flow == "Main activity producer CHP plants",
                               Product == "Electricity") %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               56.7)

  # Some tests on actual values of nuclear industry flows
  expect_equal(test %>%
                 dplyr::filter(Country == "A",
                               Flow.aggregation.point == "Transformation processes",
                               Flow == "Nuclear industry",
                               Product == "Electricity") %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               39.6)

  expect_equal(test %>%
                 dplyr::filter(Country == "A",
                               Flow.aggregation.point == "Transformation processes",
                               Flow == "Nuclear industry",
                               Product == "Nuclear") %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               -120)

  # Now, new example adding heat output for CHP, country A
  second_test <- AB_data %>%
    tibble::add_row(Country = "A",
                    Method = "PCM",
                    Energy.type = "E",
                    Last.stage = "Final",
                    Year = 2018,
                    Ledger.side = "Supply",
                    Flow.aggregation.point = "Transformation processes",
                    Flow = "Main activity producer CHP plants",
                    Product = "Heat",
                    Unit = "ktoe",
                    E.dot = 30) %>%
    IEATools::specify_primary_production() %>%
    IEATools::specify_production_to_resources() %>%
    gather_producer_autoproducer() %>%
    route_pumped_storage() %>%
    route_own_use_elect_chp_heat() %>%
    add_nuclear_industry()

  # One test on total length
  expect_equal(length(second_test[["E.dot"]]), 142)

  # One test on length of nuclear industry flows
  expect_equal(second_test %>%
                 dplyr::filter(Flow == "Nuclear industry") %>%
                 dplyr::pull() %>%
                 length(),
               3)

  expect_equal(second_test %>%
                 dplyr::filter(Product == "Nuclear") %>%
                 dplyr::pull() %>%
                 length(),
               1)

  # Some tests on actual values of elec & CHP plants
  expect_equal(second_test %>%
                 dplyr::filter(Country == "A",
                               Flow.aggregation.point == "Transformation processes",
                               Flow == "Main activity producer electricity plants",
                               Product == "Electricity") %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               3163.7)

  expect_equal(second_test %>%
                 dplyr::filter(Country == "A",
                               Flow.aggregation.point == "Transformation processes",
                               Flow == "Main activity producer CHP plants",
                               Product == "Electricity") %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               57.8)

  expect_equal(second_test %>%
                 dplyr::filter(Country == "A",
                               Flow.aggregation.point == "Transformation processes",
                               Flow == "Main activity producer CHP plants",
                               Product == "Heat") %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               28.9)

  # Some tests on actual values of nuclear industry flows
  expect_equal(second_test %>%
                 dplyr::filter(Country == "A",
                               Flow.aggregation.point == "Transformation processes",
                               Flow == "Nuclear industry",
                               Product == "Electricity") %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               38.5)

  expect_equal(test %>%
                 dplyr::filter(Country == "A",
                               Flow.aggregation.point == "Transformation processes",
                               Flow == "Nuclear industry",
                               Product == "Nuclear") %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               -120)

  expect_equal(second_test %>%
                 dplyr::filter(Country == "A",
                               Flow.aggregation.point == "Transformation processes",
                               Flow == "Nuclear industry",
                               Product == "Heat") %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               1.1)
})





test_that("route_non_specified_eiou works", {

  A_B_path <- file.path("../../inst/A_B_data_full_2018_format_testing.csv")

  AB_data <- A_B_path %>%
    IEATools::load_tidy_iea_df()

  test <- AB_data %>%
    IEATools::specify_primary_production() %>%
    IEATools::specify_production_to_resources() %>%
    gather_producer_autoproducer() %>%
    route_pumped_storage() %>%
    route_own_use_elect_chp_heat() %>%
    add_nuclear_industry() %>%
    route_non_specified_eiou()


  # Now testing a few of the new A flows. Non-specified only had a "hard coal" product

  expect_equal(test %>%
              dplyr::filter(Country == "A",
                            Flow.aggregation.point == "Energy industry own use",
                            Flow == "Blast furnaces",
                            Product == "Hard coal (if no detail) [of Coal mines]") %>%
              dplyr::select(E.dot) %>%
              dplyr::pull(),
            -3.3299,
            tolerance = 0.001)

  expect_equal(test %>%
              dplyr::filter(Country == "A",
                            Flow.aggregation.point == "Energy industry own use",
                            Flow == "Coal mines",
                            Product == "Hard coal (if no detail) [of Coal mines]") %>%
              dplyr::select(E.dot) %>%
              dplyr::pull(),
            -4.7571,
            tolerance = 0.001)

  expect_equal(test %>%
              dplyr::filter(Country == "A",
                            Flow.aggregation.point == "Energy industry own use",
                            Flow == "Main activity producer electricity plants",
                            Product == "Hard coal (if no detail) [of Coal mines]") %>%
              dplyr::select(E.dot) %>%
              dplyr::pull(),
              (-947.148148 - 104.2088),
            tolerance = 0.001)

  expect_equal(test %>%
              dplyr::filter(Country == "A",
                            Flow.aggregation.point == "Energy industry own use",
                            Flow == "Main activity producer CHP plants",
                            Product == "Hard coal (if no detail) [of Coal mines]") %>%
              dplyr::select(E.dot) %>%
              dplyr::pull(),
              (-40.740741 - 4.7621),
            tolerance = 0.001)

  expect_equal(test %>%
              dplyr::filter(Country == "A",
                            Flow.aggregation.point == "Energy industry own use",
                            Flow == "Oil refineries",
                            Product == "Hard coal (if no detail) [of Coal mines]") %>%
              dplyr::select(E.dot) %>%
              dplyr::pull(),
            -28.54263,
            tolerance = 0.001)


  # Now, a few tests for country B.
  # There are non-specified flows both of hard and brown coal.

  expect_equal(test %>%
              dplyr::filter(Country == "B",
                            Flow.aggregation.point == "Energy industry own use",
                            Flow == "Blast furnaces",
                            Product == "Hard coal (if no detail) [of Coal mines]") %>%
              dplyr::select(E.dot) %>%
              dplyr::pull(),
            -23.9795,
            tolerance = 0.001)

  expect_equal(test %>%
              dplyr::filter(Country == "B",
                            Flow.aggregation.point == "Energy industry own use",
                            Flow == "Blast furnaces",
                            Product == "Brown coal (if no detail) [of Coal mines]") %>%
              dplyr::select(E.dot) %>%
              dplyr::pull(),
            -24.0816,
            tolerance = 0.001)

  expect_equal(test %>%
              dplyr::filter(Country == "B",
                            Flow.aggregation.point == "Energy industry own use",
                            Flow == "Coke ovens",
                            Product == "Hard coal (if no detail) [of Coal mines]") %>%
              dplyr::select(E.dot) %>%
              dplyr::pull(),
              -53.95407,
              tolerance = 0.001)

  expect_equal(test %>%
                 dplyr::filter(Country == "B",
                               Flow.aggregation.point == "Energy industry own use",
                               Flow == "Main activity producer electricity plants",
                               Product == "Hard coal (if no detail) [of Coal mines]") %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               (-50.4712 - 259.846827),
               tolerance = 0.001)

  expect_equal(test %>%
                 dplyr::filter(Country == "B",
                               Flow.aggregation.point == "Energy industry own use",
                               Flow == "Main activity producer heat plants",
                               Product == "Hard coal (if no detail) [of Coal mines]") %>%
                 dplyr::select(E.dot) %>%
                 dplyr::pull(),
               (-18.75406 - 96.553611),
               tolerance = 0.001)

})



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






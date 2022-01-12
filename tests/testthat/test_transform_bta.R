###########################################################
context("Transforming BTA")
###########################################################

# Testing Bilateral Trade Assumption functions

test_that("calc_bilateral_trade_df_gma works", {

  A_B_path <- system.file("extdata/A_B_data_full_2018_format.csv", package = "ECCTools")

  AB_data <- A_B_path %>%
    IEATools::load_tidy_iea_df() %>%
    IEATools::specify_all()

  bilateral_trade_df_gma <- AB_data %>%
    IEATools::add_psut_matnames() %>%
    calc_bilateral_trade_df_gma()

  # Check length
  expect_equal(dim(bilateral_trade_df_gma), c(8, 8))

  expect_equal(bilateral_trade_df_gma %>%
                 dplyr::filter(
                   Country == "A",
                   Product == "Coke oven coke") %>%
                 dplyr::select(Share_Exports_By_Product) %>%
                 dplyr::pull(),
               1)

  expect_equal(bilateral_trade_df_gma %>%
                 dplyr::filter(
                   Country == "A",
                   Product == "Natural gas") %>%
                 dplyr::select(Share_Exports_By_Product) %>%
                 dplyr::pull(),
               0)

  expect_equal(bilateral_trade_df_gma %>%
                 dplyr::filter(
                   Country == "B",
                   Product == "Natural gas") %>%
                 dplyr::select(Share_Exports_By_Product) %>%
                 dplyr::pull(),
               1)

  expect_equal(bilateral_trade_df_gma %>%
                 dplyr::filter(
                   Country == "B",
                   Product == "Coke oven coke") %>%
                 dplyr::select(Share_Exports_By_Product) %>%
                 dplyr::pull(),
               0)
})


# Testing the checking_bilateral_trade_df function

test_that("check_bilateral_trade_df works", {

  A_B_path <- system.file("extdata/A_B_data_full_2018_format.csv", package = "ECCTools")

  AB_data <- A_B_path %>%
    IEATools::load_tidy_iea_df() %>%
    IEATools::specify_all()

  bilateral_trade_df_gma <- AB_data %>%
    IEATools::add_psut_matnames() %>%
    calc_bilateral_trade_df_gma()

  expect_true(check_bilateral_trade_df(bilateral_trade_df_gma))

  # Doing a second one
  bilateral_trade_df_gma_2 <- bilateral_trade_df_gma %>%
    dplyr::bind_rows(
      bilateral_trade_df_gma %>%
        dplyr::mutate(
          Year = 2014
        )
    ) %>%
    dplyr::bind_rows(
      bilateral_trade_df_gma %>%
        dplyr::mutate(
          Method = "A very odd method"
        )
    ) %>%
    dplyr::bind_rows(
      bilateral_trade_df_gma %>%
        dplyr::mutate(
          Last.stage = "A very odd stage"
        )
    ) %>%
    dplyr::bind_rows(
      bilateral_trade_df_gma %>%
        dplyr::mutate(
          Energy.type = "A very odd energy type"
        )
    )
  expect_true(check_bilateral_trade_df(bilateral_trade_df_gma_2))

  # A third one that shouldn't work
  res <- bilateral_trade_df_gma %>%
    dplyr::mutate(
      Product = "an odd product"
    )
  expect_error(check_bilateral_trade_df(res))
})



test_that("specify_MR_Y_U_bta works", {

  A_B_path <- system.file("extdata/A_B_data_full_2018_format.csv", package = "ECCTools")

  AB_data <- A_B_path %>%
    IEATools::load_tidy_iea_df() %>%
    IEATools::specify_all()

  MR_Y_U_bta <- AB_data %>%
    IEATools::add_psut_matnames() %>%
    specify_MR_Y_U_bta()

  # In theory, we should get exactly the same outcome as when running the gma assumption here.
  MR_Y_U_gma <- AB_data %>%
    IEATools::add_psut_matnames() %>%
    specify_MR_Y_U_gma()

  # Nice. Same data frames.
  expect_equal(MR_Y_U_bta, MR_Y_U_gma)


  # Now, let's see if we feed a particular trade matrix!
  # Now, say that we have a gap - country B provenience isn't in there!
  # Then all the coke oven coke that A imports from B should now come instead from A!

  dummy_AB_trade_matrix <- AB_data %>%
    IEATools::add_psut_matnames() %>%
    calc_bilateral_trade_df_gma() %>%
    dplyr::filter(Provenience != "B")

  MR_Y_U_bta <- AB_data %>%
    IEATools::add_psut_matnames() %>%
    specify_MR_Y_U_bta(bilateral_trade_df = dummy_AB_trade_matrix)

  # These should still be equal
  expect_equal(MR_Y_U_bta %>%
                 dplyr::arrange(Country, Method, Energy.type, Last.stage, Year, Ledger.side, Flow.aggregation.point, Flow, Product, Unit, matnames),
               MR_Y_U_gma %>%
                 dplyr::arrange(Country, Method, Energy.type, Last.stage, Year, Ledger.side, Flow.aggregation.point, Flow, Product, Unit, matnames))


  # Now, say we modify the bilateral trade matrix, so that everything comes from country A!
  second_dummy_AB_trade_matrix <- AB_data %>%
    IEATools::add_psut_matnames() %>%
    calc_bilateral_trade_df_gma() %>%
    dplyr::filter(Provenience != "B") %>%
    tibble::add_row(
      Provenience = "A",
      Country = "A",
      Method = "PCM",
      Energy.type = "E",
      Last.stage = "Final",
      Year = 2018,
      Product = "Coke oven coke",
      Share_Exports_By_Product = 1
    )

  MR_Y_U_bta <- AB_data %>%
    IEATools::add_psut_matnames() %>%
    specify_MR_Y_U_bta(bilateral_trade_df = second_dummy_AB_trade_matrix)

  # Checking data frames are NOT the same
  expect_true(! nrow(MR_Y_U_bta) == nrow(MR_Y_U_gma))

  # So actually the length of the BTA data frame should be equal to the GMA data frame minus the number of times that coke oven coke is consummed by A.
  # i.e. three times.
  expect_equal(nrow(MR_Y_U_bta)+3,
               nrow(MR_Y_U_gma))

  # If we take Coke oven coke out, then we should have the same data frames.
  # Works, awesome.
  expect_equal(MR_Y_U_bta %>%
                 dplyr::arrange(Country, Method, Energy.type, Last.stage, Year, Ledger.side, Flow.aggregation.point, Flow, Product, Unit, matnames) %>%
                 dplyr::filter(! stringr::str_detect(Product, "Coke oven coke")),
               MR_Y_U_gma %>%
                 dplyr::arrange(Country, Method, Energy.type, Last.stage, Year, Ledger.side, Flow.aggregation.point, Flow, Product, Unit, matnames) %>%
                 dplyr::filter(! stringr::str_detect(Product, "Coke oven coke")))


  # Further, checking particular values
  expect_equal(MR_Y_U_bta %>%
                 dplyr::filter(
                   Flow == "{A}_Residential",
                   Product == "{A}_Coke oven coke"
                 ) %>%
                 magrittr::extract2("E.dot"),
               120)

  expect_equal(MR_Y_U_bta %>%
                 dplyr::filter(
                   Flow == "{A}_Road",
                   Product == "{A}_Coke oven coke"
                 ) %>%
                 magrittr::extract2("E.dot"),
               80)

  expect_equal(MR_Y_U_bta %>%
                 dplyr::filter(
                   Flow == "{A}_Blast furnaces",
                   Product == "{A}_Coke oven coke"
                 ) %>%
                 magrittr::extract2("E.dot"),
               -800)
})



test_that("transform_to_bta works", {

  A_B_path <- system.file("extdata/A_B_data_full_2018_format.csv", package = "ECCTools")

  AB_data <- A_B_path %>%
    IEATools::load_tidy_iea_df() %>%
    IEATools::specify_all()

  # First checking whether gma and bta, without trade matrix, give the same result:
  MR_PSUT_gma <- AB_data %>%
    IEATools::add_psut_matnames() %>%
    transform_to_gma()

  MR_PSUT_bta <- AB_data %>%
    IEATools::add_psut_matnames() %>%
    transform_to_bta()

  # Nice one!
  expect_equal(MR_PSUT_gma, MR_PSUT_bta)


  # Then, try with one missing flow.
  dummy_AB_trade_matrix <- AB_data %>%
    IEATools::add_psut_matnames() %>%
    calc_bilateral_trade_df_gma() %>%
    dplyr::filter(Provenience != "B")

  MR_PSUT_bta <- AB_data %>%
    IEATools::add_psut_matnames() %>%
    transform_to_bta(bilateral_trade_df = dummy_AB_trade_matrix)

  # Nice one!
  expect_equal(MR_PSUT_bta %>%
                 dplyr::arrange(Country, Method, Energy.type, Last.stage, Year, Ledger.side, Flow.aggregation.point, Flow, Product, Unit, matnames),
               MR_PSUT_gma %>%
                 dplyr::arrange(Country, Method, Energy.type, Last.stage, Year, Ledger.side, Flow.aggregation.point, Flow, Product, Unit, matnames))

  # Then, try with a "fake" flow
  second_dummy_AB_trade_matrix <- AB_data %>%
    IEATools::add_psut_matnames() %>%
    calc_bilateral_trade_df_gma() %>%
    dplyr::filter(Provenience != "B") %>%
    tibble::add_row(
      Provenience = "A",
      Country = "A",
      Method = "PCM",
      Energy.type = "E",
      Last.stage = "Final",
      Year = 2018,
      Product = "Coke oven coke",
      Share_Exports_By_Product = 1
    )

  MR_PSUT_bta <- AB_data %>%
    IEATools::add_psut_matnames() %>%
    transform_to_bta(bilateral_trade_df = second_dummy_AB_trade_matrix)


  # Checking data frames are NOT the same
  expect_false(nrow(MR_PSUT_gma) == nrow(MR_PSUT_bta))
  expect_equal(nrow(MR_PSUT_bta) + 3,
               nrow(MR_PSUT_gma))

  # Further, checking particular values
  expect_equal(MR_PSUT_bta %>%
                 dplyr::filter(
                   Flow == "{A}_Residential",
                   Product == "{A}_Coke oven coke",
                   matnames == "Y"
                 ) %>%
                 magrittr::extract2("E.dot"),
               120)

  expect_equal(MR_PSUT_bta %>%
                 dplyr::filter(
                   Flow == "{A}_Road",
                   Product == "{A}_Coke oven coke",
                   matnames == "Y"
                 ) %>%
                 magrittr::extract2("E.dot"),
               80)

  expect_equal(MR_PSUT_bta %>%
                 dplyr::filter(
                   Flow == "{A}_Blast furnaces",
                   Product == "{A}_Coke oven coke",
                   matnames == "U_feed"
                 ) %>%
                 magrittr::extract2("E.dot"),
               -800)
})


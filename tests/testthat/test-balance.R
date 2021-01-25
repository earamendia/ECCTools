###########################################################
context("IEA energy balance")
###########################################################

test_that("calc_tidy_iea_df_balance works correctly for 2018 data", {
  Ebal_2018 <- IEATools::load_tidy_iea_df(IEATools::sample_iea_data_path(2018)) %>%
    calc_tidy_iea_df_balances_balancing()
  expect_false(all(Ebal_2018$balance_OK))
  expect_true(Ebal_2018 %>% dplyr::filter(Country == "GHA", Year == 1971, Product == "Aviation gasoline") %>% magrittr::extract2("balance_OK"))
  expect_false(Ebal_2018 %>% dplyr::filter(Country == "GHA", Year == 1971, Product == "Electricity") %>% magrittr::extract2("balance_OK"))
  expect_true(Ebal_2018 %>% dplyr::filter(Country == "ZAF", Year == 1971, Product == "Hydro") %>% magrittr::extract2("balance_OK"))
  expect_false(Ebal_2018 %>% dplyr::filter(Country == "ZAF", Year == 2000, Product == "Other bituminous coal") %>% magrittr::extract2("balance_OK"))
})


test_that("calc_tidy_iea_df_balance works correctly for 2019 data", {
  Ebal_2019 <- IEATools::load_tidy_iea_df(IEATools::sample_iea_data_path(2019)) %>%
    calc_tidy_iea_df_balances_balancing()
  expect_false(all(Ebal_2019$balance_OK))
  expect_true(Ebal_2019 %>% dplyr::filter(Country == "GHA", Year == 1971, Product == "Aviation gasoline") %>% magrittr::extract2("balance_OK"))
  expect_false(Ebal_2019 %>% dplyr::filter(Country == "GHA", Year == 1971, Product == "Electricity") %>% magrittr::extract2("balance_OK"))
  # This one was OK in the 2018 data, but is off by 1e-4 in the 2019 data.
  expect_false(Ebal_2019 %>% dplyr::filter(Country == "ZAF", Year == 1971, Product == "Hydro") %>% magrittr::extract2("balance_OK"))
  expect_false(Ebal_2019 %>% dplyr::filter(Country == "ZAF", Year == 2000, Product == "Other bituminous coal") %>% magrittr::extract2("balance_OK"))
})


test_that("add_balancing_vector works", {

  # Path to dummy AB data
  A_B_path <- system.file("A_B_data_full_2018_format.csv", package = "ECCTools")

  # Loading AB_data
  AB_data <- A_B_path %>%
    IEATools::load_tidy_iea_df()

  AB_tidy_data <- AB_data %>%
    specify_all_revisited()

  expect_true(AB_data %>%
                 IEATools::tidy_iea_df_balanced())

  AB_tidy_MR <- AB_data %>%
    add_psut_matnames_epsilon() %>%
    transform_to_gma()

  expect_false(AB_tidy_MR %>%
                 calc_tidy_iea_df_balances_balancing() %>%
                 magrittr::extract2("balance_OK") %>%
                 all())

  AB_tidy_MR_with_balancing <- AB_tidy_MR %>%
    add_balancing_vector()

  expect_true(AB_tidy_MR_with_balancing %>%
                 calc_tidy_iea_df_balances_balancing() %>%
                 magrittr::extract2("balance_OK") %>%
                 all())
})

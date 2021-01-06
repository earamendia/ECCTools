# This script test our dummy A-B countries example.

# Loading libraries
library(dplyr)
library(tidyr)
library(IEATools)
library(Recca)
library(matsbyname)
library(stringr)
library(glue)



#### Sourcing eroi_functions ####

#source("Scripts/eroi_functions.R")


#### Loading and preparing A_B dummy data  ####

# Loading data
A_B_path <- file.path("inst/A_B_data_full_2018_format_tp.csv")
# A_B_path <- file.path("Scripts/A_B_Test_TP_industries.csv")
# A_B_path <- file.path("Scripts/A_B_Test_p_e_supply.csv")

#A_B_path <- file.path("/home/manolo/Documents/Framadrive/PhD/y_EROI paper/Scripts/A_B_data_full_2018_format.csv")

A_B_path %>% iea_file_OK()

# Sorting out data with IEATools
# First, make tidy dataframe
AB_data <- A_B_path %>%
  load_tidy_iea_df()


# Testing here the specifications
test <- AB_data %>%
  specify_primary_production() %>%
  specify_production_to_resources() %>%
  gather_producer_autoproducer() %>%
  route_pumped_storage() %>%
  route_own_use_elect_chp_heat() %>%
  route_non_specified_eiou() %>%
  route_non_specified_tp()



AB_tidy_data <- AB_data %>%
  specify_all() %>%
  add_psut_matnames() # This is part of the prep_psut() function.



#### Testing route_own_use_elect_chp_heat() - LOAD inst/A_B_data_full_2018_format_tp.csv file. ####
AB_data <- A_B_path %>%
  load_tidy_iea_df()


# Test here with everything
test <- AB_data %>%
  specify_primary_production() %>%
  specify_production_to_resources() %>%
  gather_producer_autoproducer() %>%
  route_pumped_storage() %>%
  route_own_use_elect_chp_heat()

# Now, test with no autoproducer / main industry in transf. processes; all "Own use" should go to "Main activity electricity plants"
AB_data_no_main_ind <- AB_data %>%
  filter(! Flow %in% c("Main activity producer electricity plants", "Main activity producer CHP plants", "Main activity producer heat plants",
                       "Autoproducer CHP plants", "Autoproducer heat plants", "Autoproducer electricity plants"))

test_no_main <- AB_data_no_main_ind %>%
  specify_primary_production() %>%
  specify_production_to_resources() %>%
  gather_producer_autoproducer() %>%
  #route_pumped_storage() %>% # Because otherwise that creates a "Main activity" just before and messes up everything.
  route_own_use_elect_chp_heat()
# Seems all good.

# Third case when only one is missing - say CHP
AB_data_third_test <- AB_data %>%
  filter(! Flow %in% c("Main activity producer CHP plants",
                       "Autoproducer CHP plants"))

test_third_test <- AB_data_third_test %>%
  specify_primary_production() %>%
  specify_production_to_resources() %>%
  gather_producer_autoproducer() %>%
  #route_pumped_storage() %>% # Because otherwise that creates a "Main activity" just before and messes up everything.
  route_own_use_elect_chp_heat()



# This is basically the code that will get data for the V matrix ready.
AB_supply_MR <- AB_tidy_data %>%
  filter(matnames == "V", ! str_detect(Flow, "Imports")) %>%
  mutate(
    Flow = paste0("{", Country, "}_", Flow),
    Product = paste0("{", Country, "}_", Product),
    Country = "World"
  ) %>%
  print()


# This is basically the code that will get data for the R matrix ready.
AB_resources_MR <- AB_tidy_data %>%
  filter(matnames == "R", ! str_detect(Flow, "Imports")) %>%
  mutate(
    Flow = paste0("{", Country, "}_", Flow),
    Product = paste0("{", Country, "}_", Product),
    Country = "World"
  ) %>%
  print()


# Now, code that will get data ready for the U matrix....

#### First, differentiate imported from non-imported products on the consumption side ####

# Total consumption by product, year, country
total_consumption_by_product <- AB_tidy_data %>%
  filter((matnames == "Y" | matnames == "U_feed" | matnames == "U_EIOU"), ! str_detect(Flow, "Exports")) %>% # There shouldn't be anymore exports where there are imports, now.
  mutate(
    E.dot = case_when(
      matnames == "U_feed" ~ - E.dot,
      matnames == "U_EIOU" ~ - E.dot,
      TRUE ~ E.dot
    )
  ) %>%
  group_by(Country, Method, Energy.type, Last.stage, Year, Product, Unit) %>%
  summarise(
    total_consumption = sum(E.dot)
  ) %>%
  print()

# Imports by product, year, country
imports <- AB_tidy_data %>%
  filter(str_detect(Flow, "Imports")) %>%
  rename(Imports = E.dot) %>%
  print()

# Share of imported products by product, year, country
share_imports_by_product <- total_consumption_by_product %>%
  left_join(imports, by = c("Country", "Method", "Energy.type", "Last.stage", "Year", "Product")) %>%
  select(-Ledger.side, -matnames, -Flow.aggregation.point, -Flow) %>%
  mutate(
    Share_Imports = case_when(
      is.na(Imports) ~ 0,
      !is.na(Imports) ~ Imports / total_consumption
    )
  ) %>%
  print()


# Specifying imported products in Y, U_EIOU, U_feed

defining_imported_products <- AB_tidy_data %>%
  filter((matnames == "Y" | matnames == "U_feed" | matnames == "U_EIOU"), ! str_detect(Flow, "Exports")) %>%
  left_join(
    share_imports_by_product, by = c("Country", "Year", "Product", "Method", "Energy.type", "Last.stage")
  ) %>%
  mutate(
    Domestic = E.dot * (1 - Share_Imports),
    Imported = E.dot * Share_Imports
  ) %>%
  select(-E.dot, -total_consumption, -Imports, -Share_Imports) %>%
  pivot_longer(cols = c("Domestic", "Imported"), names_to = "Origin", values_to = "E.dot") %>%
  # mutate(
  #   Product = case_when(
  #     Origin == "Imported" ~ paste0(Product, " [Imported]"),
  #     TRUE ~ Product
  #   )
  # ) %>%
  relocate(Origin, .after = Product) %>%
  filter(E.dot != 0) %>%
  print()


# Creating trade matrices data frame
total_exports <- AB_tidy_data %>%
  filter(str_detect(Flow, "Exports")) %>%
  mutate(
    E.dot = abs(E.dot)
  ) %>%
  group_by(Method, Energy.type, Last.stage, Year, Ledger.side, Flow.aggregation.point, Product) %>%
  summarise(
    Total_Exports = sum(E.dot)
  ) %>%
  print()


# This is not yet a matrix....
share_exports <- AB_tidy_data %>%
  filter(str_detect(Flow, "Exports")) %>%
  mutate(
    E.dot = abs(E.dot)
  ) %>%
  left_join(total_exports, by = c("Method", "Energy.type", "Last.stage", "Year", "Ledger.side", "Flow.aggregation.point", "Product")) %>%
  mutate(
    Share_Exports = E.dot / Total_Exports
  ) %>%
  rename(Provenience = Country) %>%
  print()


# So here we create a data frame equivalent to a matrix with n*n countries. Also A to A etc. Missing values are implicitly 0 shares.
share_exports_by_origin_destination <- share_exports %>%
  expand_grid(Country = AB_tidy_data %>%
                expand(Country) %>%
                pull()) %>%
  mutate(
    Share_Exports = case_when(
      Provenience == Country ~ 0,
      TRUE ~ Share_Exports
    )
  ) %>%
  relocate(Country, .after = Provenience) %>%
  select(- Ledger.side, -Flow.aggregation.point, -Flow, -Unit, -E.dot, -matnames, -Total_Exports) %>%
  print()


testing <- share_exports_by_origin_destination %>%
  add_row(Provenience = "C",
          Country = "B",
          Method = "PCM",
          Energy.type = "E",
          Last.stage = "Final",
          Year = 2018,
          Product = "Natural gas [of Oil and gas extraction]",
          Share_Exports = 0.5) %>%
  print()

# Testing the trade matrix

share_exports_by_origin_destination %>%
  group_by(Country, Method, Energy.type, Last.stage, Year, Product) %>%
  summarise(sum_shares = sum(Share_Exports)) %>%
  print()
# Should be either 1 or 0;
# 1 as soon as the country imports the product;
# 0 as soon as the country doesn't import the product;
# Obviously in the given year.


# Third, specifying all consumption flows - actually specifying the country of origin!
AB_domestic_consumption_MR <- defining_imported_products %>%
  filter(Origin == "Domestic") %>%
  mutate(
    Flow = paste0("{", Country, "}_", Flow),
    Product = paste0("{", Country, "}_", Product),
    Country = "World"
  ) %>%
  select(-Origin) %>%
  print()

# If we write here testing instead of share_exports_by_origin_destination, we have a decent test.
AB_imported_consumption_MR <- defining_imported_products %>%
  filter(Origin == "Imported") %>%
  left_join(share_exports_by_origin_destination, by = c("Country", "Method", "Energy.type", "Last.stage", "Year", "Product")) %>%
  relocate(Provenience, .before = Country) %>%
  mutate(
    E.dot = E.dot * Share_Exports,
    Flow = paste0("{", Country, "}_", Flow),
    Product = paste0("{", Provenience, "}_", Product)
  ) %>%
  select(-Provenience, -Share_Exports, -Origin) %>%
  print()


# Join both data frames
AB_consumption_MR <- bind_rows(AB_domestic_consumption_MR, AB_imported_consumption_MR)




AB_tidy_data_bis <- AB_tidy_data %>%
  add_row(Country = c("A", "A"),
          Method = c("PCM", "PCM"),
          Energy.type = c("E", "E"),
          Last.stage = c("Final", "Final"),
          Year = c(2018, 2018),
          Ledger.side = c("Consumption", "Consumption"),
          Flow.aggregation.point = c("Industry", "Total primary energy supply"),
          Flow = c("Iron and steel", "Imports [of Stinking Coal]"),
          Product = c("Stinking Coal", "Stinking Coal"),
          Unit = c("ktoe", "ktoe"),
          E.dot = c(1000, 900),
          matnames = c("Y", "V")) %>%
  print()


test <- AB_tidy_data_bis %>% specify_MR_Y_U_gma()


# If we write here testing instead of share_exports_by_origin_destination, we have a decent test.
AB_imported_consumption_MR <- defining_imported_products_added_row %>%
  filter(Origin == "Imported") %>%
  left_join(share_exports, by = c("Method", "Energy.type", "Last.stage", "Year", "Product")) %>%
  relocate(Provenience, .before = Country) %>%
  print()

mutate(
  E.dot = E.dot * Share_Exports_From_Func,
  Flow = paste0("{", Country, "}_", Flow),
  Product = paste0("{", Provenience, "}_", Product)
) %>%
  select(-Provenience, -Share_Exports_From_Func, -Origin, -Unit.x, -Unit.y) %>%
  print()


# Join both data frames
AB_consumption_MR <- bind_rows(AB_domestic_consumption_MR, AB_imported_consumption_MR)






# Second, checking balances
Balances <- AB_data %>%
  calc_tidy_iea_df_balances()

Balances %>%
  print()

Balances %>%
  tidy_iea_df_balanced()

AB_data %>%
  fix_tidy_iea_df_balances()


# Third, forming PSUT matrices
AB_PSUT <- AB_data %>%
  specify_all() %>%
  prep_psut() %>%
  mutate(
   U = matsbyname::sum_byname(U_EIOU, U_feed)
  )

AB_PSUT %>% glimpse()


AB_PSUT_Tidy <- AB_PSUT %>%
  pivot_longer(cols = -c("Country", "Method", "Energy.type", "Last.stage", "Year"), names_to = "matrix.name", values_to = "matrix")


 # Inspecting V matrix
AB_PSUT[["V"]][[1]] %>% View()
AB_PSUT[["U"]][[1]] %>% View()
AB_PSUT[["Y"]][[1]] %>% View()
# Why are not products specified by producing industry?


#### Trying using the Recca functions on dummy example ####
# Defining matrices
R_A <- (AB_PSUT_Tidy %>%
  filter(Country == "A", matrix.name == "R"))[["matrix"]][[1]]

U_EIOU_A <- (AB_PSUT_Tidy %>%
          filter(Country == "A", matrix.name == "U_EIOU"))[["matrix"]][[1]]

U_feed_A <- (AB_PSUT_Tidy %>%
          filter(Country == "A", matrix.name == "U_excl_EIOU"))[["matrix"]][[1]]

U_A <- sum_byname(U_EIOU_A, U_feed_A)

V_A <- (AB_PSUT_Tidy %>%
          filter(Country == "A", matrix.name == "V"))[["matrix"]][[1]]

Y_A <- (AB_PSUT_Tidy %>%
          filter(Country == "A", matrix.name == "Y"))[["matrix"]][[1]]

S_units_A <- (AB_PSUT_Tidy %>%
                filter(Country == "A", matrix.name == "S_units"))[["matrix"]][[1]]


# From individual matrices
IO_list <- calc_io_mats(U = U_A, V = V_A, Y = Y_A, S_units = S_units_A)
IO_list_from_list <- calc_io_mats(list(U = U_A, V = V_A, Y = Y_A, S_units = S_units_A))


# From matsindf-style dataframe
IO_df <- AB_PSUT %>% calc_io_mats()
#IO_list_bis <- calc_io_mats(.sutdata = AB_PSUT_Tidy %>% filter(Country == "A"))

IO_df_adding_extension <- IO_df %>% calc_E_EIOU()

# Checking content

View(IO_df_adding_extension[["E_EIOU"]][[1]])
View(IO_df_adding_extension[["e_EIOU"]][[1]])





U_EIOU_A <- (AB_PSUT_Tidy %>%
               filter(Country == "A", matrix.name == "U_EIOU"))[["matrix"]][[1]]
g_A <- IO_mats[["g"]][[1]]

E_EIOU_mat <- matsbyname::matrixproduct_byname(U_EIOU_A, g_A %>% matsbyname::hatinv_byname())
e_EIOU_vec <- matsbyname::rowsums_byname(E_EIOU_mat)

list(e_EIOU_vec) %>% magrittr::set_names(e_EIOU)


calc_E_EIOU(g = )


# Okay seems to work well.
# And also here with the Recca package example everything works fine.

View(UKEnergy2000mats)
View(UKEnergy2000tidy)


testing <- UKEnergy2000tidy %>%
  prep_psut() %>%
  mutate(
    U = matsbyname::sum_byname(U_EIOU, U_feed)
  ) %>%
  filter(Last.stage == "Final")

UK_IO_mats <- testing %>%
  calc_io_mats()

View(UK_IO_mats[["U_feed"]][[1]])
View(UK_IO_mats[["U_EIOU"]][[1]])
View(UK_IO_mats[["g"]][[1]])
View(UK_IO_mats[["D"]][[1]])
View(UK_IO_mats[["C"]][[1]])


write.csv(UK_IO_mats[["U_feed"]][[1]], file = "Scripts/UK_2000_Example/UK_2000_U_feed.csv")
write.csv(UK_IO_mats[["U_EIOU"]][[1]], file = "Scripts/UK_2000_Example/UK_2000_U_EIOU.csv")
write.csv(UK_IO_mats[["g"]][[1]], file = "Scripts/UK_2000_Example/UK_2000_g.csv")
write.csv(UK_IO_mats[["D"]][[1]], file = "Scripts/UK_2000_Example/UK_2000_D.csv")
write.csv(UK_IO_mats[["C"]][[1]], file = "Scripts/UK_2000_Example/UK_2000_C.csv")
write.csv(UK_IO_mats[["Y"]][[1]], file = "Scripts/UK_2000_Example/UK_2000_Y.csv")
write.csv(UK_IO_mats[["U"]][[1]], file = "Scripts/UK_2000_Example/UK_2000_U.csv")



View(UK_IO_mats[["Z_feed"]][[1]])
View(UK_IO_mats[["A_feed"]][[1]])
View(UK_IO_mats[["L_pxp_feed"]][[1]])
View(UK_IO_mats[["L_ixp_feed"]][[1]])



mats <- UKEnergy2000mats %>%
  tidyr::spread(key = matrix.name, value = matrix) %>%
  # Put rows in a natural order
  dplyr::mutate(
    Last.stage = factor(Last.stage, levels = c("Final", "Useful", "Services")),
    Energy.type = factor(Energy.type, levels = c("E", "X"))
  ) %>%
  dplyr::arrange(Last.stage, Energy.type)

R <- mats$R[[1]]
U <- mats$U[[1]]
V <- mats$V[[1]]
Y <- mats$Y[[1]]
S_units <- mats$S_units[[1]]

IO_list <- calc_io_mats(U = U, V = V, Y = Y, S_units = S_units)

#IO_df <- mats %>% calc_io_mats()


# Testing EROI function
AB_erois <- AB_PSUT %>%
  calc_io_mats() %>%
  calc_E_EIOU() %>%
  calc_erois()

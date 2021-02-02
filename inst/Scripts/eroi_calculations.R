
# Libraries
library(dplyr)
library(tidyr)
library(stringr)
library(IEATools)
library(Recca)


# Loading data

# Path to data
IEA_path <- file.path("/home/manolo/Documents/Extended-Energy-Balances-2018/", "IEA Extended Energy Balances 2019.csv")


# List regions to remove

list_regions_to_remove <- c(IEATools::aggregation_regions,
                            memo_uganda = "Memo: Uganda",
                            world = "World",
                            world_aviation_bunkers = "World aviation bunkers",
                            world_marine_bunkers = "World marine bunkers")

# Loading IEA data, removing aggregation flows, and specifying flows.
IEA_data <- IEA_path %>%
  load_tidy_iea_df() %>%
  dplyr::filter(dplyr::between(Year, 1971, 2017)) %>% #Year >= 1971 & Year <= 2017) %>%
  fix_tidy_iea_df_balances() %>%
  remove_agg_regions(agg_regions = list_regions_to_remove) %>% # We can provide our own list here!
  specify_all()


# Reading aggregation table
aggregation_table_exiobase <- read_aggregation_region_table(file_path = "inst/aggregation_table_exiobase_iea_2019.xlsx")


# Doing the aggregation
IEA_data_aggregated <- IEA_data %>%
  aggregate_regions(aggregation_table = aggregation_table_exiobase, net_trade = TRUE)


# Check if balanced
IEA_data_aggregated %>%
  tidy_iea_df_balanced()


#### FIRST TRANSFORMATION  - DOMESTIC TECHNOLOGY ASSUMPTION ####

IEA_data_aggregated_dta_1971 <- IEA_data_aggregated %>%
  IEATools::add_psut_matnames() %>%
  dplyr::filter(Year == 1971) %>%
  transform_to_dta()

psut_dta_1971 <- IEA_data_aggregated_dta_1971 %>%
  prep_psut()

# Note: if I filter BEFORE add_psut_matnames() I get an error!
IEA_data_aggregated_dta_1971 <- IEA_data_aggregated %>%
  dplyr::filter(Year == 1971) %>%
  IEATools::add_psut_matnames() %>%
  transform_to_dta()


#### SECOND TRANSFORMATION  - GLOBAL MARKET ASSUMPTION ####
IEA_data_aggregated_gma_1971 <- IEA_data_aggregated %>%
  IEATools::add_psut_matnames() %>%
  dplyr::filter(Year == 1971) %>%
  transform_to_gma()

psut_mr_gma_1971 <- IEA_data_aggregated_gma_1971 %>%
  prep_psut()


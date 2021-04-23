# This script creates constants and saves them in the right locations.
# If there are any changes to these constants,
# source this script before building the package.


#
# Reading and saving default AB data
#

A_B_path <- system.file("extdata/A_B_data_full_2018_format.csv", package = "ECCTools")

tidy_AB_data <- A_B_path %>%
  IEATools::load_tidy_iea_df() %>%
  IEATools::specify_all()

usethis::use_data(tidy_AB_data, overwrite = TRUE)

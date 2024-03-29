% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transforms_to_bta.R
\name{check_bilateral_trade_df}
\alias{check_bilateral_trade_df}
\title{Checks bilateral trade data}
\usage{
check_bilateral_trade_df(
  .bilateral_trade_df,
  country = IEATools::iea_cols$country,
  provenience = "Provenience",
  method = IEATools::iea_cols$method,
  energy_type = IEATools::iea_cols$energy_type,
  last_stage = IEATools::iea_cols$last_stage,
  year = IEATools::iea_cols$year,
  product = IEATools::iea_cols$product,
  e_dot = IEATools::iea_cols$e_dot,
  share_exports_by_product = "Share_Exports_By_Product",
  .sum_share_exports = ".sum_share_exports"
)
}
\arguments{
\item{.bilateral_trade_df}{The bilateral trade data frame to be checked.}

\item{country}{The name of the country column.
Default is \code{IEATools::iea_cols$country}.}

\item{provenience}{The name of the country of provenience (i.e. exporting country).
Default is "Provenience".}

\item{method}{The name of the Method column.
Default is \code{IEATools::iea_cols$method}.}

\item{energy_type}{The name of the energy type column.
Default is \code{IEATools::iea_cols$energy_type}.}

\item{last_stage}{The name of the last stage column.
Default is \code{IEATools::iea_cols$last_stage}.}

\item{year}{The name of the year column.
Default is \code{year = IEATools::iea_cols$year}.}

\item{product}{The name of the product column.
Default is \code{IEATools::iea_cols$product}.}

\item{e_dot}{The name of the E.dot column.
Default is \code{IEATools::iea_cols$e_dot}.}

\item{share_exports_by_product}{The name of the share of exports by product column.
Default is "Share_Exports_By_Product".}

\item{.sum_share_exports}{The name of the temporary sum shares of exports by product column.
Default is ".sum_share_exports".}
}
\value{
Returns TRUE if the sum of the shares of exported products, by country of provenience and by product, is 1.
Returns an error if otherwise.
}
\description{
This function checks the bilateral trade data before it is provided to the transform_to_bta() function.
It returns TRUE if the sum of the shares of exported products, by country of provenience and by product, is 1.
Otherwise, it returns an error.
}
\examples{
A_B_path <- system.file("extdata/A_B_data_full_2018_format.csv", package = "ECCTools")
AB_data <- A_B_path \%>\%
 IEATools::load_tidy_iea_df() \%>\%
 IEATools::specify_all()
bilateral_trade_df_gma <- AB_data \%>\%
 IEATools::add_psut_matnames() \%>\%
 calc_bilateral_trade_df_gma()
check_bilateral_trade_df(bilateral_trade_df_gma)
}

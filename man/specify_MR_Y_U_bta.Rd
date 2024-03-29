% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transforms_to_bta.R
\name{specify_MR_Y_U_bta}
\alias{specify_MR_Y_U_bta}
\title{Specifies Multi-Regional final demand (Y) and use (U) matrices with Bilateral Trade Assumption}
\usage{
specify_MR_Y_U_bta(
  .tidy_iea_df,
  bilateral_trade_df = calc_bilateral_trade_df_gma(.tidy_iea_df),
  flow = IEATools::iea_cols$flow,
  product = IEATools::iea_cols$product,
  year = IEATools::iea_cols$year,
  method = IEATools::iea_cols$method,
  energy_type = IEATools::iea_cols$energy_type,
  last_stage = IEATools::iea_cols$last_stage,
  e_dot = IEATools::iea_cols$e_dot,
  country = IEATools::iea_cols$country,
  unit = IEATools::iea_cols$unit,
  aggregate_country_name = "World",
  provenience = "Provenience",
  origin = "Origin",
  domestic = "Domestic",
  imported = "Imported",
  Share_Exports_By_Product = "Share_Exports_By_Product",
  Producing_Country = "Producing_Country",
  Share_Global_Production_By_Product = "Share_Global_Production_By_Product"
)
}
\arguments{
\item{.tidy_iea_df}{The \code{.tidy_iea_df} from which the Multi-Regional Y and U matrices needs to be created.}

\item{bilateral_trade_df}{The bilateral trade data to be used to specify Y and U matrices flows.
Default is bilateral trade data corresponding to the Global Market Assumption, calculated as \code{calc_bilateral_trade_df_gma(.tidy_iea_df)}.}

\item{flow, product, year, method, energy_type, last_stage, e_dot, country, unit}{See \code{IEATools::iea_cols}.}

\item{aggregate_country_name}{The name of the new region that gathers all flows of the \code{.tidy_iea_df}.
Default is "World".}

\item{provenience}{The name of the temporary column that specifies the origin of a given flow.
Default is "Provenience".}

\item{origin}{The name of the column specifying whether a given flow comes refers to a domestic or imported product.
Default is "Origin".}

\item{domestic}{The string that indicates that the product is of domestic origin in the new origin column.
Default is "Domestic".}

\item{imported}{The string that indicates that the product is of imported origin in the new origin column.
Default is "Imported".}

\item{Share_Exports_By_Product}{The name of a temporary column that contains the share of global exports by country, for each product.
Default is "Share_Exports_By_Product".}

\item{Producing_Country}{The name of a temporary column that contains the provenance country for a given flow, i.e. the exporting country of a given flow.
Default is "Producing_Country".}

\item{Share_Global_Production_By_Product}{The name if a temporary column that contains the share of global product by country, for each product.
Default is "Share_Global_Production_By_Product".}
}
\value{
A \code{.tidy_iea_df} with flows corresponding to the Y, U_feed, U_eiou, and B matrices are specified.
}
\description{
This function specifies flows belonging to the Y, U_feed, U_eiou, and B (only for those flows akin to final demand) matrices,
according to the Bilateral Trade Assumption. See details for more explanations.
}
\details{
First, each flow is separated into a flow of domestic product and imported product, using the \code{specify_imported_products()} function.
Then, the bilateral trade data passed as \code{bilateral_trade_df} argument, is used to specify the flows that are imported.

Only flows belonging to the Y, U_feed, U_eiou, and B matrices are returned.
Note that matrices names need to be added first, most likely using the \code{IEATools::add_psut_matnames()} function.
}
\examples{
tidy_AB_data \%>\%
IEATools::add_psut_matnames() \%>\%
specify_MR_Y_U_bta() \%>\%
print()
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/specify_elect_heat.R
\name{specify_other_elec_heat_production}
\alias{specify_other_elec_heat_production}
\title{Specifies remaining electricity and heat production flows}
\usage{
specify_other_elec_heat_production(
  .tidy_iea_df,
  flow_aggregation_point = IEATools::iea_cols$flow_aggregation_point,
  flow = IEATools::iea_cols$flow,
  e_dot = IEATools::iea_cols$e_dot,
  product = IEATools::iea_cols$product,
  transformation_processes = IEATools::aggregation_flows$transformation_processes,
  negzeropos = ".negzeropos"
)
}
\arguments{
\item{.tidy_iea_df}{The \code{.tidy_iea_df} for which non-specified electricity and heat production flows need to be specified.}

\item{flow_aggregation_point, flow, e_dot, product}{See \code{IEATools::iea_cols}.}

\item{transformation_processes}{The name of the transformation processes in the flow aggregation point column.
Default is IEATools::aggregation_flows$transformation_processes.}

\item{negzeropos}{Temporary column name. Default is ".netzeropos".}
}
\value{
Returns a tidy data frame with all electricity and heat production flows specified (products are specified).
}
\description{
This function specifies electricity and heat production flows (i.e. positive flows in the transformation processes
flow aggregation point) that are not yet specified by calling the energy product
respectively "Electricity \code{[}from Other processes\verb{]} and "Heat \code{[}from Other processes\verb{]}.
}
\examples{
A_B_path <- system.file("extdata/A_B_data_full_2018_format.csv", package = "ECCTools")
IEATools::load_tidy_iea_df(A_B_path) \%>\%
IEATools::specify_all() \%>\%
tibble::add_row(
Country = "A",
Method = "PCM",
Energy.type = "E",
Last.stage = "Final",
Year = 2018,
Ledger.side = "Supply",
Unit = "ktoe",
Flow.aggregation.point = "Transformation processes",
Flow = "Heat pumps",
Product = "Heat",
E.dot = 82) \%>\%
specify_other_elec_heat_production()
}

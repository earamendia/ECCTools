% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cleans.R
\name{add_balancing_vector}
\alias{add_balancing_vector}
\title{Adds a balancing vector to the Balancing matrix}
\usage{
add_balancing_vector(
  .tidy_iea_df,
  country = IEATools::iea_cols$country,
  method = IEATools::iea_cols$method,
  energy_type = IEATools::iea_cols$energy_type,
  last_stage = IEATools::iea_cols$last_stage,
  year = IEATools::iea_cols$year,
  ledger_side = IEATools::iea_cols$ledger_side,
  flow_aggregation_point = IEATools::iea_cols$flow_aggregation_point,
  flow = IEATools::iea_cols$flow,
  product = IEATools::iea_cols$product,
  e_dot = IEATools::iea_cols$e_dot,
  unit = IEATools::iea_cols$unit,
  supply = IEATools::ledger_sides$supply,
  consumption = IEATools::ledger_sides$consumption,
  matnames = IEATools::mat_meta_cols$matnames,
  balancing = "balancing",
  supply_sum = "supply_sum",
  consumption_sum = "consumption_sum",
  supply_minus_consumption = "supply_minus_consumption",
  balance_OK = "balance_OK",
  err = "err",
  balancing_matrix = "B"
)
}
\arguments{
\item{.tidy_iea_df}{The \code{.tidy_iea_df} for which a balancing vector needs being added.}

\item{country, method, energy_type, last_stage, year, ledger_side, flow_aggregation_point, flow, product, e_dot, unit}{See \code{IEATools::iea_cols}.}

\item{supply}{The name of the supply ledger side.
Default is \code{IEATools::ledger_sides$supply}.}

\item{consumption}{The name of the consumption ledger side.
Default is \code{IEATools::ledger_sides$consumption}.}

\item{matnames}{The column name of the column having matrices names.
Default is \code{IEATools::mat_meta_cols$matnames}.}

\item{balancing}{The name of the balancing ledger side.
Default is "balancing".}

\item{supply_sum}{The name of the supply sum column (total supply for each product).
Default is "supply_sum".}

\item{consumption_sum}{The name of the consumption sum column (total consumption for each product).
Default is "consumption_sum".}

\item{supply_minus_consumption}{The name of the column calculating the difference between total supply and consumption for each product.
Default is "supply_minus_consumption".}

\item{balance_OK}{A boolean stating whether flows for each product are balanced or not.
Default is "balance_OK".}

\item{err}{The numerical error observed when calculating the balance. Balanced flows will return 0
Default is "err".}

\item{balancing_matrix}{The name of the Balancing matrix.
Default is "B".}
}
\value{
The \code{.tidy_iea_df} with balancing flows added when needed.
}
\description{
This function adds a balancing vector to the \code{.tidy_iea_df}
in the Balancing matrix when the data frame is not balanced.
}
\details{
The balancing flow is added by adding a single flow for each product
for which consumption and supply are not balanced. So, a single vector is added,
but a few flows may be added, depending the number of products for which flows do not balance.
The ledger side of balancing flows is "balancing", as these are neither actual
supply nor consumption flows, but unbalances that are somehow present in IEA data.
Note: one needs to add the column containing matrices names first,
most likely using the \code{IEATools::add_psut_matnames()} function.
}
\examples{
# When we build the MR-PSUT with the AB example, we get unbalanced products.
# Note that this is done on purpose to reflect reality of IEA data.
tidy_AB_data \%>\%
IEATools::add_psut_matnames() \%>\%
 transform_to_gma() \%>\%
 IEATools::calc_tidy_iea_df_balances() \%>\%
 dplyr::filter(balance_OK == FALSE) \%>\%
 print()
# Now, if we add the balancing vector, we obtain balanced flows:
tidy_AB_data \%>\%
IEATools::add_psut_matnames() \%>\%
 transform_to_gma() \%>\%
 add_balancing_vector() \%>\%
 IEATools::calc_tidy_iea_df_balances() \%>\%
 dplyr::filter(balance_OK == FALSE) \%>\%
 print()
# Let's have a look to balancing flows, which are all ascribed to the Balancing matrix:
tidy_AB_data \%>\%
IEATools::add_psut_matnames() \%>\%
 transform_to_gma() \%>\%
 add_balancing_vector() \%>\%
 dplyr::filter(matnames == "B") \%>\%
 print()
}

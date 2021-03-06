


#' Specify primary production industries
#'
#' The IEA extended energy balances include
#' some `Flow`s that identify `Energy industry own use` (EIOU),
#' the consumption of energy by energy-producing industries.
#' But some primary production industries that receive EIOU do not produce anything.
#' For example, `Coal mines` receive electricity
#' but there are no `Coal mines` that produce coal.
#' Rather, the generic `Production` industry produces coal.
#' This function solves that problem by
#' replacing the generic `Production` industry with
#' specific industries.
#'
#' By default, the following changes are made to `.tidy_iea_df`:
#'
#' 1. Energy industry own use for `Liquefaction (LNG) / regasification plants` is
#'    reassigned to `Oil and gas extraction`.
#' 2. The `Production` industry for `coal_and_coal_products`
#'    is replaced by `Coal mines`.
#' 3. A `Resources [of Coal]` industry is created which becomes the
#'    source of all primary `coal_and_coal_products` in the energy conversion chain.
#' 4. Flows from `Resources [of Coal]` to `Coal mines` are added.
#' 5. The `Coal mines (energy)` EIOU `Flow` is replaced by `Coal mines`.
#' 6. The `Production` industry for `oil_and_oil_products` and `Natural gas`
#'    is replaced by `Oil and gas extraction`.
#' 7. A `Resources [of Oil and natural gas]` industry is created which becomes the
#'    source of all primary `oil_and_oil_products` and `Natural gas` in the energy conversion chain.
#' 8. Flows from `Resources [of Oil and natural gas]` to `Oil and gas extraction` are added.
#' 9. The `Oil and gas extraction (energy)` EIOU `Flow` is replaced by `Oil and gas extraction`.
#'
#' Users can specify other changes by adjusting the default argument values.
#'
#' Be sure to call this function _after_ calling `augment_iea_df()` or
#' `load_tidy_iea_df()`.
#'
#' @param .tidy_iea_df an IEA data frame whose columns have been renamed by `rename_iea_df_cols()`
#' @param eiou_destinations a vector of destinations for EIOU for primary production of coal and coal products and oil and natural gas.
#'        Default is `c("Coal mines", "Oil and gas extraction")`.
#' @param liquefaction_regas a string identifying liquefaction and regasification plants. Default is "Liquefaction (LNG) / regasification plants".
#' @param liquefaction_regas_reassign a string identifying the industry to which EIOU into `liquefaction_regas` will be reassigned.
#'        Default is "`Oil and gas extraction`".
#' @param production_products a list of products for which we want to specify primary industries.
#'        Default is `list(primary_coal_products, c(primary_oil_products, "Natural gas"))`.
#' @param production_products_short_names a vector of short names for primary industries.
#'        Default is `c("Coal", "Oil and natural gas")`.
#' @param flow_aggregation_point the name of the flow aggregation point column in `.tidy_iea_df`.
#'        Default is "`Flow.aggregation.point`".
#' @param eiou a string identifying energy industry own use in the `Flow.aggregation.point` column of `.tidy_iea_df`.
#'        Default is "`Energy industry own use`".
#' @param transformation_processes a string identifying transformation processes in the flow column of `.tidy_iea_df`.
#'        Default is "`Transformation processes`".
#' @param ledger_side,flow See `IEATools::iea_cols`.
#' @param consumption See `IEATools::ledger_sides`.
#' @param resources a string identifying resource industries to be added to `.tidy_iea_df`.
#'        Default is "`Resources`".
#' @param production a string identifying production in the flow column. Default is "`Production`".
#' @param e_dot the name of the energy column in `.tidy_iea_df`. Default is "`E.dot`".
#' @param product the name of the product column in `.tidy_iea_df`.  Default is "`Product`".
#' @param notation a list of specification notations. Default is `IEATools::of_notation`.
#'
#' @return `.tidy_iea_df` with adjusted production information for primary energy
#'         for both coal and coal products and oil and gas extraction
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' load_tidy_iea_df() %>%
#'   specify_primary_production() %>%
#'   add_psut_matnames() %>%
#'   filter(Flow %in% c("Resources (Coal)", "Coal mines")) %>%
#'   select(-Method, -Last.stage, -Ledger.side, -Unit)
#' # EIOU by "Liquefaction (LNG) / regasification plants" is reassigned to "Oil and gas extraction"
#' data.frame(
#'   Flow.aggregation.point = c("Energy industry own use"),
#'   Flow = c("Liquefaction (LNG) / regasification plants"),
#'   Product = c("Natural gas"),
#'   E.dot = c(-42),
#'   stringsAsFactors = FALSE
#' ) %>%
#'   specify_primary_production()
specify_primary_production <- function(.tidy_iea_df,
                                       eiou_destinations = c("Coal mines", "Oil and gas extraction"),
                                       liquefaction_regas = "Liquefaction (LNG) / regasification plants",
                                       liquefaction_regas_reassign = "Oil and gas extraction",
                                       production_products = list(IEATools::primary_coal_products,
                                                                  c(IEATools::primary_oil_products, natural_gas = "Natural gas")),
                                       production_products_short_names = c("Coal", "Oil and natural gas"),
                                       flow_aggregation_point = "Flow.aggregation.point",
                                       eiou = "Energy industry own use",
                                       transformation_processes = "Transformation processes",
                                       flow = "Flow",
                                       ledger_side = IEATools::iea_cols$ledger_side,
                                       resources = "Resources",
                                       production = IEATools::tpes_flows$production,
                                       consumption = IEATools::ledger_sides$consumption,
                                       e_dot = "E.dot",
                                       product = "Product",
                                       notation = IEATools::of_notation){
  specify_primary_func <- function(.tidf, eiou_dest, prod_prods, prod_short_name){
    # Convert from the Production industry to Resources (prod_short_name)
    # For example, Flow = Production, Product = Anthracite becomes Flow = Resources (Coal), Product = Anthracite
    res_name <- resources
    end_string <- paste0(notation[["suff_start"]], prod_short_name, notation[["suff_end"]])
    if (!endsWith(resources, end_string)) {
      res_name <- matsbyname::paste_pref_suff(pref = res_name, suff = prod_short_name, notation = notation)
    }
    # Replace Production with res_name in Production rows
    .tidf <- .tidf %>%
      dplyr::mutate(
        "{flow}" := dplyr::case_when(
          .data[[flow]] == production & .data[[product]] %in% prod_prods ~ res_name,
          TRUE ~ .data[[flow]]
        )
      )


    # Previously, I was checking whether EIOU was present before renaming
    # the production products.
    # But I should have been doing that for EVERY product.
    # Thus, I am commenting this code that relied on the EIOU check
    # on 25 Aug 2020.
    # The commented code below can be deleted in due course,
    # say Nov 2020.
    # ---MKH

    # EIOU <- .tidf %>%
    #   dplyr::filter(!!as.name(flow_aggregation_point) == eiou &
    #                   !!as.name(flow) == eiou_dest)
    # if (nrow(EIOU) > 0) {
    #   # We have EIOU rows, so we have more work to do.
    #
    #   # Find the places where the Production energy is consumed,
    #   # for example, Hard coal (if no detail).
    #   # These pieces of consumed energy need to be renamed
    #   # to "product (eiou_dest)"
    #   # i.e., the product is produced by (is from) eiou_dest.
    #   .tidf <- .tidf %>%
    #     dplyr::mutate(
    #       "{product}" := dplyr::case_when(
    #         .data[[product]] %in% prod_prods &
    #           !startsWith(.data[[flow]], resources) ~ matsbyname::paste_pref_suff(pref = .data[[product]], suff = eiou_dest, notation = notation),
    #         TRUE ~ .data[[product]]
    #       )
    #     )
    #
    #   # Find rows of production of prods
    #   Resource_rows <- .tidf %>%
    #     dplyr::filter(.data[[flow]] == res_name & .data[[product]] %in% prod_prods)
    #   # Make rows for input of prod into eiou_dest
    #   Input <- Resource_rows %>%
    #     dplyr::mutate(
    #       "{flow_aggregation_point}" := transformation_processes,
    #       "{flow}" := eiou_dest,
    #       # Convert to an input (negative)
    #       "{e_dot}" := -.data[[e_dot]]
    #     )
    #   # Make rows for production of prod by eiou_dest
    #   Output <- Input %>%
    #     dplyr::mutate(
    #       # Convert the Product to the specified product, i.e., product (eiou_dest)
    #       "{product}" := matsbyname::paste_pref_suff(pref = .data[[product]], suff = .data[[flow]], notation = notation),
    #       "{e_dot}" := -.data[[e_dot]]
    #     )
    #
    #   # Put it all together
    #   .tidf <- .tidf %>%
    #     # Add rows for additional flow from Resources to the EIOU industry to .tidy_iea_df
    #     dplyr::bind_rows(Input, Output)
    # }

    # Find the places where the Production energy is consumed,
    # for example, Hard coal (if no detail).
    # These pieces of consumed energy need to be renamed
    # according to notation.
    .tidf <- .tidf %>%
      dplyr::mutate(
        "{product}" := dplyr::case_when(
          .data[[product]] %in% prod_prods &
            !startsWith(.data[[flow]], resources) ~ matsbyname::paste_pref_suff(pref = .data[[product]], suff = eiou_dest, notation = notation),
          TRUE ~ .data[[product]]
        )
      )

    # Find rows of production of prods
    Resource_rows <- .tidf %>%
      dplyr::filter(.data[[flow]] == res_name & .data[[product]] %in% prod_prods)
    # Make rows for input of prod into eiou_dest
    Input <- Resource_rows %>%
      dplyr::mutate(
        "{flow_aggregation_point}" := transformation_processes,
        "{flow}" := eiou_dest,
        # Convert to an input (negative)
        "{e_dot}" := -.data[[e_dot]]
      )
    # Make rows for production of prod by eiou_dest
    Output <- Input %>%
      dplyr::mutate(
        # Convert the Product to the specified product, i.e., product (eiou_dest)
        "{product}" := matsbyname::paste_pref_suff(pref = .data[[product]], suff = .data[[flow]], notation = notation),
        "{e_dot}" := -.data[[e_dot]]
      )

    # Put it all together
    .tidf <- .tidf %>%
      # Add rows for additional flow from Resources to the EIOU industry to .tidy_iea_df
      dplyr::bind_rows(Input, Output)

    return(.tidf)
  }

  # The first task is to reassign EIOU tagged as "Liquefaction (LNG) / regasification plants" to
  # the Oil and gas extraction sector.
  .tidy_iea_df <- .tidy_iea_df %>%
    dplyr::mutate(
      !!as.name(flow) := dplyr::case_when(
        !!as.name(flow) == liquefaction_regas ~ liquefaction_regas_reassign,
        TRUE ~ !!as.name(flow)
      )
    ) %>%
    # After reassigning, we may have multiple rows of liquefaction_regas_reassign,
    # so we need to sum those rows.
    matsindf::group_by_everything_except(e_dot) %>%
    dplyr::summarise(!!as.name(e_dot) := sum(!!as.name(e_dot))) %>%
    dplyr::ungroup()

  # Now specify all primary production
  for (i in 1:length(eiou_destinations)) {
    .tidy_iea_df <- specify_primary_func(.tidf = .tidy_iea_df,
                                         eiou_dest = eiou_destinations[[i]],
                                         prod_prods = production_products[[i]],
                                         prod_short_name = production_products_short_names[[i]])
  }
  return(.tidy_iea_df)
}


#' Convert Production Flows to Resource Flows
#'
#' The IEA gives resource extraction in rows where the `Flow` is "`Production`".
#' This function changes the "`Production`" string
#' to "`Resources [of Product]`",
#' where `product` is the name of the energy carrier for this resource.
#'
#' This function should be called _after_ `specify_primary_production()`,
#' which adjusts for energy industry own use
#' of some primary energy producing industries.
#' If this function is called first,
#' EIOU will not be accounted correctly.
#'
#' @param .tidy_iea_df an IEA data frame whose columns have been renamed by `rename_iea_df_cols()`
#' @param flow the name of the flow column in `.tidy_iea_df`.  Default is "Flow".
#' @param production a string identifying production in the flow column. Default is "Production".
#' @param resources a string identifying resource industries to be added to `.tidy_iea_df`.
#'        Default is "Resources".
#' @param product the name of the product column in `.tidy_iea_df`.  Default is "Product".
#' @param notation a list of specification notations. Default is `IEATools::bracket_notation`.
#'
#' @return `.tidy_iea_df` with `Production` changed to `resources .resources_open product .resources_close` in the `flow` column
#'
#' @export
#'
#' @examples
#' load_tidy_iea_df() %>%
#'   specify_primary_production() %>%
#'   specify_production_to_resources()
specify_production_to_resources <- function(.tidy_iea_df,
                                            flow = "Flow",
                                            product = "Product",
                                            production = "Production",
                                            resources = "Resources",
                                            notation = IEATools::from_notation){
  # Take any remaining "Production" rows and convert them to Resources (Product).
  .tidy_iea_df %>%
    dplyr::mutate(
      "{flow}" := dplyr::case_when(
        .data[[flow]] == production ~ matsbyname::paste_pref_suff(pref = resources, suff = .data[[product]], notation = notation),
        TRUE ~ .data[[flow]]
      )
    )
}


#' Specify interface industries
#'
#' An interface industry is one that moves energy carriers into or out of a country.
#' When `Flow` is any of the interface industries, we need to be more specific.
#' If we don't separate these `Flow`s by `Product`, we run into trouble with
#' upstream swims (e.g., all `Product`s are produced even if only one is needed) and
#' embodied energy calculations (many types of energy are embodied, even if only one should be).
#' This function adds a suffix `[of Product]` to each of these interface industries.
#'
#' Note that "`Production`" also needs to be specified,
#' but that is accomplished in the `specify_primary_production()` and
#' `specify_production_to_resources()` functions.
#'
#' @param .tidy_iea_df a tidy data frame containing IEA extended energy balance data
#' @param flow the name of the flow column in `.tidy_iea_df`.  Default is "`Flow`".
#' @param int_industries a string vector of industries involved in exchanges with other countries,
#'        bunkers, or stock changes. Default is `IEATools::interface_industries`.
#' @param product the name of the product column in `.tidy_iea_df`.  Default is "`Product`".
#' @param notation a list of specification notations. Default is `IEATools::of_notation`.
#'
#' @return a modified version of `.tidy_iea_df` with specified interface industries
#'
#' @export
#'
#' @examples
#' load_tidy_iea_df() %>%
#'   specify_interface_industries()
specify_interface_industries <- function(.tidy_iea_df,
                                         flow = "Flow",
                                         int_industries = IEATools::interface_industries,
                                         product = "Product",
                                         notation = IEATools::of_notation){
  .tidy_iea_df %>%
    dplyr::mutate(
      !!as.name(flow) := dplyr::case_when(
        # !!as.name(flow) %in% int_industries ~ paste0(!!as.name(flow), .interface_ind_open, !!as.name(product), .interface_ind_close),
        .data[[flow]] %in% int_industries ~ matsbyname::paste_pref_suff(pref = .data[[flow]], suff = .data[[product]], notation = notation),
        TRUE ~ .data[[flow]]
      )
    )
}


#' Specify destinations for energy industry own use flows into transformation processes
#'
#' The extended energy balance data from the IEA includes
#' Energy industry own use (EIOU) for many transformation processes.
#' Unfortunately, in some cases
#' the EIOU flows into industries that aren't included in transformation processes.
#' For example, "Electricity" is consumed by
#' "Own use in electricity, CHP and heat plants",
#' which is not a transformation process.
#' We have to make some decisions to ensure that
#' EIOU is routed to actual transformation processes.
#' See details for a list of changes made to the `.tidy_iea_df` data frame.
#'
#' The following changes are made to the `.tidy_iea_df` data frame:
#' 1. EIOU classified as `own_use_elect_chp_heat` is sent to `main_act_producer_elect`.
#' 2. EIOU classified as `pumped_storage` is sent to `main_act_producer_elect`.
#' 3. EIOU classified as `nuclear_industry` is sent to `main_act_producer_elect`.
#' 4. EIOU classified as `non_spec_energy` is sent to `nonspecenergy_reclassify`.
#'
#' After the changes are made, reassigned EIOU may double-up pre-existing EIOU.
#' For example, a country may already have "Electricity" EIOU flowing into "Main activity producer electricity plants".
#' It may also have EIOU flowing into "Nuclear industry".
#' When we switch the EIOU flow into "Nuclear industry" into "Main activity producer electricity plants",
#' we now have two rows of electricity EIOU into "Main activity producer electricity plants".
#' To avoid double rows, all like rows are summed before returning.
#'
#' @param .tidy_iea_df an IEA data frame whose columns have been renamed by [rename_iea_df_cols()]
#' @param flow_aggregation_point the name of the flow aggregation point column in `.tidy_iea_df`. Default is "Flow.aggregation.point".
#' @param eiou a string identifying energy industry own use in the flow aggregation point column. Default is "Energy industry own use".
#' @param transformation_processes a string identifying transformation processes in the flow aggregation point column. Default is "Transformation processes".
#' @param flow the name of the flow column in `.tidy_iea_df`. Default is "Flow".
#' @param own_use_elect_chp_heat a string identifying own use in electricity, CHP and heat plants in the flow column. Default is "Own use in electricity, CHP and heat plants".
#' @param pumped_storage a string identifying pumped storage plants in the flow column. Default is "Pumped storage plants".
#' @param nuclear_industry a string identifying nuclear plants in the flow column. Default is "Nuclear industry".
#' @param e_dot the name of the energy flow column in `.tidy_iea_df`. Default is "E.dot".
#' @param negzeropos the name of a temporary column created in `.tidy_iea_df`. Default is ".negzeropos".
#' @param main_act_producer_elect a string identifying main activity producer electricity plants. Default is "Main activity producer electricity plants".
#'
#' @return a modified version of `.tidy_iea_df`
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' load_tidy_iea_df() %>%
#'   specify_tp_eiou_revisited() %>%
#'   filter(Flow.aggregation.point == "Energy industry own use" &
#'            Flow == "Main activity producer electricity plants")
specify_tp_eiou_revisited <- function(.tidy_iea_df,
                            flow_aggregation_point = "Flow.aggregation.point",
                            eiou = "Energy industry own use",
                            transformation_processes = "Transformation processes",
                            flow = "Flow",
                            # Industries that receive EIOU but are not in Transformation processes
                            own_use_elect_chp_heat = "Own use in electricity, CHP and heat plants",
                            pumped_storage = "Pumped storage plants",
                            nuclear_industry = "Nuclear industry",
                            e_dot = "E.dot",
                            negzeropos = ".negzeropos",
                            # Places where the EIOU will e reassigned
                            main_act_producer_elect = "Main activity producer electricity plants"){
  .tidy_iea_df %>%
    matsindf::verify_cols_missing(negzeropos)

  .tidy_iea_df %>%
    gather_producer_autoproducer() %>%
    route_pumped_storage() %>%
    route_own_use_elect_chp_heat() #%>%
    #add_nuclear_industry() %>%
    #route_non_specified_flows()


    # dplyr::mutate(
    #   !!as.name(flow) := dplyr::case_when(
        # Apply "Own use in electricity, CHP and heat plants" to "Main activity producer electricity plants"
        # This solves a problem in Ghana where "Own use in electricity, CHP and heat plants"
        # would lead to a zero row in the make matrix.
        # There is no Industry that makes "Own use in electricity, CHP and heat plants".
        # In the absence of further information, and assuming that there is more
        # electricity production than CHP or heat production in each country,
        # we apply "Own use in electricity, CHP and heat plants" to
        # "Main activity producer electricity plants".
        #!!as.name(flow) == own_use_elect_chp_heat & !!as.name(flow_aggregation_point) == eiou ~ main_act_producer_elect,

        # When pumped storage is in the mix,
        # the IEA data helpfully indicates EIOU assigned to "Pumped storage plants".
        # However, Pumped storage plants do not make any electricity,
        # so there is no appropriate Industry for its EIOU.
        # To fix this problem,
        # apply EIOU by Pumped storage plants to
        # the Industry in which production from Pumped storage plants is accounted:
        # Main activity producer electricity plants.
        #!!as.name(flow) == pumped_storage & !!as.name(flow_aggregation_point) == eiou ~ main_act_producer_elect,

        # If Nuclear is used, we need to reclassify EIOU by Nuclear plants
        # to Main activity producer electricity plants.
        #!!as.name(flow) == nuclear_industry & !!as.name(flow_aggregation_point) == eiou ~ main_act_producer_elect,

        # Non-specified (energy) is an Industry that receives EIOU.
        # However, Non-specified (energy) is not an Industry that makes anything.
        # So, we need to reassign these EIOU flows somwehere.
        # For the UK, the numbers for "Non-specified (energy)" are rather small.
        # In the absence of any better information, we apply
        # "Non-specified (energy)" to nonspecenergy_reclassify.
        # !!as.name(flow) == non_spec_energy & !!as.name(flow_aggregation_point) == eiou ~ nonspecenergy_reclassify,

        # Otherwise, just keep the same value for the flow column.
    #      TRUE ~ !!as.name(flow)
    #   )
    # ) %>%
    # dplyr::mutate(
      # Add a column that tells whether E.dot is negative, zero, or positive.
      # The goal is to sum like input or like outputs of a Transformation process.
      # Unless we differentiate by the sign of E.dot,
      # we'll be getting net energy flows, which we don't want.
    #   !!as.name(negzeropos) := dplyr::case_when(
    #     !!as.name(e_dot) < 0 ~ "neg",
    #     !!as.name(e_dot) == 0 ~ "zero",
    #     !!as.name(e_dot) > 0 ~ "pos"
    #   )
    # ) %>%
    # Now sum similar rows using summarise.
    # Group by everything except the energy flow rate column, "E.dot".
    # matsindf::group_by_everything_except(e_dot) %>%
    # dplyr::summarise(
    #   !!as.name(e_dot) := sum(!!as.name(e_dot))
    # ) %>%
    # dplyr::mutate(
      # Eliminate the column we added.
    #   !!as.name(negzeropos) := NULL
    # ) %>%
    # dplyr::ungroup()
}


#' Find transformation sinks and sources
#'
#' In the IEA extended energy balance data,
#' transformation processes (tp) ought to both consume and produce energy.
#' But some transformation processes consume energy without producing any energy;
#' others produce without consuming.
#' Such transformation processes can be called "transformation sinks" and
#' "transformation sources," respectively.
#' This function finds and identifies transformation processes that act as sinks or sources.
#'
#' It is important to identify transformation sinks,
#' because they cause two problems for physical supply-use table (PSUT) analysis.
#' First, when swimming upstream, a PSUT analysis cannot "see" the sunk energy carriers,
#' because they have no downstream effects.
#' Thus, upstream swims cannot conserve energy.
#' Second, when calculating embodied energy for each downstream energy carrier,
#' the sunk energy carriers cannot be embodied in any final demand energy carriers.
#' Thus, embodied energy calculations cannot conserve energy.
#'
#' Transformation sources can also cause problems for physical supply-use table (PSUT) analysis.
#' In particular, when swimming upstream, a PSUT analysis will "see" the final energy sources,
#' but cannot see any associated primary energy carriers.
#'
#' Transformation sinks and sources are identified by the following algorithm:
#'
#' 1. Identify (per group in `.tidy_iea_df`) all `Transformation processes` that consume energy (negative value for `E.dot`).
#'    Energy consumption can be for the transformation process itself or for Energy industry own use.
#' 2. Identify (per group in `.tidy_iea_df`) all `Transformation processes` that produce energy (positive value for `E.dot`).
#' 3. Take the set difference between the two (consumers less producers for sinks and producers less consumers for sources).
#'    The set difference is the list of transformation sinks or sources, respectively.
#'
#' [tp_sinks_sources()] is a function not unlike [dplyr::summarise()];
#' it returns a summary containing grouping variables and industries that are transformation sinks or sources.
#' So be sure to specify (or accept defaults for)
#' the `grouping_vars` argument.
#' Typical grouping variables are `Method`, `Last.stage`, `Country`, `Year`, `Energy.type`.
#' Don't group on `Flow.aggregation.point`, because energy from different aggregation points
#' (`Energy industry own use` and `Transformation processes`) flows into each machine.
#' Don't group on `Flow`, `Product`, or `E.dot`, either.
#' If groups are not set,
#' `flow`s will be analyzed together, possibly leading to missed transformation sinks or sources.
#'
#' The various `specify_*()` functions should also be called _before_ calling [tp_sinks_sources()].
#' The `specify_*()` functions clean up the IEA data, ensuring that energy is routed to the right places.
#'
#' Note that this function only identifies transformation sinks or sources;
#' it does not fix the problem.
#' To solve the problem of transformation sinks,
#' see the [tp_sinks_to_nonenergy()] function.
#' [tp_sinks_to_nonenergy()] uses the output of [tp_sinks_sources()]
#' to route energy consumed by transformation sinks to `Non-energy use industry/transformation/energy`.
#' There is no function to solve the problem of transformation sources at this time.
#'
#' @param .tidy_iea_df a tidy data frame containing IEA extended energy balance data
#' @param type one of "sinks" or "sources"
#' @param flow_aggregation_point the name of the flow aggregation point column in `.tidy_iea_df`. Default is "`Flow.aggregation.point`".
#' @param transformation_processes a string that identifies transformation processes in the `flow_aggregation_point` column. Default is "`Transformation processes`".
#' @param eiou a string that identifies energy industry own use in the `flow_aggregation_point` column. Default is "`Energy industry own use`".
#' @param flow the name of the flow column in `.tidy_iea_df`. Default is "`Flow`".
#' @param product the name of the product column in `.tidy_iea_df`. Default is "`Product`".
#' @param e_dot the name of the energy rate column in `.tidy_iea_df`. Default is "`E.dot`".
#'
#' @return the `grouping_vars` and the `flow` column,
#'         with one row for each industry that is a transformation sink or source.
#'         Industries that are transformation sinks or sources are named in the `flow` column.
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' load_tidy_iea_df() %>%
#'   specify_primary_production() %>%
#'   specify_interface_industries() %>%
#'   specify_tp_eiou_revisited() %>%
#'   tp_sinks_sources()
#' load_tidy_iea_df() %>%
#'   specify_primary_production() %>%
#'   specify_interface_industries() %>%
#'   specify_tp_eiou_revisited() %>%
#'   tp_sinks_sources(type = "sources")
tp_sinks_sources <- function(.tidy_iea_df,
                             type = c("sinks", "sources"),
                             flow_aggregation_point = "Flow.aggregation.point",
                             transformation_processes = "Transformation processes",
                             eiou = "Energy industry own use",
                             flow = "Flow",
                             product = "Product",
                             e_dot = "E.dot"){
  type <- match.arg(type)
  grouping_vars <- matsindf::everything_except(.tidy_iea_df, flow_aggregation_point, flow, product, e_dot)
  use_rows <- .tidy_iea_df %>%
    dplyr::group_by(!!!grouping_vars) %>%
    dplyr::filter((!!as.name(flow_aggregation_point) == transformation_processes | !!as.name(flow_aggregation_point) == eiou) & !!as.name(e_dot) < 0) %>%
    dplyr::select(dplyr::group_cols(), flow) %>%
    unique() %>%
    dplyr::ungroup()
  make_rows <- .tidy_iea_df %>%
    dplyr::group_by(!!!grouping_vars) %>%
    dplyr::filter(!!as.name(flow_aggregation_point) == transformation_processes & !!as.name(e_dot) > 0) %>%
    dplyr::select(dplyr::group_cols(), flow) %>%
    unique() %>%
    dplyr::ungroup()
  # setdiff gives the rows that are IN use_rows but NOT in make_rows.
  if (type == "sinks") {
    return(dplyr::setdiff(use_rows, make_rows))
  } else {
    return(dplyr::setdiff(make_rows, use_rows))
  }
}


#' Reassign Transformation process sinks to Non-energy use
#'
#' Transformation processes that consume energy without producing any energy are called
#' "transformation process sinks".
#' See [tp_sinks_sources()] for information about why transformation process sinks are problematic.
#' This function reclassifies energy flowing into transformation process sinks
#' as `non_energy_flow`, by default "Non-energy use in industry/transformation/energy".
#'
#' @param .tidy_iea_df a tidy data frame containing IEA extended energy balance data
#' @param ledger_side the name of the ledger side column in `.tidy_iea_df`. Default is "`Ledger.side`".
#' @param consumption a string identifying the consumption side of the ledger. Default is "`Consumption`".
#' @param flow_aggregation_point the name of the flow aggregation point column in `.tidy_iea_df`. Default is "`Flow.aggregation.point`".
#' @param non_energy_flow_agg_point the name of the aggregation point where transformation process sinks will be reassigned. Default is "`Non-energy use`".
#' @param transformation_processes a string that identifies transformation processes in the `flow_aggregation_point` column. Default is "`Transformation processes`".
#' @param eiou a string that identifies energy industry own use in the `flow_aggregation_point` column. Default is "`Energy industry own use`".
#' @param flow the name of the flow column in `.tidy_iea_df`. Default is "`Flow`".
#' @param non_energy_flow a sting identifying non-energy flows. Default is "`Non-energy use industry/transformation/energy`".
#' @param product the name of the product column in `.tidy_iea_df`. Default is "`Product`".
#' @param e_dot the name of the energy rate column in `.tidy_iea_df`. Default is "`E.dot`".
#'
#' @return `.tidy_iea_df` with energy sunk in Transformation processes sinks reassigned to Non-energy use
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' DF <- data.frame(
#'   Ledger.side = c("Supply", "Supply", "Supply", "Consumption"),
#'   Flow.aggregation.point = c("Transformation processes",
#'                              "Transformation processes",
#'                              "Transformation processes",
#'                              "Non-energy use"),
#'   Flow = c("Automobiles", "Automobiles", "Furnaces",
#'            "Non-energy use industry/transformation/energy"),
#'   Product = c("Petrol", "MD", "Coal", "Coal"),
#'   E.dot = c(-1, 1, -2, 8),
#'   stringsAsFactors = FALSE
#' ) %>%
#'   mutate(
#'     Method = "PCM",
#'     Last.stage = "Final",
#'     Energy.type = "E",
#'     Country = "Bogus",
#'     Year = 1971
#'   )
#' DF
#' DF %>%
#'   tp_sinks_to_nonenergy()
tp_sinks_to_nonenergy <- function(.tidy_iea_df,
                                  ledger_side = "Ledger.side",
                                  consumption = "Consumption",
                                  flow_aggregation_point = "Flow.aggregation.point",
                                  non_energy_flow_agg_point = "Non-energy use",
                                  transformation_processes = "Transformation processes",
                                  eiou = "Energy industry own use",
                                  flow = "Flow",
                                  non_energy_flow = "Non-energy use industry/transformation/energy",
                                  product = "Product",
                                  e_dot = "E.dot"){
  # grouping_vars = c("Method", "Last.stage", "Country", "Year", "Energy.type")){
  # First step is to find all Transformation process sinks.
  # These items need to removed from the IEAData data frame, eventually.
  Sinks <- .tidy_iea_df %>%
    tp_sinks_sources(type = "sinks",
                     flow_aggregation_point = flow_aggregation_point,
                     transformation_processes = transformation_processes,
                     eiou = eiou,
                     flow = flow,
                     product = product,
                     e_dot = e_dot)
  # Figure out which rows have sinks in them.
  # They will need to be removed later.
  # But a modified version of Sinks will be routed to final demand.
  # (semi_join keeps only those rows in .tidy_iea_df that match rows in Sinks.)
  Remove_later <- dplyr::semi_join(.tidy_iea_df, Sinks, by = names(Sinks))
  # When modified, the rows in Remove_later will be added to final demand
  # Change the metadata for these items.
  To_add_to_final_demand <- Remove_later %>%
    dplyr::mutate(
      !!as.name(ledger_side) := consumption,
      !!as.name(flow_aggregation_point) := non_energy_flow_agg_point,
      !!as.name(flow) := non_energy_flow,
      # The Remove_later entries are all negative
      # (because they were on the Consumption side of the ledger in Transformation processes).
      # But they need to be positive when they are moved to final demand.
      !!as.name(e_dot) := abs(!!as.name(e_dot))
    )
  Temp <- .tidy_iea_df %>%
    # Eliminate rows in .tidy_iea_df that match Remove_later,
    dplyr::anti_join(Remove_later, by = names(Remove_later)) %>%
    # rbind the new rows to the data frame
    dplyr::bind_rows(To_add_to_final_demand)
  # Look at Temp to find the rows Non-energy use rows
  Nonenergy <- Temp %>%
    dplyr::filter(!!as.name(flow) == non_energy_flow)
  # Summarize these Non-energy flows
  # This has the effect of adding the new Non-energy use to any existing non-energy use
  # in the same group.
  SummarizedNonenergy <- Nonenergy %>%
    # Group by all columns except for E.dot
    matsindf::group_by_everything_except(e_dot) %>%
    dplyr::summarise(!!as.name(e_dot) := sum(!!as.name(e_dot))) %>%
    dplyr::ungroup()
  # Return after removing the non-summarized Non-energy flows and inserting the summarized Non-energy flows.
  Temp %>%
    # Remove the Nonenergy rows
    dplyr::anti_join(Nonenergy, by = names(Temp)) %>%
    dplyr::bind_rows(SummarizedNonenergy)
}


#' Specify all industries
#'
#' This is a convenience function.
#' It bundles several others:
#' 1. [specify_primary_production()]
#' 2. [specify_production_to_resources()]
#' 3. [specify_tp_eiou_revisited()]
#' 4. [specify_interface_industries()]
#' 5. [tp_sinks_to_nonenergy()]
#'
#' Each bundled function is called in turn using default arguments.
#' See examples for two ways to achieve the same result.
#'
#' @param .tidy_iea_df a tidy data frame containing IEA extended energy balance data
#'
#' @return an enhanced and corrected version of `.tidy_iea_df`
#'         that is ready for physical supply-use table (PSUT) analysis.
#'
#' @export
#'
#' @examples
#' # Simple
#' load_tidy_iea_df() %>%
#'   specify_all()
#' # Complicated
#' load_tidy_iea_df() %>%
#'   specify_primary_production() %>%
#'   specify_production_to_resources() %>%
#'   specify_tp_eiou_revisited() %>%
#'   specify_interface_industries() %>%
#'   tp_sinks_to_nonenergy()
specify_all_revisited <- function(.tidy_iea_df){
  .tidy_iea_df %>%
    specify_primary_production() %>%
    specify_production_to_resources() %>%
    specify_tp_eiou_revisited() %>%
    specify_interface_industries() %>%
    tp_sinks_to_nonenergy()
}


#' Remove specification strings from a column
#'
#' `Flow` and `Product` columns of IEA data frames may have been "specified"
#' with one of the functions of `specify_all()`.
#' The specifying makes it difficult to sort the columns in IEA order (with `sort_iea_df()`),
#' as the `Flow` and `Product` columns now contain non-IEA flows and products.
#' To enable sorting, this function de-specifies a column in `.df`.
#'
#' De-specifying includes the following changes:
#'     * Any "Resource" flows are replaced by "Production". E.g., "Resources \[of Coal\]" becomes "Production".
#'     * All parenthetical decorations are removed.  E.g., "Other bituminous coal \[of Coal mines\]" becomes "Other bituminous coal".
#'
#' Identification of parenthetical notation delimiters is determined by a notation object
#'
#'
#' @param .df the data frame in which `col` exists.
#' @param col the string name of the column in `.df` to be de-specified.
#' @param despecified_col the string name of the column in the output data frame to contain the de-specified version of `col`.
#' @param notations the notations used for row and column names. See `matsbyname::notation_vec()`.
#'                 Default is `list(IEATools::of_notation, IEATools::from_notation)`,
#'                 because both `IEATools::of_notation` and `IEATools::from_notation` can be used in the `Flow` column.
#' @param production,resources See `IEATools::tpes_flows`.
#'
#' @return a de-specified version of `.df`
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' load_tidy_iea_df() %>%
#'   specify_all() %>%
#'   despecify_col(col = "Flow", despecified_col = "clean_Flow") %>%
#'   select(Flow, Product, E.dot, clean_Flow) %>%
#'   filter(endsWith(Flow, bracket_notation[["suff_end"]]))
despecify_col <- function(.df, col, despecified_col,
                          notations = list(IEATools::of_notation, IEATools::from_notation),
                          production = IEATools::tpes_flows$production,
                          resources = IEATools::tpes_flows$resources) {
  out <- .df %>%
    dplyr::mutate(
      "{despecified_col}" := dplyr::case_when(
        # Change "Resources" back to "Production"
        startsWith(.data[[col]], resources) ~ production,
        TRUE ~ .data[[col]]
      )
    )
  # Now eliminate all suffixes from despecified_col in out
  for (nota in notations) {
    out <- out %>%
      dplyr::mutate(
        "{despecified_col}" := matsbyname::split_pref_suff(out[[despecified_col]], notation = nota) %>%
          purrr::transpose() %>%
          magrittr::extract2("pref")
      )
  }
  return(out)
}

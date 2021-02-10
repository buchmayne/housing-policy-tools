#' Get Matrix of Household Income and Housing Affordability (CHAS Data 2013-2017)
#'
#' This function formats table 18 of the 2013-2017 CHAS data to return the "matrix" of units by household income and unit affordability.
#' @param geography Options are "county" or "place", determines the geographic unit to return CHAS data for.
#' @param state U.S. state to return data for, requires the state two letter abbreviation to be passed
#' @param bin_adjustment_type Options are either "unify" or "low end ownership", if "unify" is selected the bins are unified across renters and owners which makes the results less granular but no assumptions are made. "low end ownership" preserves the 0-30% affordability bin but assumes all ownership units in 0-50% are actuall in 30-50%.
#' @export
#' @examples
#' get_affordability_matrix_from_chas(geography = "place", state = "ID", bin_adjustment_type = "unify")

get_affordability_matrix_from_chas <- function(geography, state, bin_adjustment_type = "unify") {

  # restrict queries to county, and, place
  if (!bin_adjustment_type %in% c("unify", "low end ownership")) {
    stop("'unify', and 'low end ownership' are the only valid inputs to bin_adjustment_type")
  }

  renters <- housingdata::get_income_and_rent_for_renters_from_chas(geography, state)
  owners_with_mortgage <- housingdata::get_income_and_value_for_owners_with_mortgage_from_chas(geography, state)
  owners_without_mortgage <- housingdata::get_income_and_value_for_owners_without_mortgage_from_chas(geography, state)

  owners <- dplyr::inner_join(owners_with_mortgage, owners_without_mortgage)
  owners <- owners %>%
    dplyr::mutate(ownership_units = owner_occ_hu + owner_occ_no_mortgage_hu) %>%
    dplyr::select(
      geoid,
      name,
      state_code,
      state_abbreviation,
      state_name,
      home_value,
      household_income,
      ownership_units
      )

  if (bin_adjustment_type == "unify") {

    adjusted_data <- housingdata::unify_rent_and_value(renters, owners)

    affordability_bins <- c("0-50%", "50-80%", "+80%")

  }

  else if (bin_adjustment_type == "low end ownership") {

    adjusted_data <- housingdata::low_end_ownership_rent_and_value(renters, owners)

    affordability_bins <- c("0-30%", "30-50%", "50-80%", "+80%")

  }


  affordability_matrix <- adjusted_data %>%
    dplyr::mutate(total_units = renter_occ_hu + ownership_units) %>%
    dplyr::select(geoid, state_name, name, affordability, household_income, total_units) %>%
    tidyr::spread(household_income, total_units) %>%
    dplyr::mutate(affordability = factor(affordability, levels = affordability_bins)) %>%
    dplyr::arrange(geoid, affordability) %>%
    dplyr::select(
      geoid,
      state_name,
      name,
      affordability,
      `0-30%`,
      `30-50%`,
      `50-80%`,
      `80-100%`,
      `+100%`
    )

  return(affordability_matrix)

}

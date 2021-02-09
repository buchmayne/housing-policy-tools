#' Get Ratio of Housing Units to Households from ACS
#'
#' This function uses the census API to pull down cost burdening data from table B25106 of the ACS. It then tidies the returned data.
#' @param year Defaults to 2019, value passed to the census API call
#' @param geography The unit of geography to return data for, options are "state", "county", "place" and "tract". This value is passed to the census API call
#' @param state U.S. state to return data for, value passed to the census API call. Defaults to NULL
#' @param county Defaults to NULL, option to return tract or county data from a specific county. Can't return place data from a specific county. This value is passed to census API call
#' @param survey Defaults to "acs5", the version of the ACS to return. Options are "acs5" and "acs1" for either the five-year or one-year ACS survey. Value passed to census API call
#' @export
#' @examples
#' get_ratio_housing_units_to_households(year = 2019, geography = "county", state = "ID", survey = "acs5")


get_ratio_housing_units_to_households <- function(year = 2019, geography, state = NULL, county = NULL, survey = "acs5") {

  # restrict queries to state, county, place, and tract
  if (!geography %in% c("state", "county", "place", "tract")) {
    stop("state, county, place, and tract are the only valid inputs to geography")
  }

  # error handling for survey parameter
  if (!survey %in% c("acs5", "acs1")) {
    stop("acs5 and acs1 are the only valid inputs to survey")
  }

  # check for place + county combo which is invalid
  if (geography == "place" & !is.null(county)) {
    stop("Can't pass county parameter when using place as geography")
  }

  # get census variable names
  acs_vars <- tidycensus::load_variables(year = year, dataset = survey, cache = TRUE)

  household_type_tbl <- "B11001"
  housing_units_var <- "B25001_001"

  # pull down data via census api
  raw_hh <- tidycensus::get_acs(
    geography = geography,
    table = household_type_tbl,
    state = state,
    county = county,
    year = year,
    survey = survey,
    cache_table = TRUE
  )

  raw_hu <- tidycensus::get_acs(
    geography = geography,
    variables = housing_units_var,
    state = state,
    county = county,
    year = year,
    survey = survey,
    cache_table = TRUE
  )


  raw_hh <- dplyr::inner_join(raw_hh, acs_vars, by = c("variable" = "name"))
  raw_hu <- dplyr::inner_join(raw_hu, acs_vars, by = c("variable" = "name"))

  # relabel and filter on total households
  hh <- raw_hh %>%
    dplyr::filter(label == "Estimate!!Total:") %>%
    dplyr::select(-variable, -concept, -label) %>%
    dplyr::rename(
      households = estimate,
      hh_moe = moe
    )

  # relabel
  hu <- raw_hu %>%
    dplyr::select(-variable, -concept, -label) %>%
    dplyr::rename(
      housing_units = estimate,
      hu_moe = moe
    )

  # take ratio of housing units to households
  ratio_ <- dplyr::inner_join(hu, hh)
  ratio_ <- ratio_ %>%
    dplyr::mutate(ratio_hu_to_hh = housing_units / households)

  return(ratio_)

}

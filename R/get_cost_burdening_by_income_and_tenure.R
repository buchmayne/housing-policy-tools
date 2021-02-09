#' Get Cost Burdening by Income and Tenure
#'
#' This function uses the census API to pull down cost burdening data from table B25106 of the ACS. It then tidies the returned data.
#' @param year Defaults to 2019, value passed to the census API call
#' @param geography The unit of geography to return data for, options are "state", "county", "place" and "tract". This value is passed to the census API call
#' @param state U.S. state to return data for, value passed to the census API call. Defaults to NULL
#' @param county Defaults to NULL, option to return tract or county data from a specific county. Can't return place data from a specific county. This value is passed to census API call
#' @param survey Defaults to "acs5", the version of the ACS to return. Options are "acs5" and "acs1" for either the five-year or one-year ACS survey. Value passed to census API call
#' @export
#' @examples
#' get_cost_burdening_by_income_and_tenure(year = 2019, geography = "county", state = "ID", survey = "acs5")


get_cost_burdening_by_income_and_tenure <- function(year = 2019, geography, state = NULL, county = NULL, survey = "acs5") {

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
  cost_burdening_tbl <- "B25106"

  # pull down data via census api
  raw_cb <- tidycensus::get_acs(
    geography = geography,
    table = cost_burdening_tbl,
    survey = survey,
    year = year,
    state = state,
    county = county,
    cache_table = TRUE
  )


  # join variable names and relabel
  raw_cb <- dplyr::inner_join(raw_cb, acs_vars, by = c("variable" = "name"))

  raw_cb <- raw_cb[c("GEOID", "NAME", "estimate", "moe", "label")]
  colnames(raw_cb) <- c("GEOID", "NAME", "housing_units", "moe", "label")


  # drop row containing totals and subtotals, create tenure column
  cb <- raw_cb %>%
    dplyr::filter(!label %in% c("Estimate!!Total:", "Estimate!!Total:!!Owner-occupied housing units:", "Estimate!!Total:!!Renter-occupied housing units:")) %>%
    dplyr::mutate(label = gsub("Estimate!!Total:!!", "", label)) %>%
    dplyr::mutate(tenure = dplyr::if_else(
      stringr::str_detect(label, "Owner-occupied housing units"),
      "Owner-occupied housing units",
      dplyr::if_else(
        stringr::str_detect(label, "Renter-occupied housing units"),
        "Renter-occupied housing units",
        NA_character_
        )
      )
    ) %>%
    dplyr::mutate(label = gsub("Owner-occupied housing units:!!", "", gsub("Renter-occupied housing units:!!", "", label))) %>%
    dplyr::filter(!stringr::str_detect(label, ":$"))


  # extract household income and housing costs from label and create new columns
  cb <- cb %>%
    dplyr::mutate(
      household_income = dplyr::case_when(
        stringr::str_detect(label, "Less than \\$20,000") ~ "Less than $20,000",
        stringr::str_detect(label, "\\$20,000 to \\$34,999") ~ "$20,000 to $34,999",
        stringr::str_detect(label, "\\$35,000 to \\$49,999") ~ "$35,000 to $49,999",
        stringr::str_detect(label, "\\$50,000 to \\$74,999") ~ "$50,000 to $74,999",
        stringr::str_detect(label, "\\$75,000 or more") ~ "$75,000 or more",
        stringr::str_detect(label, "Zero or negative income") ~ "Zero or negative income",
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::mutate(
      housing_costs_as_pct_of_income = dplyr::case_when(
        stringr::str_detect(label, "Less than 20 percent") ~ "Less than 20 percent",
        stringr::str_detect(label, "20 to 29 percent") ~ "20 to 29 percent",
        stringr::str_detect(label, "30 percent or more") ~ "30 percent or more",
        stringr::str_detect(label, "No cash rent") ~ "No cash rent",
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::select(-label)


  # check that totals match before and after modification
  raw_total_units <- raw_cb %>%
    dplyr::filter(label == "Estimate!!Total:") %>%
    dplyr::select(GEOID, housing_units)

  check_totals_match <- cb %>%
    dplyr::group_by(GEOID) %>%
    dplyr::summarise(transformed_total = sum(housing_units)) %>%
    dplyr::ungroup() %>%
    dplyr::inner_join(raw_total_units) %>%
    dplyr::filter(housing_units != transformed_total)

  stopifnot(nrow(check_totals_match) == 0)


  return(cb)


}

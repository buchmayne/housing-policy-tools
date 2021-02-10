#' Get Count of Housing Units by Income and Rent for Renters (CHAS Data 2013-2017)
#'
#' This function formats table 18C of the 2013-2017 CHAS data to return the count of units for renters by income and affordability.
#' @param geography Options are "county" or "place", determines the geographic unit to return CHAS data for.
#' @param state U.S. state to return data for, requires the state two letter abbreviation to be passed
#' @export
#' @examples
#' get_income_and_rent_for_renters_from_chas(geography = "place", state = "ID")

get_income_and_rent_for_renters_from_chas <- function(geography, state) {

  # restrict queries to county, and, place
  if (!geography %in% c("county", "place")) {
    stop("county, and place are the only valid inputs to geography")
  }


  if (geography == "county") {

    load("data/tbl18C_county.rda")

    # filter on state
    tbl18C_county <- tbl18C_county %>%
      dplyr::filter(state_abbreviation == state)


    return(tbl18C_county)

  }

  else if (geography == "place") {

    load("data/tbl18C_places.rda")

    # filter on state
    tbl18C_places <- tbl18C_places %>%
      dplyr::filter(state_abbreviation == state)


    return(tbl18C_places)

  }

}

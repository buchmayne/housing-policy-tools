#' Get Count of Housing Units by Income and Value of Home for Owners with a Mortgage (CHAS Data 2013-2017)
#'
#' This function formats table 18A of the 2013-2017 CHAS data to return the count of units for owners with a mortgage by income and affordability.
#' @param geography Options are "county" or "place", determines the geographic unit to return CHAS data for.
#' @param state U.S. state to return data for, requires the state two letter abbreviation to be passed
#' @export
#' @examples
#' get_income_and_value_for_owners_with_mortgage_from_chas(geography = "county", state = "ID")

get_income_and_value_for_owners_with_mortgage_from_chas <- function(geography, state) {

  # restrict queries to county, and, place
  if (!geography %in% c("county", "place")) {
    stop("county, and place are the only valid inputs to geography")
  }


  if (geography == "county") {

    load("data/tbl18A_county.rda")

    # filter on state
    tbl18A_county <- tbl18A_county %>%
      dplyr::filter(state_abbreviation == state)


    return(tbl18A_county)

  }

  else if (geography == "place") {

    load("data/tbl18A_places.rda")

    # filter on state
    tbl18A_places <- tbl18A_places %>%
      dplyr::filter(state_abbreviation == state)


    return(tbl18A_places)

  }

}


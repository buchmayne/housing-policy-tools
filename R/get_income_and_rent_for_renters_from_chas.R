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

    tbl18C <- read.csv("data/CHAS_COUNTY_13_17/Table18C.csv", stringsAsFactors = FALSE)
    fips_joiner <- tidycensus::fips_codes

    geoid_prefix <- "^05000US"

  }

  else if (geography == "place") {

    tbl18C <- read.csv("data/CHAS_PLACES_13_17/Table18C.csv", stringsAsFactors = FALSE)

    fips_joiner <- tidycensus::fips_codes %>%
      dplyr::select(-county, -county_code) %>%
      dplyr::distinct()

    geoid_prefix <- "^16000US"

  }


  dd_col_names <- c(
    "Column Name",
    "Column Type",
    "Description 1",
    "Description 2",
    "Description 3",
    "Description 4",
    "Description 5",
    "Census variable codes"
  )

  data_dictionary <- read.csv(
    "data/CHAS_TBL18_DATA_DICTIONARY.csv",
    stringsAsFactors = FALSE,
    sep = "\t",
    col.names = dd_col_names,
    header = FALSE
    )


  tbl18C_data_dictionary <- data_dictionary %>%
    dplyr::filter(Column.Name %in% names(tbl18C)) %>%
    dplyr::filter(Column.Type == "Detail") %>%
    dplyr::arrange(Description.3, Description.4) %>%
    dplyr::rename(CHAS_COLS = Column.Name)

  tbl18C <- tbl18C %>%
    dplyr::select(geoid, name, tbl18C_data_dictionary$CHAS_COLS)

  tbl18C <- tbl18C %>%
    tidyr::gather(key = CHAS_COLS, value = estimate, -geoid, -name) %>%
    dplyr::inner_join(tbl18C_data_dictionary) %>%
    dplyr::group_by(geoid, name, Description.3, Description.4) %>%
    dplyr::summarise(renter_occ_hu = sum(estimate)) %>%
    dplyr::ungroup()


  tbl18C <- tbl18C %>%
    dplyr::mutate(geoid = gsub(geoid_prefix, "", geoid)) %>%
    dplyr::mutate(
      state_code = stringr::str_sub(geoid, 1, 2),
      county_code = stringr::str_sub(geoid, 3, 5)
    ) %>%
    dplyr::mutate(
      rent = dplyr::case_when(
        Description.3 == " AND Rent greater than RHUD80" ~ "+80%",
        Description.3 == " AND Rent greater than RHUD30 and less than or equal to RHUD50" ~ "30-50%",
        Description.3 == " AND Rent greater than RHUD50 and less than or equal to RHUD80" ~ "50-80%",
        Description.3 == " AND Rent less than or equal to RHUD30" ~ "0-30%"
      )
    ) %>%
    dplyr::mutate(
      household_income = dplyr::case_when(
        Description.4 == " AND Household income greater than 100% of HAMFI" ~ "+100%",
        Description.4 == " AND Household income greater than 50% of HAMFI but less than or equal to 80% of HAMFI" ~ "50-80%",
        Description.4 == " AND Household income greater than 80% of HAMFI but less than or equal to 100% of HAMFI" ~ "80-100%",
        Description.4 == " AND Household income less than or equal to 30% of HAMFI" ~ "0-30%",
        Description.4 == " AND Household income greater than 30% of HAMFI but less than or equal to 50% of HAMFI" ~ "30-50%"
      )
    ) %>%
    dplyr::select(-Description.3, -Description.4)


  # join fips codes
  tbl18C <- tbl18C %>%
    dplyr::inner_join(fips_joiner) %>%
    dplyr::rename(state_abbreviation = state)

  # filter on state
  tbl18C <- tbl18C %>%
    dplyr::filter(state_abbreviation == state)

  if (geography == "place") {
    tbl18C <- tbl18C %>%
      dplyr::select(-county_code)
  }

  return(tbl18C)

}

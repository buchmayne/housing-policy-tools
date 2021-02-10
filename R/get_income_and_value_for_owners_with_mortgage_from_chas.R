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

    tbl18A <- read.csv("data/CHAS_COUNTY_13_17/Table18A.csv", stringsAsFactors = FALSE)
    fips_joiner <- tidycensus::fips_codes

    geoid_prefix <- "^05000US"

  }

  else if (geography == "place") {

    tbl18A <- read.csv("data/CHAS_PLACES_13_17/Table18A.csv", stringsAsFactors = FALSE)

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

  data_dictionary <- read.csv("data/CHAS_TBL18_DATA_DICTIONARY.csv", stringsAsFactors = FALSE, sep = "\t", col.names = dd_col_names, header = FALSE)


  tbl18A_data_dictionary <- data_dictionary %>%
    dplyr::filter(Column.Name %in% names(tbl18A)) %>%
    dplyr::filter(Column.Type == "Detail") %>%
    dplyr::arrange(Description.3, Description.4) %>%
    dplyr::rename(CHAS_COLS = Column.Name)

  tbl18A <- tbl18A %>%
    dplyr::select(geoid, name, tbl18A_data_dictionary$CHAS_COLS)

  tbl18A <- tbl18A %>%
    tidyr::gather(key = CHAS_COLS, value = estimate, -geoid, -name) %>%
    dplyr::inner_join(tbl18A_data_dictionary) %>%
    dplyr::group_by(geoid, name, Description.3, Description.4) %>%
    dplyr::summarise(owner_occ_hu = sum(estimate)) %>%
    dplyr::ungroup()

  tbl18A <- tbl18A %>%
    dplyr::mutate(geoid = gsub(geoid_prefix, "", geoid)) %>%
    dplyr::mutate(
      state_code = stringr::str_sub(geoid, 1, 2),
      county_code = stringr::str_sub(geoid, 3, 5)
      ) %>%
    dplyr::mutate(
      home_value = dplyr::case_when(
        Description.3 == " AND Value greater than VHUD100" ~ "+100%",
        Description.3 == " AND Value greater than VHUD50 and less than or equal to VHUD80" ~ "50-80%",
        Description.3 == " AND Value greater than VHUD80 and less than or equal to VHUD100" ~ "80-100%",
        Description.3 == " AND Value less than or equal to VHUD50" ~ "0-50%"
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
  tbl18A <- tbl18A %>%
    dplyr::inner_join(fips_joiner) %>%
    dplyr::rename(state_abbreviation = state)

  # filter on state
  tbl18A <- tbl18A %>%
    dplyr::filter(state_abbreviation == state)

  if (geography == "place") {
    tbl18A <- tbl18A %>%
      dplyr::select(-county_code)
  }

  return(tbl18A)

}


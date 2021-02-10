library(dplyr)
library(stringr)
library(tidyr)
library(tidycensus)


# Define Functions to Format CHAS Data ------------------------------------


preprocess_tbl18_A <- function(geography) {

  # restrict queries to county, and, place
  if (!geography %in% c("county", "place")) {
    stop("county, and place are the only valid inputs to geography")
  }


  if (geography == "county") {

    tbl18A <- read.csv("data-raw/COUNTY_Table18A.csv", stringsAsFactors = FALSE)
    fips_joiner <- tidycensus::fips_codes

    geoid_prefix <- "^05000US"

  }

  else if (geography == "place") {

    tbl18A <- read.csv("data-raw/PLACES_Table18A.csv", stringsAsFactors = FALSE)

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

  data_dictionary <- read.csv("data-raw/CHAS_TBL18_DATA_DICTIONARY.csv", stringsAsFactors = FALSE, sep = "\t", col.names = dd_col_names, header = FALSE)


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


  if (geography == "place") {
    tbl18A <- tbl18A %>%
      dplyr::select(-county_code)
  }

  return(tbl18A)

}


preprocess_tbl18_B <- function(geography) {

  # restrict queries to county, and, place
  if (!geography %in% c("county", "place")) {
    stop("county, and place are the only valid inputs to geography")
  }


  if (geography == "county") {

    tbl18B <- read.csv("data-raw/COUNTY_Table18B.csv", stringsAsFactors = FALSE)
    fips_joiner <- tidycensus::fips_codes

    geoid_prefix <- "^05000US"

  }

  else if (geography == "place") {

    tbl18B <- read.csv("data-raw/PLACES_Table18B.csv", stringsAsFactors = FALSE)

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

  data_dictionary <- read.csv("data-raw/CHAS_TBL18_DATA_DICTIONARY.csv", stringsAsFactors = FALSE, sep = "\t", col.names = dd_col_names, header = FALSE)


  tbl18B_data_dictionary <- data_dictionary %>%
    dplyr::filter(Column.Name %in% names(tbl18B)) %>%
    dplyr::filter(Column.Type == "Detail") %>%
    dplyr::arrange(Description.3, Description.4) %>%
    dplyr::rename(CHAS_COLS = Column.Name)

  tbl18B <- tbl18B %>%
    dplyr::select(geoid, name, tbl18B_data_dictionary$CHAS_COLS)

  tbl18B <- tbl18B %>%
    tidyr::gather(key = CHAS_COLS, value = estimate, -geoid, -name) %>%
    dplyr::inner_join(tbl18B_data_dictionary) %>%
    dplyr::group_by(geoid, name, Description.3, Description.4) %>%
    dplyr::summarise(owner_occ_no_mortgage_hu = sum(estimate)) %>%
    dplyr::ungroup()

  tbl18B <- tbl18B %>%
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
  tbl18B <- tbl18B %>%
    dplyr::inner_join(fips_joiner) %>%
    dplyr::rename(state_abbreviation = state)


  if (geography == "place") {
    tbl18B <- tbl18B %>%
      dplyr::select(-county_code)
  }

  return(tbl18B)

}


preprocess_tbl18_C <- function(geography) {

  # restrict queries to county, and, place
  if (!geography %in% c("county", "place")) {
    stop("county, and place are the only valid inputs to geography")
  }


  if (geography == "county") {

    tbl18C <- read.csv("data-raw/COUNTY_Table18C.csv", stringsAsFactors = FALSE)
    fips_joiner <- tidycensus::fips_codes

    geoid_prefix <- "^05000US"

  }

  else if (geography == "place") {

    tbl18C <- read.csv("data-raw/PLACES_Table18C.csv", stringsAsFactors = FALSE)

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
    "data-raw/CHAS_TBL18_DATA_DICTIONARY.csv",
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


  if (geography == "place") {
    tbl18C <- tbl18C %>%
      dplyr::select(-county_code)
  }

  return(tbl18C)

}


# Get Formatted CHAS Data -------------------------------------------------


tbl18A_places <- preprocess_tbl18_A("place")
save(tbl18A_places, file = "data/tbl18A_places.rda")

tbl18A_county <- preprocess_tbl18_A("county")
save(tbl18A_county, file = "data/tbl18A_county.rda")

tbl18B_places <- preprocess_tbl18_B("place")
save(tbl18B_places, file = "data/tbl18B_places.rda")

tbl18B_county <- preprocess_tbl18_B("county")
save(tbl18B_county, file = "data/tbl18B_county.rda")

tbl18C_places <- preprocess_tbl18_C("place")
save(tbl18C_places, file = "data/tbl18C_places.rda")

tbl18C_county <- preprocess_tbl18_C("county")
save(tbl18C_county, file = "data/tbl18C_county.rda")


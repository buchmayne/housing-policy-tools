library(dplyr)
library(stringr)
library(tidycensus)


get_cost_burdening_by_income_and_tenure <- function(year = 2019, geography, state, county = NULL, survey = "acs5") {

  # get census variable names
  acs_vars <- readRDS("data/acs_variables.rds")
  cost_burdening_tbl <- "B25106"

  # pull down data via census api
  cb <- tidycensus::get_acs(
    geography = geography,
    table = cost_burdening_tbl,
    survey = survey,
    year = year,
    state = state,
    county = county,
    cache_table = TRUE
  )

  # join variable names and relabel
  cb <- dplyr::inner_join(cb, acs_vars, by = c("variable" = "name"))

  cb <- cb[c("GEOID", "NAME", "estimate", "moe", "label")]
  colnames(cb) <- c("GEOID", "NAME", "housing_units", "moe", "label")

  # drop row containing totals and subtotals, create tenure column
  cb <- cb %>%
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


  return(cb)


}




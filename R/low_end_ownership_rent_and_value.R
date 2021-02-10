#' Internal function to unify rent and ownership CHAS bins
#'
#' @param renters_data
#' @param owners_data
#' @export

low_end_ownership_rent_and_value <- function(renters_data, owners_data) {


  unified_renters <- renters_data %>%
    dplyr::rename(affordability = rent)

  unified_owners <- owners_data %>%
    dplyr::mutate(home_value = dplyr::case_when(
      home_value == "0-50%" ~ "30-50%",
      home_value == "50-80%" ~ "50-80%",
      home_value == "80-100%" ~ "+80%",
      home_value == "+100%" ~ "+80%"
    )) %>%
    dplyr::group_by(
      geoid,
      name,
      state_code,
      state_abbreviation,
      state_name,
      home_value,
      household_income
    ) %>%
    dplyr::summarise(ownership_units = sum(ownership_units)) %>%
    dplyr::ungroup() %>%
    dplyr::rename(affordability = home_value)

  # need to expand the ownership affordability to include the lower affordability bin
  unified_data <- dplyr::left_join(unified_renters, unified_owners)
  unified_data <- unified_data %>%
    dplyr::mutate(ownership_units = dplyr::if_else(is.na(ownership_units), 0L, ownership_units))

  return(unified_data)

}

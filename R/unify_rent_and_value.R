#' Internal function to unify rent and ownership CHAS bins
#'
#' @param renters_data renters data from CHAS already formatted
#' @param owners_data owners data from CHAS already formatted
#' @export

unify_rent_and_value <- function(renters_data, owners_data) {


  unified_renters <- renters_data %>%
    dplyr::mutate(rent = dplyr::case_when(
      rent == "0-30%" ~ "0-50%",
      rent == "30-50%" ~ "0-50%",
      rent == "50-80%" ~ "50-80%",
      rent == "+80%" ~ "+80%",
    )) %>%
    dplyr::group_by(
      geoid,
      name,
      state_code,
      state_abbreviation,
      state_name,
      rent,
      household_income
    ) %>%
    dplyr::summarise(renter_occ_hu = sum(renter_occ_hu)) %>%
    dplyr::ungroup() %>%
    dplyr::rename(affordability = rent)

  unified_owners <- owners_data %>%
    dplyr::mutate(home_value = dplyr::case_when(
      home_value == "0-50%" ~ "0-50%",
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

  unified_data <- dplyr::inner_join(unified_renters, unified_owners)

  return(unified_data)

}

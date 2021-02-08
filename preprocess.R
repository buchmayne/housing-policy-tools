library(tidyverse)
library(tidycensus)
library(DT)

acs_vars <- tidycensus::load_variables(year = 2019, dataset = "acs5")


tbl_ <- "B25106"


test <- get_acs(geography = "tract", table = tbl_, survey = "acs5", year = 2019, state = "OR")
test <- inner_join(test, acs_vars, by = c("variable" = "name"))

test %>% 
  count(label)


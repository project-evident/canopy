## import all canopy data
library(googlesheets4)
library(readxl)
library(stringr)

import_all = function(
  sheet_id = "10wBWoUNIIEGfJ91yUEHsuSp2FRd4F5o8krSsvB0NMvA",
  tabs = c("Winter 2020-21 School Data", "Fall 2020 School Data"),
  paths = c("data/The-Canopy-Dataset.xlsx"),
  path_tab = c("Confirmed Schools")
) {
  gs_data = lapply(tabs, read_sheet, ss = sheet_id, col_types = "c")
  names(gs_data) = str_extract(tabs, "[0-9]{4}")
  
  xl_data = mapply(read_xlsx, path = paths, sheet = path_tab, MoreArgs = list(col_types = "text"), SIMPLIFY = FALSE)
  names(xl_data) = "2019"
  
  all_data = bind_rows(c(gs_data, xl_data), .id = "year") %>%
    mutate(type = coalesce(
      type,
      case_when(
        charter == "Yes" ~ "Charter",
        school_type == "Regular School" ~ "District",
        !is.na(school_type) ~ "Independent",
        TRUE ~ NA_character_
      )
    )) %>%
    mutate(across(matches("percent"), function(x) {
        x = as.numeric(sub("%", "", x,  fixed = TRUE))
        x = ifelse(x > 1, x / 100, x)
        return(x)
      })) %>%
    mutate(
      leader_tenure = factor(
        leader_tenure,
        levels = c("Less than a year", "1-3 years", "4-6 years", "7-9 years", "10 years or more")
      )
    )
  ## TODO - could use some type cleanup
  return(all_data)
}


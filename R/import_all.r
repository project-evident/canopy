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
  gs_data = lapply(tabs, read_sheet, ss = sheet_id, col_types = "c", na = c("", "-"))
  names(gs_data) = c("2020", "2021")
  
  xl_data = read_xlsx(path = paths, sheet = path_tab, col_types = "text", na = c("", "-"))
  
  tag_renames = c(
    ## new_name = old_name
    "student_count" = "CCD_student_count",
    "design_equity" = "equity",
    "community_supports" = "community_support",
    "assessments_sel" = "measures_sel",
    "physical_well_being" = "physical_health",
    "place_based" = "local_focus",
    "assessments_deeper" = "measures_deeper",
    "interoperability" = "integrated_data",
    "extended_learning" = "outside_credit",
    "assessments_career" = "measures_career"
  )
  
  stopifnot(all(names(tag_renames) %in% c(tag_vec, "student_count")))
  stopifnot(all(tag_renames %in% names(xl_data)))
  
  xl_data = rename(xl_data, all_of(tag_renames))
  
  ## additionally, portfolios_exhibitions = portfolios | exhibitions
  xl_data$portfolios_exhibitions = case_when(
    xl_data$portfolios == "1" | xl_data$exhibitions == "1" ~ "1",
    TRUE ~ NA_character_
  )
  xl_data = select(xl_data, -portfolios, -exhibitions)
  
  
  all_data = bind_rows(c(gs_data, "2019" = list(xl_data)), .id = "year") %>% 
    mutate(type = coalesce(
      type,
      case_when(
        charter == "Yes" ~ "Charter",
        school_type == "Regular School" ~ "District",
        !is.na(school_type) ~ "Independent",
        TRUE ~ NA_character_
      )
    )) %>%
    mutate(
      across(matches("percent"), function(x) {
        x = as.numeric(sub("%", "", x,  fixed = TRUE))
        x = ifelse(x > 1, x / 100, x)
        return(x)
      }),
      across(matches("count"), ~as.numeric(replace(., . == "", NA)))  
    ) %>%
    mutate(
      leader_tenure = factor(
        leader_tenure,
        levels = c("Less than a year", "1-3 years", "4-6 years", "7-9 years", "10 years or more")
      )
    ) 
  ## TODO - could use some more type cleanup
  return(all_data)
}

# canopy = import_all()
# saveRDS(canopy, file = sprintf("data/canopy_all_%s.rds", Sys.Date()))

most_recent_file = function(x, ...) {
  tail(list.files(pattern = x, ..., full.names = TRUE), 1)
}

library(tidygeocoder)

sch_raw %>% 
  select(school_id, school_city, school_state) %>%
  geocode(
    paste(school_city, school_state, sep = ","), method = "census" ## need google for non-address locations
  )
  
ne_states = c(
  "Massachusetts", "New Hampshire", "Connecticut", "Rhode Island", "Maine", "Vermont"
)

filter(canopy, school_state %in% ne_states & `12_grade` %in% "Yes") %>% pull(NCES_id) %>% length

ne_high = filter(canopy, school_state %in% ne_states & `12_grade` %in% "Yes")

ne_high %>% count(locale)
ne_high %>% count(school_state)

## Lots of interest in Canopy vs non-Canopy schools in NE.
## strong concern around equity and equal access.
## big questions would be 
##   are nominated schools predominantly in affluent community?
##   are nom schools in the upper quartile of funding?
##   are nom schools in the upper quartile of diversity?
##   Confirmed schools - will have selected tags to look at, eg
##     HS to career pathways
##

## Questions about 
## Chelsea likes the consideration of 

## Timeline: 
## Mid-end of April have refined questions (Betheny & Chelsea)
## First two weeks of May: analysis and review
## Final product in early June

## Deliverable on our end will be similar to blog entries - 
## we provide visuals/tables, talk 
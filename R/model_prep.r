
### TODO clean-up creation of these columns into import_all.r (or maybe a new prep file)

canopy_latest = canopy %>%
  group_by(school_id) %>%
  filter(year == max(year)) %>%
  ungroup()

sch_demo = canopy_latest %>%
  group_by(school_id) %>%
  select(school_id, matches("_percent"), matches("_grade"), locale, type, student_count, school_state) %>%
  mutate(across(matches("_percent"), ~ as.numeric(str_replace(., "%", "")) / 100)) %>%
  mutate(
    elementary = `1_grade` == "Yes" | `2_grade` == "Yes" | `3_grade` == "Yes" | `4_grade` == "Yes",
    middle = `7_grade` == "Yes",
    high = `10_grade` == "Yes" | `11_grade` == "Yes" | `12_grade` == "Yes",
    #student_count = as.numeric(student_count),
    locale = factor(locale, levels = c("Urban", "Suburban", "Rural"))
  ) %>% 
  ungroup()


continuous_cols = c(
  paste0(c("black", "hispanic", "IDEA", "FRPL"), "_percent"),
  "student_count"
)
#continuous_cols = setdiff(continuous_cols, c("IDEA_percent", "FRPL_percent"))


sch_demo = sch_demo %>% mutate(
 across(any_of(continuous_cols), scale, .names = "{.col}_scaled")
)


sch_demo$charter_fl = as.integer(sch_demo$type == "Charter")

canopy_latest = canopy_latest %>%
  mutate(across(any_of(tag_vec), ~ !is.na(.)))

logistic_data =
  canopy_latest %>% 
  select(school_id, any_of(tag_vec), year) %>% 
  left_join(sch_demo, by = "school_id") %>%
  mutate(
    locale_urban = as.integer(locale == "Urban"),
    locale_suburban = as.integer(locale == "Suburban"),
    locale_rural = as.integer(locale == "Rural")
  )

scaled_cols = paste0(continuous_cols, "_scaled")
features = c(scaled_cols, "elementary", "middle", "high", "charter_fl", "locale")
features = features[!str_detect(features, "IDEA|FRPL")]


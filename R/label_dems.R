### Demographic labels ####

dem_cols = c(
  "student_count",
  "CCD_student_count",
  "non_white_percent",
  "black_count",
  "black_percent",
  "hispanic_count",
  "hispanic_percent",
  "FRPL_count",
  "FRPL_percent",
  "LEP_percent",
  "IDEA_percent",
  "charter",
  "level_elem",
  "level_middle", 
  "level_high",
  "level_simple",
  "locale_urban",
  "locale_suburban",
  "locale_rural"
)

dem_labs = dem_cols
names(dem_labs) = c(
  "Number of students",
  "Number of students",
  "% students of color",
  "# Black students",
  "% Black students",
  "# Hispanic students",
  "% Hispanic student",
  "# FRPL eligible",
  "% FRPL eligible",
  "% English Language Learner",
  "% special education",
  "Charter schools",
  "Elementary schools",
  "Middle schools",
  "High schools",
  "Level",
  "Urban schools",
  "Suburban schools",
  "Rural schools"
)

dem_labs_rv = names(dem_labs)
names(dem_labs_rv) = dem_labs

label_dems = function(dems) dem_labs_rv[dems]
scale_x_demo = scale_x_discrete(labels = dem_labs_rv)

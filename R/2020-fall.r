library(tidyverse)
library(qualtRics)

source("R/branding.R")
tags = read_csv("data/Canopy Tags Public Access.csv")

y2_new = read_survey("data/School Survey - New Nominees_September 17, 2020_07.50 mod.csv")
y2_new = filter(y2_new, Finished, `Response Type` == "IP Address")
## 95 records
##  - 1 spam, 1 preview = 93 records
##  - 5 no school name = 88 records
##  - 15 didn't complete = 78 records (though some of those got through the most of it...)
## 78 complete, not spam or preview


y2_return = read_survey("data/School Survey_September 17, 2020_07.48.csv")
y2_ret_q = filter(y2_return, Finished, Status == "IP Address") ##??
## 61 records
##  - 5 survey previews = 56 records
##  - 4 unfinished (1 of which is very close!!)
## = 52 complete records, but 53 *nearly* complete...



# y2_ret_van = read_csv("data/School Survey_September 17, 2020_07.48.csv")
# y2_ret_van = y2_ret_van[-1, ]
# y2_ret_van = type_convert(y2_ret_van)
# y2_ret_van %>%
#   filter(Finished) %>%
#   nrow
# which(names(y2_return) %in% tags$`Tag name`)
# mutate(tags, present = `Tag name` %in% names(y2_ret_van)) %>% View()

t1_raw = c(
  y2_new[["The Big Picture    Is your school implementing any of these general approaches at the start of SY2020-21? Please select all that apply. Hint: Hover your mouse over any of the response options to see a description of the term or phrase."]],
  y2_ret_q[["Tier1A"]],
  y2_ret_q[["Tier1B"]]
)

t1 = t1_raw %>% str_split(pattern = ",") %>% unlist


t1_dat = table(t1) %>% as.data.frame 

t1_new_plot = ggplot(t1_dat, aes(x = reorder(t1, -Freq), y = Freq)) + 
  geom_col(fill = cc_cols["green"]) + 
  bar_y_scale_count +
  labs(x = "", y = "Number of schools", 
       title = "Counts of schools confirming General Approaches") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        panel.grid.major.x = element_blank(),
        plot.margin = margin(t = 8, r = 8, b = 8, l = 40, unit = "pt"))

ggsave_cc(t1_new_plot, file = "Tier 1 Frequency 2020", dir = "reporting/2020", write_data = TRUE)

covid_raw = c(
  y2_ret_q[["Covid1"]],
  y2_ret_q[["Covid3"]],
  y2_ret_q[["Covid2"]],
  y2_ret_q[["Covid3b"]],
  y2_new[["Learning during COVID-19   Select the learning modality (or modalities) your school is implementing at the start of SY2020-21:    Hint: hover your mouse over any of the response options to see a description of the term or phrase."]],
  y2_new[["Is your school implementing any of the following practices at the start of SY2020-21?"]],
  y2_new[["Select if either of the following is true at your school at the start of SY2020-21"]],
  y2_new[["Select if you are using any of the following types of rotating schedules at the start of SY2020-21:"]]
)

covid = covid_raw %>% str_split(pattern = ",") %>% unlist %>% na.omit

covid_dat = table(covid) %>% as.data.frame()

covid_plot = ggplot(covid_dat, aes(x = reorder(covid, -Freq), y = Freq)) + 
  geom_col(fill = cc_cols["light blue"]) + 
  bar_y_scale_count +
  labs(x = "", y = "Number of schools", 
       title = "Counts of schools confirming\nCOVID-related best practices") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        panel.grid.major.x = element_blank(),
        plot.margin = margin(t = 8, r = 8, b = 8, l = 40, unit = "pt"))

covid_plot
ggsave_cc(covid_plot, file = "Covid Tag Frequency 2020", dir = "reporting/2020", write_data = TRUE)


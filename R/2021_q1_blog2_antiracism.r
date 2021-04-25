# Charts related to anti-racism and equity. (Needed to start Blog 2 draft around
# March 5) We talked about this a bit earlier this month...see here. I put
# together a list of equity-related tags that might be fruitful to investigate
# across demographics and other contextual factors.

library(tidyverse)
library(googlesheets4)

source("R/import_tags.r")
source("R/import_all.r")
source("R/branding.R")
source("R/import_all.r")
source("R/model_functions.r")

tags = import_tags()
tag_vec = tags$tag
source("R/model_prep.r") # creates logistic_data object

out_folder = "reporting/2021Q1/blog2-antiracism/"

canopy = readRDS(most_recent_file("canopy_all", path = "data/"))


anti_racism_tags = c(
  "design_equity",
  "anti_racist_action", ## missing in 2019
  "culturally_relevant",
  "restorative_practice",
  "trauma_informed",  ## missing in 2019
  "reallocation_resources",
  "hiring_equity",   ## missing in 2019
  "social_justice", ## missing in 2019
  "elimination_tracking"#,
  #"equity"   ## renamed to design_equity
)

anti_racism_adjacent_tags = c(
  "community_supports",
  "mental_health",
  "physical_well_being",
  "open_hours",
  "dual_language",
  "families_leaders"
)


all(anti_racism_tags %in% tag_vec)
all(anti_racism_adjacent_tags %in% tag_vec)

## Analytic ideas:
## odds ratios for anti-racist action + tags in the SEL/Equity cluster 
##   across geography, demographics, charter/district
## racial demographics of schools implementing anti-racism (maybe separate post?)

logistic_data$ar_tag_count = rowSums(logistic_data[anti_racism_tags], na.rm = TRUE)
canopy_latest$ar_tag_count = rowSums(!is.na(canopy_latest[anti_racism_tags]))
canopy_latest$ar_adj_count = rowSums(!is.na(canopy_latest[anti_racism_adjacent_tags]))
  
ggplot(filter(logistic_data, year != "2019"), aes(x = ar_tag_count)) +
  geom_bar(fill = cc_cols[5]) +
  bar_theme +
  bar_y_scale_count +
  scale_x_continuous(breaks = c(0, 3, 6, 9)) +
  labs(
    x = "Number of equity-focused practices",
    y = "Count of Canopy schools (2020-2021)",
    title = "Distribution of equity-focused practices"
  ) -> ar_counts
ggsave_cc(ar_counts, file = "distribution of ar tags", dir = out_folder)


ggplot(filter(canopy_latest, year != "2019"), aes(x = ar_adj_count)) +
  geom_bar(fill = cc_cols[5]) +
  bar_theme +
  bar_y_scale_count +
  #scale_x_continuous(breaks = c(0, )) +
  labs(
    x = "Number of equity-adjacent practices",
    y = "Count of Canopy schools (2020-2021)",
    title = "Distribution of equity-adjacent practices"
  ) -> ar_adj_counts
ggsave_cc(ar_adj_counts, file = "distribution of ar adjacent tags", dir = out_folder)


ggplot(filter(canopy_latest, year != "2019"), aes(x = ar_tag_count, fill = locale)) +
  geom_bar() +#fill = cc_cols[5]) +
  bar_theme +
  bar_y_scale_count +
  scale_x_continuous(breaks = c(0, 3, 6, 9)) +
  scale_fill_manual(values = locale_cols, na.value = "gray80") +
  labs(
    x = "Number of equity-focused practices",
    y = "Count of Canopy schools (2020-2021)",
    title = "Distribution of equity-focused practices by locale"
  ) -> ar_counts_locale
ggsave_cc(ar_counts_locale, file = "distribution of ar tags by locale", dir = out_folder)

filter(canopy_latest, year != "2019") %>%
  count(locale, ar_tag_count) %>% 
  ggplot(aes(x = ar_tag_count, y = n, color = locale)) +
  geom_line() +
  geom_point(size = 3) + 
  bar_theme +
  bar_y_scale_count +
  scale_x_continuous(breaks = c(0, 3, 6, 9)) +
  scale_color_manual(values = locale_cols, na.value = "gray80") +
  coord_cartesian(clip = "off") +
  labs(
    x = "Number of equity-focused practices",
    y = "Count of Canopy schools (2020-2021)",
    title = "Distribution of equity-focused practices by locale"
  ) -> ar_counts_locale_point
ggsave_cc(ar_counts_locale_point, file = "distribution of ar tags by locale scatterplot", dir = out_folder)

filter(canopy_latest, year != "2019") %>%
  count(locale, ar_tag_count) %>% 
  group_by(locale) %>%
  mutate(prop = n / sum(n)) %>%
ggplot(aes(x = ar_tag_count, y = prop, fill = locale)) +
  geom_col() +
  bar_theme +
  scale_y_continuous(
    limits = c(0, .25),
    breaks = c(0, .1, .2),
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0))
  )  +
  scale_x_continuous(breaks = c(0, 3, 6, 9)) +
  scale_color_manual(values = locale_cols, na.value = "gray80") +
  #coord_cartesian(clip = "off") +
  labs(
    x = "Number of equity-focused practices",
    y = "Percent of Canopy schools (2020-2021)",
    title = "Distribution of equity-focused practices by locale"
  ) +
  facet_wrap(~locale)-> ar_counts_locale_prop
ggsave_cc(ar_counts_locale_prop, file = "distribution of ar tags by locale proportion", dir = out_folder)



ggplot(filter(logistic_data, year != "2019"), aes(x = ar_tag_count)) +
  geom_bar(fill = cc_cols[5]) +
  bar_theme +
  bar_y_scale_count +
  scale_x_continuous(breaks = c(0, 3, 6, 9)) +
  facet_wrap(~locale) +
  labs(
    x = "Number of equity-focused practices",
    y = "Count of Canopy schools (2020-2021)",
    title = "Distribution of equity-focused practices"
  ) -> ar_counts_facet
ggsave_cc(ar_counts_facet, file = "distribution of ar tags by locale", dir = out_folder)


#build_linear_model(logistic_data, responses = "ar_tag_count")
logistic_data$year2019 = as.integer(logistic_data$year == "2019")

## This was a try to get more data points, but if we don't know locale there's
## a lot we don't know and they get omitted anyway
logistic_data$locale_no_na = factor(coalesce(logistic_data$locale, "Locale unk"), levels = c("Urban", "Suburban", "Rural", "Locale unk"))

mod_dat = logistic_data %>% select(year2019, black_percent_scaled, hispanic_percent_scaled,
                         student_count_scaled, elementary, middle, high, charter_fl, locale)

ar_mod = stan_lm(
  ar_tag_count ~ black_percent_scaled + hispanic_percent_scaled + 
    student_count_scaled + elementary + middle + high + charter_fl + 
    locale + year2019,
  data = logistic_data,
  prior = R2(location = 0.3)
)

ar_no_locale = stan_lm(
  ar_tag_count ~ black_percent_scaled + hispanic_percent_scaled + 
    student_count_scaled + elementary + middle + high + charter_fl + year2019,
  data = logistic_data,
  prior = R2(location = 0.3)
)




# ar_mod2 = stan_lm(
#   ar_tag_count ~ black_percent_scaled + hispanic_percent_scaled + 
#     student_count_scaled + elementary + middle + high + charter_fl + 
#     locale_no_na + year2019,
#   data = logistic_data,
#   prior = R2(location = 0.4)
# )


library(broom)
ar_mod %>% coef %>% 
    as.data.frame() %>% 
    set_names("estimate") %>%
    rownames_to_column(var = "term") %>%
    filter(!term %in% c("(Intercept)", "year2020", "year2021", "year2019")) %>%
    dplyr::mutate(
      #nice_tag = label_tags(response), 
      term = str_replace(term, "_scaled|TRUE|_fl", ""),
      term = if_else(term %in% c("middle", "high", "elementary"), paste0("level_", term), term),
      term = str_replace(term, "elementary", "elem"),
      term = str_replace(term, "locale", "locale_"),
      term = if_else(str_detect(term, "locale"), tolower(term), term),
      nice_demog = factor(label_dems(term), levels = unique(dem_labs_rv))
    ) -> ar_coefs

ggplot(
  ar_coefs, 
  aes(x = estimate, y = fct_reorder(nice_demog, estimate))
) + 
  geom_col(fill = cc_cols[3]) + 
  labs(y = "",
       x = "Average change in number of equity-focused practices", 
       title = "Association between school characteristics\nand equity-focused practices",
       subtitle = "(compared to urban schools with average student demographics)") +
  # scale_y_continuous(
  #   trans = "log",
  #   breaks = c(.125, .25, .5, 1, 2, 4, 8),
  #   labels = c("1/8", "1/4", "1/2", "1", "2", "4", "8"),
  #   expand = expansion(0, .1)
  # ) +
  #coord_flip() +
  guides(fill = FALSE) +
  theme(#axis.text = element_text(size = 8), strip.text = element_text(size = rel(0.6)),
        panel.grid.major.y = element_blank()#,
        #axis.text.x = element_text(angle = -90, vjust = 0.5, hjust = 0)
      ) ->
  ar_coef_plot

ggsave_cc(ar_coef_plot, file = "antiracist coefficients", dir = out_folder)


ar_locale_interaction = stan_lm(
  ar_tag_count ~ (black_percent_scaled + hispanic_percent_scaled) * locale + 
    student_count_scaled + elementary + middle + high + charter_fl + year2019,
  data = logistic_data,
  prior = R2(location = 0.3)
)

ar_locale_interaction %>% coef %>% 
    as.data.frame() %>% 
    set_names("estimate") %>%
    rownames_to_column(var = "term") %>%
    filter(!term %in% c("(Intercept)", "year2020", "year2021", "year2019")) %>%
    dplyr::mutate(
      #nice_tag = label_tags(response), 
      term = str_replace(term, "_scaled|TRUE|_fl", ""),
      term = if_else(term %in% c("middle", "high", "elementary"), paste0("level_", term), term),
      term = str_replace(term, "elementary", "elem"),
      term = str_replace(term, "locale", "locale_"),
      term = if_else(str_detect(term, "locale"), tolower(term), term),
      nice_demog = factor(coalesce(label_dems(term), term))
    ) -> ar_interact_coefs

ggplot(
  ar_interact_coefs, 
  aes(x = estimate, y = fct_reorder(nice_demog, estimate))
) + 
  geom_col(fill = cc_cols[3]) + 
  labs(y = "",
       x = "Average change in number of equity-focused practices", 
       title = "Association between school characteristics\nand equity-focused practices",
       subtitle = "(compared to urban schools with average student demographics)") +

  guides(fill = FALSE) +
  theme(#axis.text = element_text(size = 8), strip.text = element_text(size = rel(0.6)),
        panel.grid.major.y = element_blank()#,
        #axis.text.x = element_text(angle = -90, vjust = 0.5, hjust = 0)
      ) ->
  ar_interaction_plot
#ar_frpl_plot
ggsave_cc(ar_interaction_plot, file = "antiracist coefficients with interaction",
          dir = out_folder, fig_width = 11)



## adding in FRPL precent
ar_frpl = stan_lm(
  ar_tag_count ~ (black_percent_scaled + hispanic_percent_scaled) * locale + FRPL_percent_scaled +
    student_count_scaled + elementary + middle + high + charter_fl + year2019,
  data = logistic_data,
  prior = R2(location = 0.3)
)

ar_frpl %>% coef %>% 
    as.data.frame() %>% 
    set_names("estimate") %>%
    rownames_to_column(var = "term") %>%
    filter(!term %in% c("(Intercept)", "year2020", "year2021", "year2019")) %>%
    dplyr::mutate(
      #nice_tag = label_tags(response), 
      term = str_replace(term, "_scaled|TRUE|_fl", ""),
      term = if_else(term %in% c("middle", "high", "elementary"), paste0("level_", term), term),
      term = str_replace(term, "elementary", "elem"),
      term = str_replace(term, "locale", "locale_"),
      term = if_else(str_detect(term, "locale"), tolower(term), term),
      nice_demog = factor(coalesce(label_dems(term), term))
    ) -> ar_frpl_coefs

ggplot(
  ar_frpl_coefs, 
  aes(x = estimate, y = fct_reorder(nice_demog, estimate))
) + 
  geom_col(fill = cc_cols[3]) + 
  labs(y = "",
       x = "Average change in number of equity-focused practices", 
       title = "Association between school characteristics\nand equity-focused practices",
       subtitle = "(compared to urban schools with average student demographics)") +

  guides(fill = FALSE) +
  theme(#axis.text = element_text(size = 8), strip.text = element_text(size = rel(0.6)),
        panel.grid.major.y = element_blank()#,
        #axis.text.x = element_text(angle = -90, vjust = 0.5, hjust = 0)
      ) ->
  ar_frpl_plot
#ar_frpl_plot
ggsave_cc(ar_frpl_plot, file = "antiracist coefficients with interaction and FRPL",
          dir = out_folder, fig_width = 11)

## creating prediction plot for interaction mode
logistic_data$white_percent = 100 * logistic_data$white_percent
ar_locale_interaction_nw = stan_lm(
  ar_tag_count ~ white_percent * locale + 
    student_count_scaled + elementary + middle + high + charter_fl + year2019,
  data = logistic_data,
  prior = R2(location = 0.3)
)
summary(ar_locale_interaction_nw, digits = 2)

pred_data = expand.grid(
  white_percent = c(.1, .9),
  locale = unique(logistic_data$locale)
) %>% na.omit %>%
  mutate(
    student_count_scaled = 0,
    elementary = FALSE,
    middle = FALSE,
    high = FALSE,
    charter_fl = 0,
    year2019 = 0
  )

pred = posterior_predict(ar_locale_interaction_nw, newdata = pred_data, )
apply(pred, 2, quantile, c(.1, .5, .9)) %>% t %>%
  cbind(pred_data, .) -> pred_data

ggplot(pred_data, aes(x = white_percent, y = `50%`, color = locale, fill = locale)) +
  #geom_ribbon(aes(ymin = `10%`, ymax = `90%`), alpha = 0.2, color = NA) +
  geom_line(size = 2) +
  scale_fill_locale +
  scale_color_locale +
  scale_x_continuous(labels = scales::label_percent(), limits = c(0, 1), expand = c(0, 0)) +
  scale_y_continuous(breaks = c(0, 2, 4, 6, 8), limits = c(0, 9), expand = c(0, 0)) + 
  labs(
    x = "Percent of student body that is white",
    y = "Number of equity-focused practices",
    title = "Modeled association of white %\non number of equity-focused practices"
  ) -> ar_locale_slopes
  
ggsave_cc(plot = ar_locale_slopes, file = "locale slopes", dir = out_folder)
ggsave_cc(
  plot = ar_locale_slopes + 
    geom_point(data = logistic_data, aes(y = ar_tag_count), 
               position = position_jitter(height = 0.2), alpha = 0.6),
  file = "locale slopes with raw data", dir = out_folder
)
  

## Mapping
loc = read_tsv("data/Canopy School Lat Long.tsv", col_names = c("lat", "lon", "school_name"), skip = 1)
loc = loc %>%
  mutate(
    school_name = case_when(
      school_name == "Nuvu" ~ "Nuvu Studio",
      school_name == "Two Rivers Public Charter School - 4th Street Campus" ~ "Two Rivers Public Charter School - 4th St",
      TRUE ~ school_name
    )
  )
if(length(setdiff(loc$school_name, canopy_latest$school_name))) stop("Location school names don't line up")
# "Mountain Academy at Teton Science School" problematic longitude
canopy_latest = canopy_latest %>% left_join(loc, by = "school_name")

#library(ggmap)
#library(tmap)
# ggmap(canopy_latest, aes(x = lon, y = lat, color = ar_tag_count))

# us <- c(left = -125, bottom = 25.75, right = -67, top = 49)
# get_stamenmap(us, zoom = 5, maptype = "toner-lite") %>% 
#   ggmap() +
#   geom_point(aes(x = lon, y = lat, color = ar_tag_count, size = ar_tag_count),
#         data = canopy_latest %>% filter(lon > -125)) +
#   scale_color_distiller(type = "div") +
#   theme_map()
library(sf)

canopy_latest %>%
  filter(!is.na(lat) & !is.na(lon)) %>% 
  mutate(ar_tag_count_factor = factor(ar_tag_count)) %>%
  st_as_sf(crs = "+proj=longlat +datum=WGS84", coords = c("lon", "lat")) ->
  canopy_sf

# library(tmap) 
# tmap_mode("view")
# tm_basemap(server = leaflet::providers$Stamen.TonerLite) +
#   tm_shape(canopy_sf) +
#   tm_bubbles(
#     size = "ar_tag_count", 
#     col = "ar_tag_count"
#   ) -> ar_map

#htmlwidgets::saveWidget(ar_map, file = paste0(out_folder, "ar_map.html"), selfcontained = TRUE)


library(leaflet)
library(htmltools)
pal <- colorNumeric(
  palette = c(cc_cols["light blue"], cc_cols["dark blue"]),
  domain = c(0, 9)
  #domain = c("Few equity-focused practices", "Many equity-focused practices")
)

library(scales)
canopy_sf = canopy_sf %>%
  mutate(
    across(matches("percent"), .fns = percent, accuracy = 1,
           .names = "pct_{.col}")
  )

leaflet(canopy_sf) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(
    radius = 6,
    color = ~pal(ar_tag_count),
    stroke = FALSE, 
    #color = "gray80",
    fillOpacity = 0.75,
    popup = ~(
      glue(
        "{school_name}",
        "# of equity-focused tags: {ar_tag_count}",
        "Locale: {locale}",
        "% Black: {pct_black_percent}",
        "% Hispanic: {pct_hispanic_percent}",
        .sep = "<br/>"
      )
    )
  ) %>%
  addLegend("bottomright", pal = pal, values = ~ar_tag_count,
    title = "# of equity-focused</br>practices",
    opacity = 0.8
  ) -> ar_leaflet

htmlwidgets::saveWidget(ar_leaflet, file = here::here(out_folder, "ar_leaflet_map.html"), selfcontained = TRUE)


## designing for equity and culture of antiracist practice - logistic regression

ar_log_mods = build_model(
  logistic_data, responses = c("design_equity", "anti_racist_action")
)

plot_bayes_coefs(ar_log_mods) %>%
  ggsave_cc(file = "logistic_model_comparison", dir = out_folder)

plot_one_mod(
  filter(ar_log_mods, response == "design_equity"), title = "Designing for Equity Odds Ratios"
) %>%
  ggsave_cc(file = "odds ratios design_equity", dir = out_folder)

plot_one_mod(
  filter(ar_log_mods, response == "anti_racist_action"), title = "Anti-Racist Action for Equity Odds Ratios"
) %>%
  ggsave_cc(file = "odds ratios anti_racist_action", dir = out_folder)


canopy_latest %>%
  filter(year != "2019") %>%
  group_by(locale) %>%
  summarize(avg_ar_practices = mean(ar_tag_count))

# # A tibble: 4 x 2
#   locale   avg_ar_practices
#   <chr>               <dbl>
# 1 Rural                2.02
# 2 Suburban             2.69
# 3 Urban                2.62
# 4 NA                   2.47

canopy_latest %>% filter(year >= "2020") %>%
  select(school_id, any_of(tag_vec)) %>%
  pivot_longer(
    cols = any_of(tag_vec),
    names_to = "tag",
    values_to = "val"
  ) %>%
  filter(!is.na(val)) %>%
  mutate(
    is_central = if_else(val %in% "1", "Not central", "Central")
  ) %>%
  count(tag, is_central) %>%
  group_by(tag) %>%
  mutate(
    percent_central = percent(1 - n[is_central == "Not central"] / sum(n), accuracy = 1)
  ) %>%
  ungroup() %>%
  mutate(
    tag_label = fct_reorder(label_tags(tag), n, sum, .desc = TRUE)
  ) %>%
  ggplot(aes(x = tag_label, y = n, fill = is_central, text = percent_central)) +
  geom_col() +
  scale_x_discrete(labels = NULL) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = rel(0.8)),
    legend.position = c(.84, .78),
    plot.margin = margin(t = 8, r = 8, b = 8, l = 20, unit = "pt")
  ) -> central_all

library(plotly)
central_all_interactive = ggplotly(central_all)

library(htmlwidgets)
partial_bundle(central_all_interactive) %>% 
  saveWidget(file = here::here(out_folder, "central_tags_all_interactive.html"))

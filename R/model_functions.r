library(rstanarm)
library(broom)
library(tidyverse)

build_model = function(logistic_data, responses) {
  formulas = sprintf("%s ~ %s", responses, paste(features, collapse = " + "))
   bayes_mods = list()

  for(i in seq_along(formulas)) {
    message("\n\n\nSTARTING MODEL ", i, " OF ", length(formulas), "\n\n\n")
    bayes_mods[[responses[i]]] = stan_glm(
      formulas[i],
      data = logistic_data,
      family = binomial(link = "logit"),
      prior = student_t(
        df = 7,
        location = 0,
        scale = 2.5
      )
    )
  }

  bayes_tidy = lapply(bayes_mods, broom.mixed::tidy) %>%
    bind_rows(.id = "response") %>%
    filter(term != "(Intercept)") %>%
    dplyr::mutate(
      nice_tag = label_tags(response), 
      term = str_replace(term, "_scaled|TRUE|_fl", ""),
      term = if_else(term %in% c("middle", "high", "elementary"), paste0("level_", term), term),
      term = str_replace(term, "elementary", "elem"),
      term = str_replace(term, "locale", "locale_"),
      term = if_else(str_detect(term, "locale"), tolower(term), term),
      nice_demog = factor(label_dems(term), levels = unique(dem_labs_rv))
    )
  bayes_tidy
}


### Plotting

plot_bayes_coefs = function(bayes_tidy, title = "Coefficients") {
  bayes_facet_dat = bayes_tidy %>% group_by(nice_demog) %>%
    arrange(desc(abs(estimate)))# %>%
  bayes_facet = 
  ggplot(bayes_facet_dat,
    aes(y = exp(estimate),
        x = reorder_within(nice_tag, estimate, within = nice_demog, fun = mean),
    )) +
  geom_col() + 
  labs(y = "Odds multiplier",
       x = "", 
       #fill = "Demographic",
       title = title) +
  facet_wrap(~ nice_demog, scales = "free_y") +
  scale_y_continuous(
    trans = "log",
    breaks = c(.125, .25, .5, 1, 2, 4, 8),
    labels = c("1/8", "1/4", "1/2", "1", "2", "4", "8"),
    expand = expansion(0, .1)
  ) +
  scale_x_reordered() + 
  coord_flip() +
  guides(fill = FALSE) +
  theme(axis.text = element_text(size = 8), strip.text = element_text(size = rel(0.6)),
        panel.grid.major.y = element_blank(),
        axis.text.x = element_text(angle = -90, vjust = 0.5, hjust = 0))

  bayes_facet
}


plot_one_mod = function(bayes_tidy, title = "") {
  bayes_facet_dat = bayes_tidy %>% group_by(nice_demog) %>%
    arrange(desc(abs(estimate)))# %>%
  ggplot(bayes_facet_dat,
    aes(y = exp(estimate),
        x = fct_reorder(nice_demog, estimate),
    )) +
  geom_col() + 
  labs(y = "Odds multiplier",
       x = "", 
       title = title) +
  scale_y_continuous(
    trans = "log",
    breaks = c(.125, .25, .5, 1, 2, 4, 8),
    labels = c("1/8", "1/4", "1/2", "1", "2", "4", "8"),
    expand = expansion(0, .1)
  ) +
  coord_flip() +
  guides(fill = FALSE) +
  theme(axis.text = element_text(size = 8), strip.text = element_text(size = rel(0.6)),
        panel.grid.major.y = element_blank(),
        axis.text.x = element_text(angle = -90, vjust = 0.5, hjust = 0))

}




build_linear_model = function(data, responses) {
  formulas = sprintf("%s ~ %s", responses, paste(features, collapse = " + "))
   bayes_mods = list()

  for(i in seq_along(formulas)) {
    message("\n\n\nSTARTING MODEL ", i, " OF ", length(formulas), "\n\n\n")
    bayes_mods[[responses[i]]] = stan_lm(
      formulas[i],
      data = data,
      prior = R2(
        location = 0.4
      )
    )
  }

  bayes_tidy = lapply(bayes_mods, broom.mixed::tidy) %>%
    bind_rows(.id = "response") %>%
    filter(term != "(Intercept)") %>%
    dplyr::mutate(
      nice_tag = label_tags(response), 
      term = str_replace(term, "_scaled|TRUE|_fl", ""),
      term = if_else(term %in% c("middle", "high", "elementary"), paste0("level_", term), term),
      term = str_replace(term, "elementary", "elem"),
      term = str_replace(term, "locale", "locale_"),
      term = if_else(str_detect(term, "locale"), tolower(term), term),
      nice_demog = factor(label_dems(term), levels = unique(dem_labs_rv))
    )
  bayes_tidy
}


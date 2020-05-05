load(here("data", "cleaned.rdata"))
source(here("R/branding.R"))

nom_conf[nom_conf$school_id == 130, "nom_approach"] = "T1_only"
nom_conf = filter(nom_conf, !(school_id == 130 & tier == "T2"))

gen_dir = "graphs/General Descriptives"

## Basic hist ####

cl_sch = conf_long %>% group_by(school_id) %>%
  summarize(n_tag = sum(value)) %>%
  left_join(select(dems, school_id, charter))

cl_sch_plot = ggplot(cl_sch, aes(x = n_tag)) +
    geom_histogram(binwidth = 1, fill = cc_cols["green"]) + 
    labs(x = "Number of confirmed tags", y = "Number of schools",
         title = "Distribution of number of confirmed tags per school") +
    scale_y_continuous(breaks = seq(0, 8, by = 2), expand = expansion(c(0, 0.1))) +
    scale_x_continuous(breaks = function(x) {seq(0, max(x) + 9, by = 10)}) +
    theme(panel.grid.major.x = element_blank())
ggsave_cc(cl_sch_plot, file = "Distribution of tags by school", dir = gen_dir)


## Confirmation T1 ####

nom_conf_overall = nom_conf %>%
  group_by(tier) %>%
  summarize(
    accuracy = mean(nom == conf),
    precision = sum(conf == 1 & nom == 1) / sum(nom == 1),
    sensitivity = sum(conf == 1 & nom == 1) / sum(conf == 1),
    n_nom_confirmed = sum(conf == 1 & nom == 1),
    n_tag_added = sum(conf == 1 & nom == 0),
    n_tag_removed = sum(conf == 0 & nom == 1)
  ) 

school_acc = nom_conf %>%
  group_by(school_id) %>%
  summarize(accuracy = mean(nom == conf), 
            n_t1_conf = sum(tier == "T1" & conf),
            n_t2_conf = sum(tier == "T2" & conf),
            precision = mean(conf == 1 / (conf == 1))) %>%
  arrange((accuracy), desc(n_t1_conf), desc(n_t2_conf))

mean(school_acc$accuracy == 1)


nom_conf = mutate(nom_conf, decision = case_when(
  nom & conf ~ "confirmed",
  nom & ! conf ~ "removed",
  !nom & conf ~ "added",
  TRUE ~ NA_character_
))


school_acc_t1 = nom_conf %>%
  filter(tier == "T1") %>%
  group_by(school_id) %>%
  summarize(accuracy = mean(nom == conf), 
            n_t1_conf = sum(tier == "T1" & conf)) %>%
  arrange((accuracy), desc(n_t1_conf))

mean(school_acc_t1$accuracy)

decision_cols = cc_cols[c("light blue", "red", "green")]
names(decision_cols) = c("confirmed", "removed", "added")

div_t1 = nom_conf %>% filter(nom_approach == "T1_only") %>%
  group_by(school_id) %>%
  mutate(sch_tags = sum(decision %in% c("confirmed", "added"), na.rm = TRUE),
         sch_added = sum(decision == "added", na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(sch_tags), desc(sch_added), school_id) %>%
  mutate(school_id = fct_reorder(as.character(school_id), -conf, sum),
         tag = fct_reorder(tag, conf, sum)) 

gg_t1_nom_conf =
  ggplot(filter(div_t1,!is.na(decision)), aes(
    y = tag,
    x = factor(school_id),
    fill = decision
  )) +
  geom_tile() +
  theme(
    axis.text.x = element_blank(),
    legend.position = "bottom",
    panel.grid.major = element_blank()
  ) +
  guides(fill = guide_legend(label.position = "bottom", direction = "horizontal")) +
  labs(x = "School",
       title = "Tier 1 Tag Confirmations",
       y = "",
       fill = "Tag Action") +
  scale_fill_manual(values = decision_cols, na.value = "white") +
  scale_y_discrete(labels = label_tags) #+
  #coord_fixed(ratio = 1.25)
gg_t1_nom_conf

ggsave_cc(gg_t1_nom_conf, file = "T1 Tag Confirmations", dir = gen_dir, fig_height = 5, fig_width = 12)


## Confirmation T2 ####

div_t2 = 
  nom_conf %>% filter(nom_approach != "T1_only") %>%
  group_by(school_id) %>%
  mutate(sch_tags = sum(decision %in% c("confirmed", "added"), na.rm = TRUE),
         sch_added = sum(decision == "added", na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(sch_tags), desc(sch_added), school_id) %>%
  mutate(school_id = fct_reorder(as.character(school_id), -conf, sum),
         tag = fct_reorder(tag, -conf, sum)) 

gg_t2_nom_conf = 
    ggplot(filter(div_t2, !is.na(decision)), aes(x = tag, y = school_id, fill = decision)) +
    geom_tile() +
  #  scale_fill_manual()
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = rel(0.6)),
        axis.text.y = element_blank(),
        panel.grid.major = element_blank()) +
  labs(y = "School", title = "Tier 2 Tag Confirmations", x = "", fill = "Tag Action") +
  scale_x_discrete(labels = label_tags) +
  scale_fill_manual(values = decision_cols, na.value = "white") #+
  #coord_fixed(ratio = 0.7)
gg_t2_nom_conf

ggsave_cc(gg_t2_nom_conf, file = "T2 Tag Confirmations", dir = gen_dir, fig_height = 7, fig_width = 13)

## a few schools seem to have identical tagging data...
dupes = div_t2 %>%
  group_by(school_id) %>%
  filter(conf == 1) %>%
  arrange(tag) %>%
  summarize(sch_tags = paste(tag, collapse = ",")) %>%
  group_by(sch_tags) %>%
  filter(n() > 1) %>%
  arrange(school_id) %>%
  summarize(schools = paste(school_id, collapse = ","))
unlist(dupes)
## and they're all in Mississippi?


# most added tags
nom_conf_added = nom_conf %>%
  group_by(tier, tag) %>%
  summarize(n_nom_confirmed = sum(nom == 1 & conf == 1),
            n_tag_added = sum(nom == 0 & conf == 1),
            n_tag_removed = sum(nom == 1 & conf == 0)) %>%
  group_by(tier) %>%
  arrange(tier, desc(n_tag_added), desc(n_nom_confirmed)) 

most_addded_plot = nom_conf %>% left_join(dems) %>%
  filter(decision == "added" & !is.na(charter)) %>%
  count(tag, charter) %>%
  group_by(tag) %>%
  mutate(n_tag_added = sum(n)) %>%
  ungroup() %>%
  arrange(desc(n_tag_added)) %>%
  slice(1:20) %>%
  mutate(tag = fct_inorder(tag)) %>%
  ggplot(aes(x = tag, y = n, fill = charter)) +
  geom_col(position = "dodge") + 
  scale_x_discrete(labels = label_tags) +
  bar_y_scale_count +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1, size = rel(0.8)),
        panel.grid.major.x = element_blank(), 
        plot.margin = margin(t = 8, r = 8, b = 8, l = 50, unit = "pt"))  +
  scale_fill_charter +
  labs(title = "Most added tags by school type",
       x = "", y = "Number of schools",
       #subtitle = "The data set includes 50 charter schools and 100 non-charter schools",
       fill = "")


ggsave_cc(most_addded_plot, file = "Most Added Tags by Type", dir = gen_dir)



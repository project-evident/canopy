data = left_join(sch, sch_demo, by = "school_id")

tag = "synchronous_online"
dem = "hispanic_percent"

n_bins = 5

plot_data = data %>% 
  mutate(hispanic_bin = 
           coalesce(cut(hispanic_percent, breaks = seq(0, 1, length.out = n_bins + 1)), "missing")) %>%
  group_by(hispanic_bin) %>%
  summarize(
    n_sch_with_tag = sum(asynchronous_online),
    n_sch_wo_tag = sum(!asynchronous_online),
    n_stu_with_tag = sum(student_count[asynchronous_online], na.rm = TRUE),
    n_stu_wo_tag = sum(student_count[!asynchronous_online], na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = -hispanic_bin, names_pattern = "n_(.*)_(.*)_tag", names_to = c("measure", "tagged")
  )

plot_data %>%
  filter(measure == "sch") %>%
  ggplot(aes(x = hispanic_bin, y = value, fill = tagged)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = value),
            position = position_dodge(width = 0.6),
            vjust = -.2) +
  bar_y_scale_count +
  scale_fill_manual(
    name = "Synchronous online learning",
    values = unname(cc_cols[c("light blue", "red")]),
    labels = c("Tagged", "Not Tagged")
  ) +
  labs(x = "Percent of students that are Hispanic",
       y = "Number of schools",
       title = "Synchronous online learning\nby Hispanic student percentage") +
  theme(legend.position = "bottom")


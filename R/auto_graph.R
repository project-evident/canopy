data = left_join(sch, sch_demo, by = "school_id")

#write_rds(data, "data/shiny_school_demo.rds")


tag_by_demographic = function(tag, dem, n_bins = 5) {

  breaks = seq(0, 1, length.out = n_bins + 1)
  bin_labels = label_percent_bins(breaks)
  
  plot_data = data %>% 
    mutate(
      bin = coalesce(cut(
        .data[[dem]],
        breaks = seq(0, 1, length.out = n_bins + 1)
      ),
      "missing")
    ) %>%
    group_by(bin) %>%
    summarize(
      n_sch_with_tag = sum(.data[[tag]]),
      n_sch_wo_tag = sum(!.data[[tag]]),
      n_stu_with_tag = sum(student_count[.data[[tag]]], na.rm = TRUE),
      n_stu_wo_tag = sum(student_count[!.data[[tag]]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_longer(
      cols = -bin, names_pattern = "n_(.*)_(.*)_tag", names_to = c("measure", "tagged")
    )

  tag_dem_plot = plot_data %>%
  filter(measure == "sch") %>%
  ggplot(aes(x = bin, y = value, fill = tagged)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = value),
            position = position_dodge(width = 0.6),
            vjust = -.2) +
  bar_y_scale_count +
  scale_fill_manual(
    name = label_tags(tag),
    values = unname(cc_cols[c("light blue", "red")]),
    labels = c("Tagged", "Not Tagged")
  ) +
  labs(
    x = label_dems(dem),
    y = "Number of schools",
    #title = "Synchronous online learning\nby Hispanic student percentage"
    title = glue("{label_tags(tag, capitalize = 'first')}\n by {label_dems(dem)}")
  ) +
  theme(legend.position = "bottom")

  return(tag_dem_plot)
}

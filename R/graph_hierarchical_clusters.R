conf_hclust = conf %>% 
  select(tags$tag) %>% 
  t %>% 
  dist %>%
  hclust(method = "ward.D2")

gg_hc = conf_hclust %>% dendro_data(type = "triangle")


## Compared to Rmd doc, replacing "alternate labels" with EFA clusters
e56 = read_tsv(here("reporting/clusters.tsv"))

hc_labs = gg_hc %>%
  label %>%
  left_join(e56, by = c("label" = "tag")) %>%
  mutate(
    clust_5 = factor(clust_5, levels = clust_5_order),
    tag_label = label_tags(label)
  )

# The Ward distance metric seems pretty good - might be worth trying out some
# others (definitely Jaccard, maybe Manhattan). Euclidean didn't seem as
# sensible.

hclust_plot = ggplot() +
  geom_segment(data = segment(gg_hc), aes(x, y, xend = xend, yend = yend)) +
  geom_text(
    data = hc_labs,
    aes(x, y, label = tag_label, color = clust_5),
    hjust = 0, size = 3.8,
    angle = -90,
    family = "Lato Light",
    show.legend = FALSE,
    nudge_y = -.1
  ) +
  coord_cartesian(clip = "off") +
  scale_color_tableau(
    name = "EFA Clusters",
  ) +
  geom_point(data = hc_labs, aes(x, y, color = clust_5), alpha = 0) + # dummy for legend
  guides(color = guide_legend(
    "Practice Cluster",
    override.aes = list(size = 7, alpha = 1),
    label.position = "bottom",
    direction = "horizontal",
    nrow = 2
  )) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = expansion(mult = c(1.35, 0), add = c(8, 0))) +
  labs(title = "Hierarchical tag clustering",
       subtitle = "Colored by EFA practice clusters") +
  theme_dendro() +
  theme(panel.border = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        plot.margin = margin(8, 8, 32, 8, unit = "pt")
        #legend.justification = c("right", "top")
  )

hclust_plot

ggsave_cc(hclust_plot,
          file = "Hierarchical Clusering",
          dir = "graphs/Tags",
          write_data = FALSE,
          fig_height = 6, fig_width = 11)



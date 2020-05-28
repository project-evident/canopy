## Clusters ####
e56 = read_tsv(here("reporting/clusters.tsv"))

logistic_one_coef %>%
  left_join(e56, by = "tag") %>%
  ggplot(aes(x = term, y = tag, fill = estimate)) +
    geom_tile() +
    facet_wrap(~clust_5, scales = "free") +
    scale_y_discrete(labels = label_tags) +
    scale_x_demo +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))



make_cluster_cor = function(
  clust_dat = e56,
  tagdem = tagdem,
  dems = dems,
  clust_col = "clust_5",
  dem_cols = dem_cols,
  dem_labs = dem_labs
) {
  clust_dat = clust_dat[c("tag", clust_col)]
  names(clust_dat)[2] = "Cluster"
  clust_tagdem = tagdem %>% 
    left_join(clust_dat, by = "tag")

  clust = reshape2::dcast(clust_tagdem,
                          school_id ~ Cluster, fun.aggregate = sum, value.var = "value")
  clustdem = clust %>% left_join(dems, by = "school_id")
  
  clust_names = unique(clust_dat[["Cluster"]])
  n_clust = length(clust_names)
  
  clust_cors = clustdem %>%
    select(all_of(clust_names), all_of(dem_cols)) %>%
    mutate(charter = ifelse(charter == "Yes", 1, 0)) %>% 
    cor(use = "pairwise.complete.obs")
  
  clust_cors = clust_cors[clust_names, dem_cols]
  
  clust_cor_long = reshape2::melt(clust_cors)
  names(clust_cor_long) = c("Cluster", "Demographic", "Correlation")
  
  clust_cor_long = clust_cor_long %>%
    filter(Cluster %in% clust_names, Demographic %in% dem_cols) %>%
    mutate(Demographic = factor(Demographic, levels = dem_cols))
  
  if("General Approaches" %in% clust_names) {
    clust_cor_long$Cluster = relevel(clust_cor_long$Cluster, ref = "General Approaches")
  }
  clust_cor_long$Demographic = dem_labs_rv[clust_cor_long$Demographic]
  clust_cor_long$Demographic = factor(clust_cor_long$Demographic, levels = dem_labs_rv)
  clust_cor_long  
}

plot_cluster_cor = function(
  clust_cor_long, cluster_axis = "y"
  ) {
  n_clust = length(unique(clust_cor_long$Cluster))
  clust_plot = 
    if(cluster_axis == "y") {
      ggplot(clust_cor_long, aes(x = Demographic, y = Cluster, fill = Correlation)) +
        labs(x = "Demographic/School Characteristic") +
        scale_x_discrete(expand = c(0, 0))
    }
    else {
      ggplot(clust_cor_long, aes(y = Demographic, x = Cluster, fill = Correlation)) +
        labs(y = "Demographic/School Characteristic") +
        scale_x_discrete(expand = c(0, 0), limits = rev(levels(clust_cor_long$Cluster))) 
    }
    clust_plot = clust_plot +
      geom_tile() +
      geom_text(aes(label = round(Correlation, 2)), color = "gray20", size = 3.5) +
      scale_fill_cc_gradient +
      scale_y_discrete(expand = c(0, 0)) +
      coord_equal() +
      labs(title = sprintf("Correlation between practice clusters and school demographics", n_clust),
         fill = "Correlation") +
      theme_cc_few +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
          panel.grid = element_blank()#,
          #plot.title.position = "plot"
      )
  
  return(clust_plot)
}


clust_5_cor_long = make_cluster_cor(
  clust_col = "clust_5",
  tagdem = tagdem,
  dems = dems,
  dem_cols = setdiff(dem_cols, "level_simple")
) %>%
  mutate(Cluster = factor(Cluster, levels = rev(clust_5_order)))
clust_5_plot = plot_cluster_cor(clust_5_cor_long)
clust_5_plot

ggsave_cc(clust_5_plot, "Cluster Demographic Correlations (5 clusters)", dir = gen_dir, fig_width = 1.2 * fig_width)
#ggsave(here("graphs/Clusters (5) and Demographics.png"), clust_5_plot, width = fig_width, height = fig_height)



clust_5_blog1 = 
  clust_5_cor_long %>%
  filter(Demographic %in% c("Elementary schools", "Middle schools", "High schools")) %>%
  plot_cluster_cor(cluster_axis = "x") +
    labs(title = "Correlation between category and school level", y = "")

clust_5_blog1
ggsave_cc(clust_5_blog1, "Cluster Level Heatmap", dir = gen_dir)


clust_6_plot = plot_cluster_cor(
  clust_col = "clust_6",
  tagdem = tagdem,
  dems = dems,
  dem_cols = dem_cols
)

ggsave(here("graphs/Clusters (6) and Demographics.png"), clust_6_plot, width = fig_width + 3, height = fig_height)


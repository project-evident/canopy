source("R/branding.R")

plot_tag_cor = function(cor_mat, title = "") {
  ggcorrplot(cor_mat, hc.order = T, type = "upper") +
    scale_fill_distiller(type = "div", limits = c(-1, 1), expand = c(0, 0)) +
    labs(title = title,
         fill = "Correlation") +
    #scale_x_discrete(labels = label_tags) +
    #scale_y_discrete(labels = label_tags) +
    labs(x = "", y = "") + 
    scale_fill_cc_gradient + 
    theme_cc_few + 
    theme(axis.text.x = element_blank(), 
          axis.text.y = element_blank(),
          panel.border = element_blank(),
          axis.ticks = element_blank()
          )
}

all_cor_plot = plot_tag_cor(all_cor, title = "Correlation heat map for all tags") +
  theme(legend.position = c(.85, .25))
#all_cor_plot
ggsave_cc(all_cor_plot, file = "Correlation heatmap", dir = "graphs/Tags/")

ggplotly(all_cor_plot) %>%
  htmlwidgets::saveWidget(
    file = "C:\\workspace\\canopy\\graphs\\Tags\\Correlations-heatmap-interactive.html",
    selfcontained = TRUE
  )

pivot_cor = function(m) {
  d = data.frame(row=rownames(m)[row(m)[upper.tri(m)]], 
           col=colnames(m)[col(m)[upper.tri(m)]], 
           corr=m[upper.tri(m)])
  d$clustered_order = 1:nrow(d)
  return(d)
}

all_cor_long = pivot_cor(all_cor) %>% 
  arrange(desc(corr))

write_csv(all_cor_long, path = "graphs/Tags/Correlations - All.csv")
write_csv(all_cor_long %>% slice(1:10, n() - 0:9),
          path = "graphs/Tags/Correlations - Extremes.csv")

corr_hist = ggplot(all_cor_long, aes(x = corr)) +
  geom_histogram(binwidth = 0.03, fill = cc_cols["dark blue"]) +
  geom_vline(aes(xintercept = mean(corr)), color = cc_cols["green"], size = 1) +
  geom_text(aes(x = mean(corr), y = Inf, label = paste("Average:", round(mean(corr), 2))), hjust = -.1, check_overlap = TRUE, vjust = 1.1, family = "Lato Light") + 
  bar_y_scale_count +
  scale_x_continuous(limits = c(-1, 1), expand = expansion(0, 0)) +
  labs(title = "Distribution of pairwise tag correlations", y = "Count of Tag Pairs",
       x = "Correlation") +
  theme(plot.margin = margin(t = 8, r = 12, b = 8, l = 8, unit = "pt"))
corr_hist %>%
  ggsave_cc(file = "Correlation distribution", 
            dir = "graphs/Tags",
            fig_height = 6,
            write_data = FALSE)

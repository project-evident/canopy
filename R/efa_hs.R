## EFA 

library(here)


library(ggplot2)
library(ggthemes)

library(dplyr)
library(tidyr)

library(psych)
library(cluster)
library(igraph)
library(parameters)

library(readr)
library(stringr)

theme_set(theme_bw())

load(here("data", "cleaned.rdata"))


## HS Cor ####
conf_hs = conf %>%
  filter(str_detect(level, "High")) %>%
  select(t1$tag, t2$tag) 

sort(colSums(conf_hs)) ## minimum 9... not too bad

cor_hs = conf_hs %>%
    cor

fa_pa = fa.parallel(cor_hs, fm = "pa", fa = "fa", n.obs = nrow(conf_hs))
fa_mr = fa.parallel(cor_hs, fm = "minres", fa = "fa", n.obs = nrow(conf_hs))
# 7 clusters...

library(ggcorrplot)
cor_hs_no_diag = cor_hs
diag(cor_hs_no_diag) = NA
ggcorrplot(cor_hs_no_diag, hc.order = T) +
  scale_fill_distiller(type = "div", limits = c(-1, 1), expand = c(0, 0), direction = 1) +
  labs(title = "Correlation heat map between Tier 2 Tags",
       fill = "Correlation") +
  theme(axis.text.x = element_text(size = rel(0.7), angle = 40, hjust = 1),
        axis.text.y = element_text(size = rel(0.7)))


# iclust_hs = iclust(cor_hs)
# colSums(iclust_hs$clusters)
# C84 C80 C70 C81 
#  73   7   3   5
## 4 clusters in result
## pretty much everything in first cluster
## pretty worthless

library(qgraph)
#qgraph.efa(cor_hs)
# doesn't work with pysch::fa
# the MLE implementation of stats::factanal doesn't work on this data


hs_6_mr = fa(cor_hs, nfactors = 6, rotate = "oblimin", fm = "minres")
hs_6_pa = fa(cor_hs, nfactors = 6, rotate = "oblimin", fm = "pa")
hs_6_mc = fa(cor_hs, nfactors = 6, rotate = "oblimin", fm = "minchi")
hs_6_al = fa(cor_hs, nfactors = 6, rotate = "oblimin", fm = "alpha")
hs_6 = hs_6_mr


# print(hs_6, sort = T)
# print(hs_6$loadings, cutoff = 0.29, sort = T)
# fa.diagram(hs_6)


if(!file.exists(here("reporting", "EFA HS Results Max.txt"))) {
  hs_6 %>%
    model_parameters(sort = TRUE, threshold = "max") %>%
    write_tsv(here("reporting", "EFA HS Results Max.txt"), na = "")
  
  hs_6 %>%
    model_parameters(sort = TRUE, threshold = 0.30) %>%
    write_tsv(here("reporting", "EFA HS Results Thresh.txt"), na = "")
  
  hs_6 %>%
    model_parameters(sort = TRUE) %>%
    write_tsv(here("reporting", "EFA HS Results All.txt"), na = "")
}


hs_viz = qgraph.loadings(fact = hs_6$loadings,
                labels = row.names(hs_6$loadings), shape = "rectangle",
                vsize = 2.3, 
                #vsize2 = .5,
#                usePCH = FALSE,
                label.norm = "OOOOOO",
#                label.scale.equal = TRUE#,
                label.scale = FALSE,
                border.color = "gray70",
                border.width = 0.5

                )

#qgraph(cor_hs)
# hard to get right
cairoDevice::Cairo_png("graphs/hs_network_diagram_big.png",
                       height = 25, width = 32)
plot(hs_viz)
dev.off()
#qgraphAnnotate(hs_viz)
# hs_viz_ig = igraph::as.igraph(hs_viz)
# hs_viz_d3 = networkD3::igraph_to_networkD3(, group = members)
# networkD3::forceNetwork(Links = hs_viz_d3$links,  Nodes = hs_viz_d3$nodes,
# Source = 'source', Target = 'target', NodeID = 'name',
# Group = 'group')

#qgraph:::qgraphD3(hs_viz, D3width = 1600, D3height = 1600)




## ES Cor ####

conf_es = conf %>%
  filter(str_detect(level, "Elementary")) %>%
  select(t1$tag, t2$tag)

es_tag_counts = sort(colSums(conf_es))
conf_es = conf_es[, colSums(conf_es) > 5]

cor_es = conf_es %>%
  cor


fa_pa = fa.parallel(cor_es, fm = "pa", fa = "fa", n.obs = nrow(cor_es))
fa_mr = fa.parallel(cor_es, fm = "minres", fa = "fa", n.obs = nrow(cor_es))
# wants 13 or 15 --- too many


efa_es = fa(cor_es, nfactors = 6, rotate = "oblimin", fm = "minres")
#print(efa_es, sort = T)
print(efa_es$loadings, cutoff = 0.3, sort = T)
#fa.diagram(efa_es)


if(!file.exists(here("reporting", "EFA Elem Results Max.txt"))) {
  efa_es %>%
    model_parameters(sort = TRUE, threshold = "max") %>%
    write_tsv(here("reporting", "EFA Elem Results Max.txt"), na = "")
  
  efa_es %>%
    model_parameters(sort = TRUE, threshold = 0.29) %>%
    write_tsv(here("reporting", "EFA Elem Results Thresh.txt"), na = "")
  
  efa_es %>%
    model_parameters(sort = TRUE) %>%
    write_tsv(here("reporting", "EFA Elem Results All.txt"), na = "")
}



elem_viz = qgraph.loadings(fact = efa_es$loadings,
                labels = row.names(efa_es$loadings), shape = "rectangle",
                vsize = 2.3, 
                #vsize2 = .5,
#                usePCH = FALSE,
                label.norm = "OOOOOO",
#                label.scale.equal = TRUE#,
                label.scale = FALSE,
                border.color = "gray70",
                border.width = 0.5

                )

#qgraph(cor_hs)
# hard to get right
cairoDevice::Cairo_png("graphs/elem_network_diagram_big.png",
                       height = 25, width = 32)
plot(hs_viz)
dev.off()


#### All cor processing ####

## From Chelsea
# 5-cluster names:
# - Social-Emotional Learning & Equity
# - Project-based Learning
# - Flexible Pathways to College & Career
# - Assessment & Student Success
# - Blended Learning
# 
# 6-cluster names:
# - Equity
# - PBL
# - Flexible Pathways to College & Career
# - Assessment & Student Success
# - Blended Learning
# - General Approaches


# efa5_key = c(
#   "MR3" = "Social-Emotional Learning & Equity",
#   "MR1" = "Project-Based Learning",
#   "MR5" = "Flexible Pathways to College & Career",
#   "MR4" = "Assessment & Student Success",
#   "MR2" = "Blended Learning"
# )
# 
# efa6_key = c(
#   "MR3" = "Equity",
#   "MR1" = "Project-Based Learning",
#   "MR5" = "Flexible Pathways to College & Career",
#   "MR4" = "Assessment & Student Success",
#   "MR2" = "Blended Learning",
#   "MR6" = "General Approaches"
# )
# 
# 
# e5 = all_efa_5 %>%
#   model_parameters(sort = TRUE, threshold = "max") %>%
#    select(Variable, starts_with("MR"))
# 
# e5$Cluster = names(e5[-1])[apply(e5[-1], 1, function(x) which(!is.na(x)))]
# e5 = as.data.frame(e5) %>%
#   mutate(Cluster = efa5_key[Cluster]) %>%
#   select(tag = Variable, clust_5 = Cluster)
# 
# e6 = all_efa_6 %>%
#   model_parameters(sort = TRUE, threshold = "max") %>%
#    select(Variable, starts_with("MR"))
# 
# e6$Cluster = names(e6[-1])[apply(e6[-1], 1, function(x) which(!is.na(x)))]
# e6 = as.data.frame(e6) %>%
#   mutate(Cluster = efa6_key[Cluster]) %>%
#   select(tag = Variable, clust_6 = Cluster)
# 
# e56 = left_join(e5, e6, by = "tag")
# 
# write_tsv(e56, here("reporting/clusters.tsv"))
# 


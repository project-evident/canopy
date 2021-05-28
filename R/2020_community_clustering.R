## Community Clustering
library(here)

library(ggplot2)
library(ggthemes)

library(dplyr)
library(tidyr)

library(psych)
library(cluster)
## This was an experiment in visualizing tag connections
## that never really proved useful.
## Perhaps with more effort on formatting it could become more readable/usable

library(igraph)
library(parameters)

library(readr)

theme_set(theme_bw())

load(here("data", "cleaned.rdata"))

g = graph.adjacency(all_cor + 1, mode = "undirected", weighted = TRUE)
g = simplify(g)
plot(g)

circ = layout.circle(g)
plot(g, layout = circ)

fd = layout.fruchterman.reingold(g)
plot(g, layout = fd)

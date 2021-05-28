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

theme_set(theme_bw())

load(here("data", "cleaned.rdata"))

fa_pa = fa.parallel(all_cor, fm = "pa", fa = "fa", n.obs = nrow(all_cor))
fa_mr = fa.parallel(all_cor, fm = "minres", fa = "fa", n.obs = nrow(all_cor))
# with T1, 5 or 6 factors/components recomended


all_efa_5 = fa(all_cor, nfactors = 5, rotate = "oblimin", fm = "minres")
print(all_efa_5, sort = T)
print(all_efa_5$loadings, cutoff = 0.27)
fa.diagram(all_efa_5)

all_efa_6 = fa(all_cor, nfactors = 6, rotate = "oblimin", fm = "minres")
print(all_efa_6$loadings, cutoff = 0.3)
fa.diagram(all_efa_6)

(all_efa5_mc = fa(all_cor, nfactors = 5, rotate = "oblimin", fm = "minchi", np.obs = 173))
# BIC -12706.72
(all_efa5_pa = fa(all_cor, nfactors = 5, rotate = "oblimin", fm = "pa", n.obs = 173))
# BIC -12703.02
(all_efa5_mle = fa(all_cor, nfactors = 5, rotate = "oblimin", fm = "mle", n.obs = 173))
# BIC = -23708.82
## Of these, by BIC, minchi does marginally best, and is comparable to minres.
## We'll stick with minres


if(!file.exists(here("reporting", "EFA Results.txt"))) {
  all_efa_5 %>%
    model_parameters(sort = TRUE, threshold = "max") %>%
    write_tsv(here("reporting", "EFA Results Max.txt"), na = "")
  
  all_efa_5 %>%
    model_parameters(sort = TRUE, threshold = 0.29) %>%
    write_tsv(here("reporting", "EFA Results Thresh.txt"), na = "")
  
  all_efa_5 %>%
    model_parameters(sort = TRUE) %>%
    write_tsv(here("reporting", "EFA Results All.txt"), na = "")

  all_efa_6 %>%
    model_parameters(sort = TRUE, threshold = "max") %>%
    write_tsv(here("reporting", "EFA6 Results Max.txt"), na = "")
  
  all_efa_6 %>%
    model_parameters(sort = TRUE, threshold = 0.29) %>%
    write_tsv(here("reporting", "EFA6 Results Thresh.txt"), na = "")
  
  all_efa_6 %>%
    model_parameters(sort = TRUE) %>%
    write_tsv(here("reporting", "EFA6 Results All.txt"), na = "")
}

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


efa5_key = c(
  "MR3" = "Equity & Social-Emotional Learning",
  "MR1" = "Project-Based Learning",
  "MR5" = "Flexible Pathways to College & Career",
  "MR4" = "Competency-Based Education",
  "MR2" = "Blended Learning"
)

efa6_key = c(
  "MR3" = "Equity",
  "MR1" = "Project-Based Learning",
  "MR5" = "Flexible Pathways to College & Career",
  "MR4" = "Assessment & Student Success",
  "MR2" = "Blended Learning",
  "MR6" = "General Approaches"
)


e5 = all_efa_5 %>%
  model_parameters(sort = TRUE, threshold = "max") %>%
   select(Variable, starts_with("MR"))

e5$Cluster = names(e5[-1])[apply(e5[-1], 1, function(x) which(!is.na(x)))]
e5 = as.data.frame(e5) %>%
  mutate(Cluster = efa5_key[Cluster]) %>%
  select(tag = Variable, clust_5 = Cluster)

e6 = all_efa_6 %>%
  model_parameters(sort = TRUE, threshold = "max") %>%
   select(Variable, starts_with("MR"))

e6$Cluster = names(e6[-1])[apply(e6[-1], 1, function(x) which(!is.na(x)))]
e6 = as.data.frame(e6) %>%
  mutate(Cluster = efa6_key[Cluster]) %>%
  select(tag = Variable, clust_6 = Cluster)

e56 = left_join(e5, e6, by = "tag")

write_tsv(e56, here("reporting/clusters.tsv"))



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






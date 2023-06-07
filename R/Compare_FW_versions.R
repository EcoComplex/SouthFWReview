##
# Compare original & standardized FWs
##

library(tidyverse)
library(igraph)
library(multiweb)

# Scotia Sea ----
# Old
nscotia_old <- read.csv("Data/NorthernScotia_FoodWeb.csv", header = T)
sscotia_old <- read.csv("Data/SouthernScotia_FoodWeb.csv", header = T)
# Standardized
nscotia_std <- read.csv("Data/Northern_Scotia_collapsed_basal_top.csv", header = T)
sscotia_std <- read.csv("Data/Southern_Scotia_collapsed_basal_top.csv", header = T)

# Igraph objects
nscotia_old_g <- graph_from_data_frame(nscotia_old, directed = T)
sscotia_old_g <- graph_from_data_frame(sscotia_old, directed = T)
nscotia_std_g <- graph_from_data_frame(nscotia_std, directed = T)
sscotia_std_g <- graph_from_data_frame(sscotia_std, directed = T)

# Properties
nscotia_old_prop <- calc_topological_indices(nscotia_old_g) %>% mutate(Name="Old N Scotia")
sscotia_old_prop <- calc_topological_indices(sscotia_old_g) %>% mutate(Name="Old S Scotia")
nscotia_std_prop <- calc_topological_indices(nscotia_std_g) %>% mutate(Name="Std N Scotia")
sscotia_std_prop <- calc_topological_indices(sscotia_std_g) %>% mutate(Name="Std S Scotia")

scotia_prop <- bind_rows(nscotia_old_prop, sscotia_old_prop, nscotia_std_prop, sscotia_std_prop) %>% 
  rename(Network=Name) %>% 
  dplyr::select(Network, everything())

# Plot
par(mfrow = c(2,2))
plot_troph_level(nscotia_old_g, main = "Old N Scotia")
plot_troph_level(sscotia_old_g, main = "Old S Scotia")
plot_troph_level(nscotia_std_g, main = "Std N Scotia")
plot_troph_level(sscotia_std_g, main = "Std S Scotia")


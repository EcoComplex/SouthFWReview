##
# Scotia Sea marine mammals & sea birds
##

# Packages
library(tidyverse)

# Species list ----
sp_list <- read.csv("Data/SB&MM_list_ScotiaSea.csv", header = T)

# Diet database ----
# Import Southern Ocean dietary database from
# https://esapubs.org/archive/ecol/E092/097/#data (diet.csv file)
raymond_db <- read.csv("raymond_database.csv", header = T)

# Get interactions ----
# Select columns of interest
inter_raw <- raymond_db %>% 
  select(PREY_NAME, PREDATOR_NAME, LOCATION)
# Tidy 'sp_list'
sp_list_tidy <- sp_list %>% 
  select(species) %>% 
  rename(PREDATOR_NAME = species)
# Get interactions for sea birds & marine mammals spp
inter2add <- merge(x = inter_raw, y = sp_list_tidy, by = "PREDATOR_NAME")
# Filter by prey from Scotia Sea FWs. First, load FWs
nss_fw <- read.csv("Data/NorthernScotia_FoodWeb.csv", header = T, colClasses = c("NULL", NA, NA))
nss_preylist <- nss_fw %>% 
  select(resource) %>% 
  rename(PREY_NAME = resource) %>% 
  distinct()
nss2add <- merge(x = inter2add, y = nss_preylist, by = "PREY_NAME") %>% 
  select(PREY_NAME, PREDATOR_NAME) %>% 
  rename(resource=PREY_NAME, consumer=PREDATOR_NAME) %>%
  distinct()

sss_fw <- read.csv("Data/SouthernScotia_FoodWeb.csv", header = T, colClasses = c("NULL", NA, NA))
sss_preylist <- sss_fw %>% 
  select(resource) %>% 
  rename(PREY_NAME = resource) %>% 
  distinct()
sss2add <- merge(x = inter2add, y = sss_preylist, by = "PREY_NAME") %>% 
  select(PREY_NAME, PREDATOR_NAME) %>% 
  rename(resource=PREY_NAME, consumer=PREDATOR_NAME) %>% 
  distinct()

# Add interactions to FWs
nss_fw_new <- bind_rows(nss_fw, nss2add)
sss_fw_new <- bind_rows(sss_fw, sss2add)

# New FWs ----
write.csv(nss_fw_new, file = "Data/NorthernScotia_top.csv")
write.csv(sss_fw_new, file = "Data/SouthernScotia_top.csv")


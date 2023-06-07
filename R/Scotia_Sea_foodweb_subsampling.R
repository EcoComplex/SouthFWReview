###Scotia Sea food webs###

# Susanne Kortsch
# 07.06.2023

#Scotia nodes info
#metaweb_nodes.csv
nodes_Scotia<-read.csv("Data/metaweb_nodes.csv", sep=",")
dim(nodes_Scotia)
nodes_Scotia<-nodes_Scotia[,-c(8,9)]#remove these columns
head(nodes_Scotia)

#southern_nodes
ids_s<-sort(c(which(nodes_Scotia$Lat.distribution=="Both"), which(nodes_Scotia$Lat.distribution=="South")))
SS_pw<-nodes_Scotia[ids_s,] 
head(SS_pw)
dim(SS_pw)

#northern_nodes
ids_n<-sort(c(which(nodes_Scotia$Lat.distribution=="Both"), which(nodes_Scotia$Lat.distribution=="North")))
NS_pw<-nodes_Scotia[ids_n,] 
head(NS_pw)
dim(NS_pw)

#Scotia links metaweb
links_scotia<-read.csv("Data/metaweb_links.csv", sep=",")
head(links_scotia)

#Clean names of the pl, full species list
#Select only interactions between species from species list
links_scotia$consumer[links_scotia$consumer%in%SS_pw$node]
links_scotia$resource[links_scotia$resource%in%SS_pw$node]

#subset the pairwise list for Southern Scotia Sea
pl_cons <- subset(links_scotia, links_scotia$consumer%in%SS_pw$node)
pl_reso <- subset(links_scotia, links_scotia$resource%in%SS_pw$node)
plA <- subset(links_scotia, links_scotia$consumer%in%SS_pw$node & links_scotia$resource%in%SS_pw$node)
plA <- plA[!duplicated(plA),]
dim(plA) # rows are identical to number of trophic links
str(plA)
SS_FW<-plA
write.csv(SS_FW, "Data/SouthernScotia_FoodWeb.csv", row.names=F)

#subset the pairwise list for Northern Scotia Sea
pl_cons2 <- subset(links_scotia, links_scotia$consumer%in%NS_pw$node)
pl_reso2 <- subset(links_scotia, links_scotia$resource%in%NS_pw$node)
plA2 <- subset(links_scotia, links_scotia$consumer%in%NS_pw$node & links_scotia$resource%in%NS_pw$node)
plA2 <- plA2[!duplicated(plA2),]
dim(plA2) # rows are identical to number of trophic links
str(plA2)
NS_FW<-plA2
head(NS_FW)
write.csv(NS_FW, "Data/NorthernScotia_FoodWeb.csv", row.names=F)

##################################################################################

#Extract taxonmic information for species

library("taxize")
library("tidyverse")
library("igraph")

#scotia_tax<-tax_name(nodes_Scotia$node, db = "itis", get = c("genus", "family", "order", "class", "phylum"))

#Scotia Sea taxonmic info 
#scotia_tax_list<-read_csv("Data/scotia_taxonomic_list.csv")
#scotia_tax_list<-scotia_tax_list[, -c(1,2)]
#colnames(scotia_tax_list)<-c("species", "genus", "family", "order", "class", "phylum")
#head(scotia_tax_list)
#unique(scotia_tax_list$order)

#Collapse basal species, code from Leo
ss_fw<-read_csv("Data/SouthernScotia_top.csv")
ss_fw<-ss_fw[,-1]
head(ss_fw)
dim(ss_fw)
dfn_ss <- unique(ss_fw$consumer)
con_names_ss <- gnr_resolve(dfn_ss, best_match_only = TRUE, canonical=TRUE)
# Species not found
anti_join(data.frame(user_supplied_name=dfn_ss),con_names_ss)

dfn_ss2 <- unique(ss_fw$resource)
res_names <- gnr_resolve(dfn_ss2, best_match_only = TRUE, canonical=TRUE)
anti_join(data.frame(user_supplied_name=dfn_ss2),res_names)

# pairwise interaction list for the Southern Scotia Sea food web
gM_ss <- graph_from_edgelist(as.matrix(ss_fw), directed  = T)

# identify basal species
basal_sp_ss <- (V(gM_ss)[(degree(gM_ss,mode="in")==0) ])$name
basal_sp_ss

# Solamente clase
#class <- tax_name(basal_sp_ss, get = "class", db = "itis")
#class %>% count(is.na(class))                                # 27 NA
#class <- class%>% mutate(genera= stringr::word(query)) %>% rename( species = query ) %>% select(genera,species,class)
#write_tsv(class,"Data/SoutherScotia_basal.dat")

class<-read_tsv("Data/SoutherScotia_basal.dat")
class2 <- class[-c(3,9,18),]#remove detritus, bacteria, and maxillopoda nodes
#NOTE! MAXILLOPODA IS MISSING RESOURCE ITEMS!
#rename some plankton species to other for aggregation
class2  <- class2 %>% 
mutate(class = case_when(class %in% c("Prymnesiophyceae",NA) ~ "Phytoplankton_other",TRUE ~ class))
#class2[which(is.na(class2$class)),3]<-"Phytoplankton_other" #assign phytoplankton other to phytoplankton with NAs
#class2[which(class2$class=="Prymnesiophyceae"),3]<-"Phytoplankton_other"
class2 %>% filter(!is.na(class)) %>%  distinct(genera,class)

# Genera tabla genero clase para reemplazar el nombre de la especie por la clase solo en el caso de Bacillariophyceae
#
gencla <- class2 %>% filter(!is.na(class)) %>%  distinct(genera,class) %>%  filter(class=="Bacillariophyceae")
#gencla <- add_case(gencla, genera="Nitzschia", class="Bacillariophyceae" )
gencla
gencla_dino <- class2 %>% filter(!is.na(class)) %>%  distinct(genera,class) %>%  filter(class=="Dinophyceae")
gencla_other <- class2 %>% filter(!is.na(class)) %>%  distinct(genera,class) %>%  filter(class=="Phytoplankton_other")

ss_fw2<-ss_fw
#change names of resource species that need to be aggregated
sapply( 1:nrow(ss_fw2), function(i) {
  #i<-3389
  res_gen <- stringr::word(ss_fw2$resource[i]) 
  
  rep_cla <- gencla %>% filter(genera == res_gen) 
  if(nrow(rep_cla)==1)
    ss_fw2$resource[i] <<- rep_cla$class
  
  rep_cla_dino <- gencla_dino %>% filter(genera == res_gen) 
  if(nrow(rep_cla_dino)==1)
    ss_fw2$resource[i] <<- rep_cla_dino$class
  
  rep_cla_other <- gencla_other %>% filter(genera == res_gen) 
  if(nrow(rep_cla_other)==1)
    ss_fw2$resource[i] <<- rep_cla_other$class
})

unique(ss_fw2$resource)

collapsed_ss_fw <- ss_fw2 %>% distinct()
dim(collapsed_ss_fw)
gM_ss2 <- graph_from_edgelist(as.matrix(collapsed_ss_fw), directed  = T)


#check the basal species
basal_sp_ss2 <- (V(gM_ss2)[ (degree(gM_ss2,mode="in")==0) ])$name
basal_sp_ss2


write_csv(collapsed_ss_fw,"Data/Southern_Scotia_top_collapsed_basal.csv")

#
meta_fw<-read_csv("Data/metaweb_links.csv")
###############

#Collapse basal species, code from Leo
ns_fw<-read_csv("Data/NorthernScotia_top.csv")
ns_fw<-ns_fw[,-1]
head(ns_fw)
dim(ns_fw)
dfn_ns <- unique(ns_fw$consumer)
con_names_ss <- gnr_resolve(dfn_ss, best_match_only = TRUE, canonical=TRUE)
# Species not found
anti_join(data.frame(user_supplied_name=dfn_ss),con_names_ss)


dfn_ns2 <- unique(ns_fw$resource)
res_names <- gnr_resolve(dfn_ns2, best_match_only = TRUE, canonical=TRUE)
anti_join(data.frame(user_supplied_name=dfn_ns2),res_names)

# pairwise interaction list for the Southern Scotia Sea food web
gM_ns <- graph_from_edgelist(as.matrix(ns_fw), directed  = T)

# identify basal species
basal_sp_ns <- (V(gM_ns)[ (degree(gM_ns,mode="in")==0) ])$name
basal_sp_ns
# Solamente clase
class <- tax_name(basal_sp_ns, get = "class", db = "itis")
class %>% count(is.na(class))                                # 27 NA
class <- class%>% mutate(genera= stringr::word(query)) %>% rename( species = query ) %>% select(genera,species,class)

class<-read_tsv("Data/NorthernScotia_basal.dat")
class2 <- class[-c(3,9,18),]#remove detritus, bacteria, and maxillopoda
class2[which(is.na(class2$class)),3]<-"Phytoplankton_other" #assign phytoplankton other to phytoplankton with NAs
class2[which(class2$class=="Prymnesiophyceae"),3]<-"Phytoplankton_other"
class2 %>% filter(!is.na(class)) %>%  distinct(genera,class)

# Genera tabla genero clase para reemplazar el nombre de la especie por la clase solo en el caso de Bacillariophyceae
#
gencla <- class2 %>% filter(!is.na(class)) %>%  distinct(genera,class) %>%  filter(class=="Bacillariophyceae")
#gencla <- add_case(gencla, genera="Nitzschia", class="Bacillariophyceae" )
#gencla
gencla_dino <- class2 %>% filter(!is.na(class)) %>%  distinct(genera,class) %>%  filter(class=="Dinophyceae")
gencla_other <- class2 %>% filter(!is.na(class)) %>%  distinct(genera,class) %>%  filter(class=="Phytoplankton_other")
#

sapply( 1:nrow(ss_fw), function(i) {
  #i<-5969
  res_gen <- stringr::word(ss_fw$resource[i]) 
  
  rep_cla <- gencla %>% filter(genera == res_gen) 
  if(nrow(rep_cla)==1)
    ss_fw$resource[i] <<- rep_cla$class
  
  rep_cla_dino <- gencla_dino %>% filter(genera == res_gen) 
  if(nrow(rep_cla_dino)==1)
    ss_fw$resource[i] <<- rep_cla_dino$class
  
  rep_cla_other <- gencla_other %>% filter(genera == res_gen) 
  if(nrow(rep_cla_other)==1)
    ss_fw$resource[i] <<- rep_cla_other$class
})

unique(ss_fw$resource)

collapsed_ss_fw <- ss_fw %>% distinct()
dim(collapsed_ss_fw)
gM_ss <- graph_from_edgelist(as.matrix(collapsed_ss_fw), directed  = T)


#check the basal species
basal_sp_ss <- (V(gM_ss)[ (degree(gM_ss,mode="in")==0) ])$name
basal_sp_ss

write_csv(collapsed_ss_fw,"Data/Southern_Scotia_collapsed_basal_top.csv")






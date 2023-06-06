###Scotia Sea food web###


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


#Extract taxonmic information for species

library("taxize")
library("tidyverse")
#scotia_tax<-tax_name(nodes_Scotia$node, db = "itis", get = c("genus", "family", "order", "class", "phylum"))

#Scotia Sea taxonmic info 
scotia_tax_list<-read_csv("Data/scotia_taxonomic_list.csv")
scotia_tax_list<-scotia_tax_list[, -c(1,2)]
colnames(scotia_tax_list)<-c("species", "genus", "family", "order", "class", "phylum")
#head(scotia_tax_list)
#unique(scotia_tax_list$order)

#
ss_fw<-read_csv("Data/SouthernScotia_FoodWeb.csv")
head(ss_fw)
dim(ss_fw)
dfn_ss <- unique(dtot$consumer)
con_names_ss <- gnr_resolve(dfn_ss, best_match_only = TRUE, canonical=TRUE)
# Species not found
anti_join(data.frame(user_supplied_name=dfn_ss),con_names_ss)


dfn_ss2 <- unique(dtot$resource)
res_names <- gnr_resolve(dfn_ss2, best_match_only = TRUE, canonical=TRUE)
anti_join(data.frame(user_supplied_name=dfn_ss2),res_names)

# pairwise interaction list for the Southern Scotia Sea food web
gM <- graph_from_edgelist(as.matrix(ss_fw), directed  = T)






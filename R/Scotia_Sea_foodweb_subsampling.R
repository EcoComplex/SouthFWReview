###Scotia Sea food web###

#working dir
#C:\LocalData\susakort\GitHub\PIP\Rscripts
setwd("C:/LocalData/susakort/GitHub/PIP/Rscripts")

dirF<-"../Figures/"
dirSoctia<-"../Data/Scotia_Sea/"

#Scotia nodes info
#metaweb_nodes.csv
nodes_Scotia<-read.csv(paste0(dirSoctia,"metaweb_nodes.csv"), sep=",")
dim(nodes_Scotia)
nodes_Scotia<-nodes_Scotia[,-c(8,9)]
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
links_scotia<-read.csv(paste0(dirSoctia,"metaweb_links.csv"), sep=",")
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
write.csv(SS_FW, "../Data/Scotia_Sea/SouthernScotia_FoodWeb.csv", row.names=T)

#subset the pairwise list for Northern Scotia Sea
pl_cons2 <- subset(links_scotia, links_scotia$consumer%in%NS_pw$node)
pl_reso2 <- subset(links_scotia, links_scotia$resource%in%NS_pw$node)
plA2 <- subset(links_scotia, links_scotia$consumer%in%NS_pw$node & links_scotia$resource%in%NS_pw$node)
plA2 <- plA2[!duplicated(plA2),]
dim(plA2) # rows are identical to number of trophic links
str(plA2)
NS_FW<-plA2
head(NS_FW)
write.csv(NS_FW, "../Data/Scotia_Sea/NorthernScotia_FoodWeb.csv", row.names=T)





rm(list=ls())
setwd("/Users/santiago.doyle/Library/CloudStorage/Dropbox/Investigacion/Proyectos en colaboracion/Redes_troficas_PIP_CADIC/Git_repo_paper/SouthFWReview")
source("R/nodeTrophicLevelOmnivory.R")
food.web.csv <- c("BeagleChannel_links_original.csv",
                  "BurdwoodBank_links_original.csv",
                  "GulfSanJorge_links_original.csv",
                  "NorthernScotia_links_original.csv",
                  "SouthernScotia_links_original.csv",
                  "WeddellSea_links_original.csv")

library(igraph)
library(multiweb)
library(NetIndices)
links <- list()
dir.create("Output")
for (i in 1:length(food.web.csv)){
  links[[i]] <- read.csv2 (paste("Data/",food.web.csv[i],sep=""), header = TRUE, sep = ",")
  #Compute node metrics
  fwgraph <- graph_from_data_frame(links[[i]], directed = T)
  adjMat <- get.adjacency(fwgraph,sparse=F)
  # nodesMetrics <- cbind.data.frame(data.frame(degree = degree(fwgraph)),
  #                                  TrophInd(adjMat),
  #                                  trophiclevels(fwgraph),
  #                                  data.frame(omnivory = Level.omnivory(fwgraph)))
  nodesMetrics <- cbind.data.frame(data.frame(degree = degree(fwgraph)),
                                   TrophInd(adjMat))
  nodesMetrics <- cbind.data.frame(data.frame(Nodes = rownames(nodesMetrics)),
                                   nodesMetrics)
  indexSorted <- sort(nodesMetrics$degree,decreasing = TRUE, index.return = TRUE)                     
  nodesMetrics <- nodesMetrics[indexSorted$ix,]
  labelFW <- substr(food.web.csv[i],1,regexpr("_", food.web.csv[i])-1)
  filename.nodesMetrics <- paste(labelFW,"_nodes_metrics",".csv",sep="")
  write.table(nodesMetrics,
              file = paste("Output/",filename.nodesMetrics,sep=""),
              row.names = FALSE,
              dec=",",
              sep=";")
}
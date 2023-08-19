### Susanne Kortsch 
#trophiclevels: computes short weighted trophic levels SWTL 
# from Williams and Martinez 2004, de Santana et al. 2013
# Trophic levels more appropriate for topologies
# Also includes values based on the longest path - long weighted trophic levels LWTL
trophiclevels<-function(net){
  mat <- get.adjacency(net, sparse=F)
  
  basal <- rownames(subset(mat, apply(mat, 2, sum)==0) & apply(mat, 1, sum)!= 0)
  paths_prey <- suppressWarnings(shortest.paths(graph = net, v= V(net), to = V(net)[basal], 
                                                mode = "in", weights = NULL, algorithm = "unweighted"))
  
  paths_prey[is.infinite(paths_prey)] <- NA
  shortest_paths <- suppressWarnings(as.matrix(apply(paths_prey, 1, min, na.rm=TRUE)))
  longest_paths <- suppressWarnings(as.matrix(apply(paths_prey, 1, max, na.rm=TRUE)))
  #for species with no prey apart of them
  shortest_paths[is.infinite(shortest_paths)] <- NA
  longest_paths[is.infinite(longest_paths)] <- NA
  
  # Shortest TL
  sTL <- 1 + shortest_paths 
  # Longest TL
  lTL <- 1 + longest_paths
  
  #Compute average prey trophic level
  W <- t(mat)
  rs <- rowSums(W)
  W <- W/matrix(rs, ncol = ncol(W), nrow = nrow(W))
  W[0 == rs, ] <- 0
  I <- diag(ncol(W))
  tl0<-rowSums(I - W)
  result <- tryCatch(solve(I - W), error = function(e) e)
  if ("error" %in% class(result)) {
    avtl <- rep(NA, ncol(pm))
    names(avtl) <- colnames(pm)
  }
  else {
    avtl <- rowSums(result)
  }
  
  # Short-weighted TL is the average of shortest TL and prey-averaged TL
  SWTL <- (sTL + avtl)/2
  
  # Long-weighted TL is the average of longest TL and prey-averaged TL
  LWTL <- (lTL + avtl)/2
  
  #check that only basal species have TL of 1
  SWTL[!rownames(mat)%in%basal & SWTL == 1] <- NA
  LWTL[!rownames(mat)%in%basal & LWTL == 1] <- NA
  
  res<-data.frame("swTL"= SWTL, "lwTL"=LWTL)
  rownames(res)<-rownames(mat)
  return(res)
}

#Omnivory O. Petchey code
Level.omnivory <- function(net, TLs=trophiclevels(net)[,1]) {
  web <- get.adjacency(net, sparse=F)
  if( sum(is.na(TLs)) == length(TLs) )
    #rr <- NA
    omni<-NA
  
  if( sum(is.na(TLs)) != length(TLs) ) {
    web.TLs <- matrix(rep(TLs, length(web[,1])), length(web[,1]), length(web[,1]))
    lo.pc <- numeric(length=length(web[,1]))
    for(i in 1:length(web[,1])) {
      tt <- web.TLs[web[,i]==1,i]
      if(length(tt)==0 | sum(!is.na(tt))==0 )
        lo.pc[i] = NA
      if(length(tt)>0 & sum(!is.na(tt))!=0)
        lo.pc[i] <- sd(tt)
    }
    #rr <- mean(lo.pc, na.rm=T)
    omni<- lo.pc
  }
  #rr
  omni[which(is.na(omni))]<-0
  omni
}
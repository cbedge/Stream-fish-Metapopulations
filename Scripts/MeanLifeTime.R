library(igraph)

#Necessary function that must be run
calc.w.in <- function(data, V, n){ 
  weight <- NULL
  for (j in 1:n){
    weight1 <- 0
    for (i in 1:n){
      weight1 <- weight1 + data[i,j]/V[i]
    }
    weight <- c(weight, weight1)
  }
  weight
}

####
#FUNCTION
#Calculates metapopulation mean lifetime for a graph with the following inputs
#REQUIRES nodefile: a dataframe with area, quality, upstream permeability, and downstream permeability of each node
#REQUIRES graphfile: a graph object that can be any dendritic network describing the structure of the network
#E Species specific coeffecint relating to patch size, between 1 and 100
#N level of environmental variation in population growth, between 0.01 and 4. Small is more
#U number of imigrants needed for succussful colonization
#E, N, and U can be user defined but defaults are set currently
####

MMLT.def <- function(nodefile, graphfile, E=1, N=0.5, U=2){
  p <- nrow(nodefile) #this is a counter variable
  #first step to to get the edges and permeabilities
  edge <- data.frame(get.edgelist(graphfile)) #get the edges of the graph
  edge$PermUS <- (nodefile$PermUS[match(edge$X1, nodefile$ID)]) * 100 #Set Upstream permeability, multiply by 100
  edge$PermDS <- (nodefile$PermDS[match(edge$X1, nodefile$ID)]) * 100 #Set Downstream permeability, multiply by 100
  
  weights <- matrix(nrow=p, ncol=p) #creates matrix of edge weights read as row flows to col
  for (i in 1:p)
  {
    weights[edge[i,1],edge[i,2]] <- edge[i,3]
    weights[edge[i,2],edge[i,1]] <- edge[i,4]
  }
  weights[is.na(weights)] <- 0
  
  V<-E*(nodefile$Area^(-N)) #Extinction Rate
  
  Uin <- calc.w.in(data=weights, V=V, n=p)
  Uou <- apply(weights, 1, sum) #Colonization strength out 
  
  Uset <- rbind(Uin, Uou)
  
  U1 <- apply(Uset, 2, function (x) 1/mean(1/x)) #Calculate harmonic mean
  q <- prod(sapply(subset(U1, U1>0), function(x) x^(1/p))) #Agregate Colonization Rate
  v <- prod(V) #Agregate Extinction Rate
  
  MLT=0
  for (i in 1:p)
  {
    for (k in 1:p)
    {
      MLT <- MLT + ( (1/k) * (factorial(p-i)/factorial(p-k)) * (1/((p-1)^(k-i))) * (q^(k-i)) )
    }
  }
  output <- cbind(p,E,N,U,MLT)
  print(output)
}

MMLT.def(nodefile=nodes, graphfile=g, E=50, N=0.01, U=2)


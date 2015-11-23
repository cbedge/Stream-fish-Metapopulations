library(igraph)

#creates a random habitat network and identifies edges
g <- barabasi.game(n=100, power=1, zero.appeal=1.3) # dendritic graph: n is number of patches, power=1, zero.appeal
plot(g, layout=layout_with_fr, vertex.size=4, vertex.label.dist=0.5, vertex.color="red", edge.arrow.size=0.5) #plots g to see what is going on

####
#FUNCTION - Creates a random dendrictic network built from dwonstream to upstream
#Code from
#S. Kyle McKay, John R. Schramski, Jock N. Conyngham, and J. Craig Fischenich. 2013. 
#Assessing upstream fish passage connectivity with network analysis. Ecological Applications 23:1396-1409
####

network <- function(n,tmax){					#Function to create random adjacency matrix
  A <- matrix(0,nrow=n,ncol=n)				#Create an empty adjacency matrix
  ntemp <- n						#Reset counter for the number of nodes
  ttemp <- tmax						#Reset counter for the number of tributary junctions to insert
  t <- 0							#Reset counter for the realized number of tributary junctions
  
  for (j in 1:n){
    split <- ifelse(round(runif(1,0,1),0)==1,1,0)
    for (i in 1:n){
      if(j>=i){A[i,j] <- 0}			#Generate upstream connectivity using only the "bottom" of the matrix
      else{
        A[i,j] <- ifelse(i==j+1,1,ifelse(ttemp>0,split,0))
      }
      if(sum(A[i,])>1){A[i,j] <- 0}		#Prevent network crossing
      if(sum(A[,j])>2){A[i,j] <- 0}		#Prevent more than one split at a node
      if(i>j+1){ttemp <- ttemp - A[i,j]}	#Reduce the available number of junctions left
      ntemp <- ntemp - A[i,j]			#Reduce the available number of nodes left
    }
    t <- ifelse(sum(A[,j])==2,t+1,t)
  }
  A							#Send outputs
}

Aplot <- network(50,10)
g <- graph.adjacency(t(Aplot), mode="directed")			#Convert adjacency matrix to a graph object
plot(g, layout=layout.reingold.tilford, vertex.size=4, vertex.label.dist=0.5, vertex.color="red", edge.arrow.size=0.5) #plot graph


####
#Set the size and qulaity of each of the nodes. These values can also be provided
####
Area <- sample(10:100, 50, replace=T) #Area of each row
Qual <- sample(0:100, 50, replace=T) #Relative quality of patch where 100 is high and 0 is low.
nodes <- data.frame(Area, Qual)


####
#Get edges of a graph and then assign upstream and downstream permeability. These values can also be provided
####
edges <- data.frame(get.edgelist(g)) #get the edges of the graph
edges$PermDS <- 100 #Assign permeability of each downstream connection between 0 and 100
edges$PermUS <- 100 #Assign permeability of each upstream connection between 0 and 100


####
#FUNCTION
#adds a specified number of dams to a list of edges from a graph. Dams have a permeability of 0
####
add.dams <- function(edge, dams){
  for(i in 1:dams){
    edge[sample(1:nrow(nodes), 1), 4] <- 0
  }
  edge
}

edges.dam <- add.dams(edges, 10)


####
#FUNCTION
#Calculates metapopulation mean lifetime for a graph with the following inputs
#node is a dataframe with area and quality of each node
#edge is a dataframe with two row detailing connections and weights/permeabilities
#E Species specific coeffecint relating to patch size, between 1 and 100
#N level of environmental variation in population growth, between 0.01 and 4. Small is more
#U number of imigrants needed for succussful colonization
#E, N, and U can be user defined but defaults are set currently
####

MMLT.def <- function(node, edge, E=1, N=0.5, U=2){
  p <- nrow(nodes)
  weights <- matrix(nrow=p, ncol=p) #creates matrix of edge weights read as row flows to col
  for (i in 1:p)
  {
    weights[edge[i,1],edge[i,2]] <- edge[i,3]
    weights[edge[i,2],edge[i,1]] <- edge[i,4]
  }
  weights[is.na(weights)] <- 0
  
  V<-E*(node$Area^(-N)) #Extinction Rate
  
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

MMLT.def(node=nodes, edge=edges.dam, E=50, N=0.01, U=2)

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
#Calculation of DCIp and DCIs for a defined network
#li length of segment i
#lj length of segment j
#L length of the entire network
#cij permeability of barrier between segment li and lj
####
L <- sum(nodes$Area)
p <- nrow(nodes)
x <- 0
for (i in 1:p){
  x <- x + (nodes[i,1]/L)
}
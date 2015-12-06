library(igraph)

Aplot <- network(50,35) #create adjacency matrix for a  network
g <- graph.adjacency(t(Aplot), mode="directed")	#Convert adjacency matrix to a graph object
plot(g, layout=layout.reingold.tilford, vertex.size=4, vertex.label.dist=0.5, vertex.color="red", edge.arrow.size=0.5) #plot graph

#Create the starting node attributes
ID <- 1:50
Area <- c(0, sample(10:100, 49, replace=T)) #Area of each segment (downstream length in a riverscape). Area of first segment is set as 0 because it is mouth
Qual <- c(100, sample(0:100, 49, replace=T)) #Relative quality of segment where 100 is high and 0 is low. Quality of first segment is set at 100 because it is mouth
PermDS <- rep(1, 50) #assigning a permeability to move downstream into segment
PermUS <- rep(1, 50) #Assigning a permeability to move upstream out of segment
ASurv <- rep(1, 50) #Assigning Adult survival in each node
JSurv <- rep(1, 50) #Assigning Larval survival in each node
AFec <- rep(10, 50) #Assigning adult fecundity in each node
nodes <- data.frame(ID, Area, Qual, PermDS, PermUS, ASurv, JSurv, AFec) #create a dataframe with the variables for each node

nodes.dam <- add.dams(nodefile=nodes, dams=20) #Add dams (upstream barriers)


#Randomization to check how many barriers to add to system to get a reasonable estimate of DCI
data.out <- NULL
for (ndams in 1:25){
  for (i in 1:100){
    nodes.dam <- add.dams(nodefile=nodes, dams=ndams) #Add dams (upstream barriers)
    dam.loc <- 
    res1 <- cbind(DCI.calc(nodefile = nodes.dam, graphfile = g), ndams)
    data.out <- rbind(data.out, res1)
  }
}

DCI.calc(nodefile = nodes.dam, graphfile = g)
MMLT.def(nodefile=nodes.dam, graphfile=g, E=1, N=0.5, U=2) #calculate metapopulation meanlifetime
Meta.Growth(nodefile=nodes.dam, graphfile=g)



#Simulations early tests to check variables
library(lhs)
data.test <- maximinLHS(n=30, k=5, dup=1)

dams <- 1

Vagi <- data.test[1,1] * 0.5
ASur <- data.test[1,2] * 0.9
JSur <- data.test[1,3] * 0.9
Fecu <- 100 + (data.test[1,4] * 500000)
Dens <- 2 + (data.test[1,5] * 50)
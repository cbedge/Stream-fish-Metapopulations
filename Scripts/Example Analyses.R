#This script runs analyses on a made up set of data

Aplot <- network(50,10) #create adjacency matrix for a  network
g <- graph.adjacency(t(Aplot), mode="directed")	#Convert adjacency matrix to a graph object

#lengths <- sample(10:100, 49, replace=T) #length of each segment. One less than the number of nodes because the source (1) is dropped
#set.edge.attribute(g, "length", index=E(g), value=lengths) #set the length of each path
#set.edge.attribute(g, "weight", index=E(g), value=lengths) #set the weight of each path

plot(g, layout=layout.reingold.tilford, vertex.size=4, vertex.label.dist=0.5, vertex.color="red", edge.arrow.size=0.5) #plot graph

####
#Set the size and qulaity of each of the nodes. These values can also be provided
####
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

DCI.calc(nodefile = nodes.dam, graphfile = g)

MMLT.def(nodefile=nodes.dam, graphfile=g, E=1, N=0.5, U=2) #calculate metapopulation meanlifetime

Meta.Growth(nodefile=nodes.dam, graphfile=g)

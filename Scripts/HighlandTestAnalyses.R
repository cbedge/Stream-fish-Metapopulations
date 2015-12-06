#Taking an edge matrix, barrier data, adn segmetn data and making a graph object

library(igraph)

EdgeWBarID <- read.csv(file="data/StreamNodesAndEdges/High_EdgeList_corrected.csv", header=T)
HighEdge <- EdgeWBarID[,2:3]

High.g <- graph.data.frame(HighEdge, directed=TRUE)

High.g2 <- graph.data.frame(HighEdge, vertices = Nodes, directed=TRUE)
V(High.g)$color<-ifelse(158, 'blue', 'red') #useful for highlighting certain people. Works by matching the name attribute of the vertex to the one specified in the 'ifelse' expression
E(High.g2)$color<-ifelse(158, "blue", "red")

jpeg(file = "high.jpeg", width=20, height=20, units="cm", res=600)
plot(High.g, layout=layout.reingold.tilford, vertex.size=2, vertex.label.cex=0.5, vertex.label.dist=0.2, vertex.color="red", edge.arrow.size=0.1) #plot graph                 
dev.off()

#For this to run correctly segments must be labelled 1, 2, 3... with no missig segments
#Turn an edge list into an adjacency matrix and then create the graph. Currently this works. I am not sure if other methods work too
High.adj <- matrix(0, nrow=256, ncol=256)
for (i in 1:255)
{
  High.adj[HighEdge[i,2], HighEdge[i,1]] <- 1
}

High.g3 <- graph.adjacency(t(High.adj), mode="directed")
plot(High.g3, layout=layout.reingold.tilford, vertex.size=2, vertex.label.cex=0.5, vertex.label.dist=0.2, vertex.color="red", edge.arrow.size=0.1) #plot graph                  

Nodes <- read.csv(file="data/StreamNodesAndEdges/High_JunctDSSeg_corrected.csv", header=T)
Barr.attr <- read.csv(file="data/StreamNodesAndEdges/HighJunctions_corrected.csv", header=T)
Nodes.attr <- read.csv(file="data/StreamNodesAndEdges/HighSegs_corrected.csv", header=T)

names(Nodes)
colnames(Nodes) <- c("Junction", "ID")
Nodes$Area <- Nodes.attr$Shape_Length[match(Nodes$ID, Nodes.attr$OBJECTID)]
Nodes$Qual <- 100
Nodes$PermDS <- 1 #assigning a permeability to move downstream into segment
Nodes$PermUS <- 1 #Assigning a permeability to move upstream out of segment
Nodes$ASurv <- 1 #Assigning Adult survival in each node
Nodes$JSurv <- 1 #Assigning Larval survival in each node
Nodes$AFec <- 1                 
Nodes <- Nodes[,-1]

Barr.attr$perm[match(Nodes$Junction, Barr.attr$OBJECTID)]
getwd()

DCI.calc(nodefile = Nodes, graphfile = High.g3, mouth=158)

write.csv(end, file="ends.csv")


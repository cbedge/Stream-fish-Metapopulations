####
#FUNCTION - meta.growth: Calcuates the metapopulation growth rate
#REQUIRED - A nodefile with the following names
#ID: ID of the segment and barrier
#Area: The length of the segment downstream of the barrier
#Qual: Quality of the segment, currently not used
#PermDS: downstream permeability of the barrier, this is usually 1
#PermUS: upstream permeability of the barrier, ranges between 0 (impermeable) and 1 (permeable)
#ASurv: Adult survival in the node 
#JSurv: juvenile/larval survival in the node
#AFec: Adult fecundity in the node
#
#REQUIRED - A graph object that can be any dendritic network
####
nodefile <- nodes
graphfile <- g

Meta.Growth <- function(nodefile, graphfile){
  n <- round(nodefile$Area * (nodefile$Qual/100)) #Creates a vector of populations sizes rounded to 0 decimal places. Qual is divided by 100 to make it a percent
  N <- sum(n) #Total population size
  
  edge <- data.frame(get.edgelist(graphfile)) #get the edges of the graph
  
  growth.rate <- 0
  for (i in 1:nrow(nodefile)){
    data1 <- rbind(subset(edge, edge$X1==i), subset(edge, edge$X2==i)) 
    var1 <- 0
    
    for (j in 1:nrow(data1)){
      if(data1$X1[j]==i) {
        var1 <- var1 + (nodefile$AFec[i] * (0.1 * nodefile$PermDS[i]))
        }
      else{
        var1 <- var1 + (nodefile[nodefile$ID==data1$X1[j],]$AFec * (0.1 * nodefile[nodefile$ID==data1$X2[j],]$PermUS))
      }
    }
    
    p.lamda <- nodefile$ASurv[i] * nodefile$JSurv[i] * var1
    growth.rate <- growth.rate + (p.lamda * (n[i]/N))
  }
  growth.rate
}
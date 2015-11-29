#This code calculates DCIp and DCId
#A large portion of the code is adapted from Greig Oldfords original work on FIPEX
#It has been adapted to run using R 3.X.X and igraph
#The workflow is largely consistent with Greig's earlier work and he should be cited appropriately

####
#FUNCTION - DCI.calc: calculates DCIp and DCId
#REQUIRES a node file with the following colnames
#ID: ID of the segment and barrier
#Area: The length of the segment downstream of the barrier
#Qual: Quality of the segment, currently not used
#PermDS: downstream permeability of the barrier, this is usually 1
#PermUS: upstream permeability of the barrier, ranges between 0 (impermeable) and 1 (permeable)
#
#REQUIRES a graph object that can be any dendritic network describing the structure of the network
##

DCI.calc <- function(nodefile, graphfile){
  #Construct a table with the start and end of each path between all segments, length of that path,
  #the product of the permeabilites of barriers on that path, the length of the start segment, and the length of the last segment
  all.paths <- NULL
  for (k in 1:nrow(nodefile)) {
    paths <- all_simple_paths(graph=graphfile, from=k, mode = "all") #returns path from a segment to all other segements
    start <- sapply(paths, "[[", 1) #Identify the starting segment
    end <- sapply(paths, tail, 1) #Identify the final segment
    p.length <- NULL 
    pass <- NULL
    for (j in 1:(nrow(nodefile)-1)) {  
      p.length2 <- 0
      pass2 <- 1
        for (i in paths[[j]]) {
          p.length2 <- p.length2 + nodefile$Area[i] #length of the path
          pass2 <- pass2 * (nodefile$PermUS[i] * nodefile$PermDS[i])
        }
      p.length <- c(p.length, p.length2)
      pass <- c(pass, pass2)
    }
    all.paths2 <- data.frame(start, end, p.length, pass)
    itself <- c(k, k, nodefile$Area[k], nodefile$PermUS[k] * nodefile$PermUS[k])
    all.paths <- rbind(all.paths, all.paths2, itself)
  }  
  all.paths$Start.Length <- nodes$Area[match(all.paths$start, nodefile$ID)]
  all.paths$End.Length <- nodes$Area[match(all.paths$end, nodefile$ID)]

  #DCIp Calculation
  tot.length <- sum(nodefile$Area)
  DCIp <- 0
  for (i in 1:nrow(all.paths)) {
    DCIp <- DCIp + (all.paths$pass[i] * (all.paths$Start.Length[i]/tot.length) * (all.paths$End.Length[i]/tot.length))
  }

  #Calculate DCId
  DCId <- 0
  DCId.data <- all.paths[all.paths$end==1, ]
  for (i in 1:nrow(DCId.data)){
    DCId <- DCId + (DCId.data$pass[i] * (DCId.data$Start.Length[i]/tot.length))
  }

  DCI.results <- data.frame(DCIp, DCId)
  colnames(DCI.results) <- c("DCIp", "DCId")
  DCI.results
}

DCI.results <- c(DCIp, DCId)
print(DCI.results)



####
#FUNCTION - DCId.calc: Calculated DCId
#Segment 1 must be the first segment in the network that is the mouth at the ocean or river
###


if (pass2==0) pass2 <- nodefile$PermUS[i] * nodefile$PermDS[i]
else

#Old version below
all.paths <- NULL
for (k in 1:2) {
  paths <- all_simple_paths(graph=g, from=k, mode = "all") #returns path from a segment to other segements
  start <- sapply(paths, "[[", 1)
  end <- sapply(paths, tail, 1)
  
  p.length <- NULL
  pass <- NULL
  for (j in 1:(nrow(nodes)-1)) {  
    p.length2 <- 0
    pass2 <- 0
    for (i in paths[[j]]) {
      p.length2 <- p.length2 + Area[i]
      if (pass2==0) pass2 <- PermUS[i] * PermDS[i]
      else
        pass2 <- pass2 * (PermUS[i] * PermDS[i])
    }
    p.length <- c(p.length, p.length2)
    pass <- c(pass, pass2)
  }
  all.paths2 <- data.frame(start, end, p.length, pass)
  all.paths <- rbind(all.paths, all.paths2)
}





all.paths <- NULL
for (k in 1:nrow(nodes)) {
  paths <- all_simple_paths(graph=g, from=k, mode = "all") #returns path from a segment to other segements
  start <- sapply(paths, "[[", 1)
  end <- sapply(paths, tail, 1)
  
  p.length <- NULL
  pass <- NULL
  for (j in 1:(nrow(nodes)-1)) {  
    p.length2 <- 0
    pass2 <- 0
    for (i in paths[[j]]) {
      p.length2 <- p.length2 + Area[i]
      if (pass2==0) pass2 <- PermUS[i] * PermDS[i]
      else
        pass2 <- pass2 * (PermUS[i] * PermDS[i])
    }
    p.length <- c(p.length, p.length2)
    pass <- c(pass, pass2)
  }
  all.paths2 <- data.frame(start, end, p.length, pass)
  itself <- c(k, k, Area[k], PermUS[k]*PermUS[k])
  all.paths <- rbind(all.paths, all.paths2, itself)
}  
all.paths$Start.Length <- nodes$Area[match(all.paths$start, nodes$ID)]
all.paths$End.Length <- nodes$Area[match(all.paths$end, nodes$ID)]

#DCIp Calculation
tot.length <- sum(nodes$Area)

DCIp <- 0
for (i in 1:nrow(all.paths)) {
  DCIp <- DCIp + (all.paths$pass[i] * (all.paths$Start.Length[i]/tot.length) * (all.paths$End.Length[i]/tot.length))
}
DCIp
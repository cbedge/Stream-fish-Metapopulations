library(igraph)
#The functions below create various types of networks

#creates a random habitat network
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








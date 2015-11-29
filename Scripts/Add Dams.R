####
#FUNCTION
#adds a specified number of dams to a list of edges from a graph. Dams have a permeability of 0
####


add.dams <- function(nodefile, dams){
   for (i in 1:dams){
     nodefile$PermUS[sample(1:nrow(nodefile),1)] <- 0
   }
  nodefile
}
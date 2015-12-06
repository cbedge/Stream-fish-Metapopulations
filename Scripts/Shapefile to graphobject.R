library(rgdal)
library(igraph)
library(dismo)

#Takes ESRI shapefiles from ARC and and converts them into proper files for calculations
#Requires a shapefile with stream segment

getwd()

#turning a shapefile into a graph object
Stream.shp <- readOGR("data/Shapefiles", "WC_Highland")
print(proj4string(Stream.shp))

Stream.mer <- spTransform(Stream.shp,  CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs")) #Transform projection to mercator to work with google map
HighArea<-gmap(Stream.mer, type="satellite")
plot(HighArea)
lines(Stream.mer, col="blue", lwd=2.0)

# First build a data.frame of network segment end point coordinates
# That first line is the hardest part, and there may be a much easier
# way to do it (e.g. in ArcGIS if you have
# access to that; but it's nice to have a 'pure R' solution...)

c<-t(sapply(unlist(coordinates(Stream.shp),recursive=FALSE),FUN=function(x) cbind(x[1,],x[nrow(x),])))
c<-as.data.frame(c)
names(c)<-c("From.x","From.y","To.x","To.y")

# Then complete the vertex table
n2 <- cbind(Stream.shp,c)
v<-unique(rbind(data.frame(X=n2$From.x,Y=n2$From.y),data.frame(X=n2$To.x,Y=n2$To.y)))
v$ID <- 1:nrow(v)

# Then match back to the original network coordinates to assign vertex IDs to each feature end point
n3<-merge(n2,data.frame(From=v$ID,From.x=v$X,From.y=v$Y),by=c("From.x","From.y"))
n4<-merge(n3,data.frame(To=v$ID,To.x=v$X,To.y=v$Y),by=c("To.x","To.y"))

# Then make an igraph
# remember igraph indices are 0-based (although if you don't, you'lljust end up with one unconnected vertex labeled 0)
edge.list <- cbind(n4[,c("From","To")],n4[,grep("^(From|To)$",names(n4),invert=TRUE)])
edge.list$From <- edge.list$From-1    # 0-based conversion
edge.list$To <- edge.list$To-1

g <- graph.data.frame(edge.list, directed=F) # All the shapefile attributes are now edge attributes

V(g)$color<-ifelse(V(g)$OBJECTID==157, 'blue', 'red') #useful for highlighting certain people. Works by matching the name attribute of the vertex to the one specified in the 'ifelse' expression
E(g)$color<-ifelse(E(g)$OBJECTID==0, "blue", "white")

plot(g, layout=layout.reingold.tilford, vertex.size=2, vertex.label.dist=0.2, vertex.label.cex=0.5, edge.arrow.size=0.5) #plot graph


library(Matrix)
m <- sparseMatrix( i=n4$From, j=n4$To, x=n4$OBJECTID )


edge1<-get.edgelist(g)

edge1[edge1[,1]==158,]


edge.list[259,]
edge.list$OBJECTID==74
vertex.color="red", 
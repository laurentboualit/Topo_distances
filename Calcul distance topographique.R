
library(rgeos)
library(raster)
library(sp)
library(readxl)
library(xlsx)

rm(list=ls())

setwd("/home/laurent/Dropbox/Boulot/Projets/Tetrao_urogallus/Papier 1/r/New 2.0/Calcul dist topo")
matrix <- read_excel("/home/laurent/Dropbox/Boulot/Projets/Tetrao_urogallus/Papier 1/r/New 2.0/Calcul dist topo/matrice base.xlsx") # Euclidian distance matrices for importing paired coordinates
coord <- read_excel("/home/laurent/Dropbox/Boulot/Projets/Tetrao_urogallus/Papier 1/r/New 2.0/Calcul dist topo/Centroides_leks.xlsx") #Matrices containing sites and coodinates in L93 system

#matrix <- matrix[c(11,12,13),]

r <-raster("/home/laurent/Dropbox/Boulot/Projets/Tetrao_urogallus/Données/BDALTI/Big1") #Read the raster
proj4string(r) <- "+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs" #Assign the projection system

stock <- matrix(NA,nrow(matrix),ncol(matrix)) # Creation matrix containing topo. distances
stock <- data.frame(stock)
stock[,1] <- matrix[,1]
stock[,2] <- matrix[,2]
names(stock) <- c("InputID","OutputID","Topo_Dist")

for (i in 1:nrow(matrix)) { 
  
  #Get the sites 2 sites names
  Patch1 <- matrix$InputID[i]
  Patch2 <- matrix$TargetID[i]
  #Extract sites informations
  lek1 <- subset(coord, Code_entier == Patch1 )
  lek2 <- subset(coord, Code_entier == Patch2 )
  #Extract sites coordinates
  X_lek1 <- lek1$X_L93_Centroide
  Y_lek1 <- lek1$Y_L93_Centroide
  X_lek2 <- lek2$X_L93_Centroide
  Y_lek2 <- lek2$Y_L93_Centroide
  #table containing both coordinates.
  xy <- data.frame(x= c(X_lek1, X_lek2), y=c(Y_lek1, Y_lek2))
  
  #creation of a spatial line between the sites
  l <- SpatialLines(list(Lines(list(Line(xy)),ID="1"))) 
  
  #creation of points along the spatial line
  
  numOfPoints  <-  gLength(l)*10 #segments of 10cm along the spacial line
  points<-spsample(l, n = numOfPoints, type = "regular")
  
  #Get the altitude of each point
  points.df<-as.data.frame(points)
  points.df$elevation<-raster::extract(r, points.df)
  
  #Evaluate distance between the two coordinates
  
  head(points.df)
  output<-data.frame(length=1:length(points.df$x))
  j.2<-(length(points.df$x)-1) #The last point has no more point after by definition
  for(j in 1:j.2){
    
    x1<-points.df[j,"x"]
    x2<-points.df[j+1,"x"]
    y1<-points.df[j,"y"]
    y2<-points.df[j+1,"y"]
    z1<-points.df[j,"elevation"]
    z2<-points.df[j+1,"elevation"]
    
    
    Distance<-((x1-x2)^2+(y1-y2)^2+(z1-z2)^2)^0.5 #Pythagoras is our friend
    
    output[j,]<-Distance
  }
  output[length(points.df$x),]<-0 #The last point has no more point after by definition
  
  dist <- sum(output) # Length between A and B
  
  #for (z in 1:nrow(stock)) {
    stock$Topo_Dist[i] <- dist
    View(stock)
    
  #}
  
}

write.xlsx(stock, "Mat_dist_topo.xlsx", sheetName="Sheet1",col.names=TRUE, row.names=FALSE, append=FALSE, showNA=TRUE)

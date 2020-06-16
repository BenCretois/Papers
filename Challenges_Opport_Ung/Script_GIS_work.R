###Libraries
.libPaths("C:/R/R-3.5.1/library")
library(ggplot2)
library(GISTools)
library(dplyr)
library(raster)
library(rgdal)
library(reshape2)
###Functions for the script
##Draw a common attribute to merge
same<-function(x){
  x$same <- 1
  return(x)}

##Merge the polygons for each shapefile of the list
merge<- function(x){
  x.transform <- unionSpatialPolygons(x, x@data$same)
  return(x.transform)}

##Calculate the occupied surface
calc.area <- function(x){
  y <- poly.areas(x) / (1000*1000)
  return(y)
}

###Read the polyogns dataset
setwd("//home.ansatt.ntnu.no/benjamcr/Desktop/Research_Project/UngulatePAPER/Ungulates")
l <- list.files(pattern="\\.shp")


##Extracting the names to create a dataset after
v.names <- sapply(l, function(x) sub("\\.shp$", "", x))
l.spdf <- lapply(l, readOGR)
names(l.spdf) <- v.names

##Merge the polygons
l.spdf.same <- lapply(l.spdf, same)
l.spdf.merge <- lapply(l.spdf.same, merge)

##Shapefile europe 
europe <- shapefile("//home.ansatt.ntnu.no/benjamcr/Desktop/Data/Countryborders/MyAnalysisEurope.shp")
europe <- same(europe)
europe_merge <- unionSpatialPolygons(europe, europe$same)
surface_europe <- calc.area(europe_merge)

#raster PA Europe
rp <- raster("//home.ansatt.ntnu.no/benjamcr/Desktop/Research_Project/Project_DistributionUngulates/Europe_R/RasterPAEU")
PA_EU_area <- cellStats(rp, stat=sum)

#In PA for species (in km2)
setwd("//home.ansatt.ntnu.no/benjamcr/Desktop/Research_Project/UngulatePAPER/raster_sp")
l_raster_sp <- list.files(pattern="\\.grd")
l_raster_sp <- lapply(l_raster_sp, raster)
cs <- lapply(l_raster_sp, function(x) cellStats(x, stat=sum))
in_PA_surface <- as.numeric(unlist(cs))

##Take the polygons inside EU
#gIntersection with Europe (more accurrate areas)
l.intersec <- lapply(l.spdf.merge, function(x) intersect(x, europe_merge))
l_surface_EU <- lapply(l.intersec, calc.area)
in_EU_surface <- as.numeric(unlist(l_surface_EU))

#build dataframe with all the areas
sp_names <- c("Chamois", "Iberian Ibex", "Ibex","Isard", "Moose", "Red deer", "Roe deer", "Wild Reindeer", "Wild boar")

EU <- data.frame("in_EU"=surface_europe, "in_PA"=PA_EU_area, "sp"="Europe")
df <- data.frame("in_EU"=in_EU_surface, 
                 "in_PA"=in_PA_surface, 
                 "sp"=sp_names, 
                 row.names = NULL) %>% 
  rbind(EU) %>% 
  mutate(non_PA = in_EU - in_PA) %>% 
  mutate(in_nonPA_Per = (non_PA / in_EU)) %>% 
  mutate(in_PA_Per = (in_PA / in_EU))  


df$sp <- factor(df$sp, levels=c("Moose", "Europe","Roe deer", "Wild Reindeer","Wild boar","Red deer",
                                "Ibex", "Chamois", "Iberian Ibex", "Isard"))

#Plot in percent
df_percent <- df %>%dplyr::select(sp, in_nonPA_Per, in_PA_Per)
datm <- melt(df_percent, id.vars = "sp")
ggplot(datm, aes(x=sp, y=value, fill=variable))+
  geom_bar(stat="identity", position="fill")+
  coord_flip()+
  scale_fill_manual(values=c("gray48", "gold3"),
                    labels=c("Outside PA", "Inside PA"))+
  theme_minimal()

###Make the maps
library(tmap)
moose <- l.spdf[[5]]
moose[is.na(moose$Ratio),] = 0 
moose <- moose[moose$Ratio < 0.5,]
dd <- moose@data

RDeer <- l.spdf[[6]]
RDeer[is.na(RDeer$Ratio),] = 0 
RDeer <- RDeer[RDeer$Ratio < 0.5,]

RoeDeer <- l.spdf[[7]]
RoeDeer[is.na(RoeDeer$Ratio),] = 0
RoeDeer <- RoeDeer[RoeDeer$Ratio < 0.5,]


#Chamois
tm_shape(europe_same) + 
  tm_polygons(col = "white") +
tm_shape(l.spdf[[1]]) + 
  tm_polygons(col="darkblue", border.col = "darkblue")+
tm_layout(frame = F)

#IberianIbex
tm_shape(europe_same) + 
  tm_polygons(col = "white") +
  tm_shape(l.spdf[[2]]) + 
  tm_polygons(col="darkblue", border.col = "darkblue")+
  tm_layout(frame = F)


#Ibex
tm_shape(europe_same) + 
  tm_polygons(col = "white") +
  tm_shape(l.spdf[[3]]) + 
  tm_polygons(col="darkblue", border.col = "darkblue")+
  tm_layout(frame = F)


#Isard
tm_shape(europe_same) + 
  tm_polygons(col = "white") +
  tm_shape(l.spdf[[4]]) + 
  tm_polygons(col="darkblue", border.col = "darkblue")+
  tm_layout(frame = F)


#Moose
tm_shape(europe_same) + 
  tm_polygons(col = "white") +
  tm_shape(moose) + 
  tm_polygons(col="darkblue", border.col = "darkblue")+
  tm_layout(frame = F)


#RedDeer
tm_shape(europe_same) + 
  tm_polygons(col = "white") +
  tm_shape(RDeer) + 
  tm_polygons(col="darkblue", border.col = "darkblue")+
  tm_layout(frame = F)


#RoeDeer
tm_shape(europe_same) + 
  tm_polygons(col = "white") +
  tm_shape(RoeDeer) + 
  tm_polygons(col="darkblue", border.col = "darkblue")+
  tm_layout(frame = F)


#Wild Boar
tm_shape(europe_same) + 
  tm_polygons(col = "white") +
  tm_shape(l.spdf[[8]]) + 
  tm_polygons(col="darkblue", border.col = "darkblue")+
  tm_layout(frame = F)


#Wild Reindeer
tm_shape(europe_same) + 
  tm_polygons(col = "white") +
  tm_shape(l.spdf[[1]]) + 
  tm_polygons(col="darkblue", border.col = "darkblue")+
  tm_layout(frame = F)


setwd("//home.ansatt.ntnu.no/benjamcr/Desktop/Research_Project/Project_DistributionUngulates")
data <- read.csv("Final_Data.csv", header=T) 
PA <- read.csv("Europe_R/PACount.csv", header=T)

final <- full_join(data, PA, by="id") %>% dplyr::select(id, xcoord, ycoord, SnowCovDur,
                                                        PETWQ, TRI, HFI, PA=SUM, Bears,Chamois, IberianIbex,
                                                        Ibex,Isard,Lynx, Moose,RedDeer,RoeDeer, WildBoar,
                                                        WildReindeer, Wolverines, Wolves)

ggplot(final, aes(xcoord,ycoord,col=PA))+
  geom_point()

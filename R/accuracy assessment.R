library(raster)
library(rgdal)
library(dplyr)
# read a raster, GeoTiff or something
forSampling <- raster('indices/Random_forest_All_new1.tif')

classes<-as.data.frame(getValues(forSampling))

classes<-classes %>% rename("class"=`getValues(forSampling)`) %>%
  filter(!is.na(class)) %>%
  group_by(class) %>% summarise(size=n()) %>% 
  mutate(size=round(1000*size/sum(size)))
  


point_class<-function(c,s){
  
  r0<-forSampling
  r0[r0!=c]<-NA
  
  sampleSp <- sampleStratified(x = r0, size = s, xy = TRUE, sp = TRUE)
  
  names(sampleSp)<-c("cell","x","y","class")
  
  return(sampleSp)
  
}

sampled<-mapply(point_class, classes$class,classes$size)

final.points<-do.call("rbind",sampled)

# make stratified random sampling
# set sp = TRUE to get a spatialPointsDataframe
# that one can easily be converted into a shapefile
sampleSp <- sampleStratified(x = forSampling, size = 50, xy = TRUE, sp = TRUE)

# write it out to a shapefile for further processing
writeOGR(obj = sampleSp, dsn = "aa", layer = "sampleSp", driver="ESRI Shapefile") # this is in geographical projection

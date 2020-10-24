library(rgdal)
library(raster)
library(sf)
library(sp)
library(dplyr)
shp<-readRDS('data/gadm36_SEN_4_sf.rds')
sedhiou<- readOGR('data/SEN_adm_shp/SEN_adm4.shp')
# bignona <- sedhiou[sedhiou$NAME_4%in% c("Kafountine"),]
bignona <- sedhiou[sedhiou$NAME_3%in% c("Notto"),]

list_raster<-vector(mode = 'list',length = 7)
### data preparation

for (i in 1:7) {

  annee<-2012+i
  folder<-paste0('data/analysis/',annee,'/')
  list_file<-paste0(folder,
                    list.files(path = folder,pattern = '.TIF'))
  list_file<-gsub('TIF','tif',list_file[1:])
  raster_data<-raster(list_file[1])
  bignona <- spTransform(bignona, CRS(proj4string(raster_data)))
  list_raster[[i]]<-crop(stack(list_file), bignona)
  list_raster[[i]]<-mask(list_raster[[i]],bignona)
  
}


writeRaster(list_raster[[7]][[]],"NOTTO.tif")

par(mfrow=c(2,2))

plot(list_raster[[1]])
plot(list_raster[[2]])
plot(list_raster[[3]])
plot(list_raster[[4]])

vi <- function(img, k, i) {
  bk <- img[[k]]
  bi <- img[[i]]
  vi <- (bk - bi) / (bk + bi)
  return(vi)
}


res<-lapply(list_raster, vi,5,4)


lst_raster<-list()

name<-'LC08_L1TP_205050_20191205_20191217_01_T1_B'

for (i in 1:10) {
  
  # lst_raster[[i]]<-raster(paste0('data/South_SN/LC08_L1TP_205051_20190511_20190521_01_T1_B',i,'.tif'))
  
  lst_raster[[i]]<-raster(paste0('data/South_SN1/LC08_L1TP_205050_20191205_20191217_01_T1_B',i,'.tif'))
  
  bignona <- spTransform(bignona, CRS(proj4string(lst_raster[[i]])))
  lst_raster[[i]]<- crop(lst_raster[[i]], bignona)
  lst_raster[[i]] <- mask(lst_raster[[i]], bignona)
  
}

# par(mfrow=c(4,2))

plot(lst_raster[[1]],axes=FALSE)

rgb<-raster::stack(lst_raster[[4]],lst_raster[[3]],lst_raster[[2]])

plotRGB(rgb,axes=T,stretch = "lin")


filenames<-paste0('data/South_SN/LC08_L1TP_205051_20190511_20190521_01_T1_B',1:7,'.tif')

# data<-stack(filenames)

data<-stack(lst_raster[[1]],
            lst_raster[[2]],
            lst_raster[[3]],
            lst_raster[[4]],
            lst_raster[[5]],
            lst_raster[[6]],
            lst_raster[[7]])





bignona <- spTransform(bignona, CRS(proj4string(lst_raster[[i]])))
data1<- crop(data, bignona)

ndvi <- vi(data, 5, 4)
ndwi <- vi(data, 3, 5)

par(mfrow=c(1,1))
plot(ndvi, col = rev(terrain.colors(10)), main = "Indice de la vegetagion 2019")
plot(bignona,add=T)

veg <- reclassify(ndwi, cbind(-Inf, -0.2, NA))
plot(veg, main='Vegetation')

hist(ndvi,prob=TRUE)
lines(density(ndvi))



writeRaster(ndvi,"ndvi_yeumbeul.tif")



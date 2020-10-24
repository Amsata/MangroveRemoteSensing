memory.limit()
memory.limit(9999999999)

#### Import packages
library(rgdal)
library(raster)
library(dplyr)
library(RStoolbox)
library(plyr)
#library(keras)
#library(tfruns)
#library(tfestimators)
library(rgdal)
library(sf)
library(sp)
library(dplyr)
library(RStoolbox)
library(ggplot2)
library(tmap)
library(rasterVis)
library(data.table)
library(ggpubr)
library(randomForest)

rasterOptions(tmpdir='Temp')

source("R/fonctions.R")
shp<-readRDS('data/gadm36_SEN_4_sf.rds')
senegal<- readOGR('data/SEN_adm_shp/SEN_adm4.shp')
polyg <- senegal[senegal$NAME_3%in% c("Niodior","Fimela","Toubacouta"),]
landuse<-readOGR("data/occ_sol/Occ_sol.shp")
# limite<-readOGR("data/occ_sol/limite.shp")
#polyg<- readOGR('data/Saloum/saloum.shp')
# polyg<-limite

fichier<-list.files(file.path("data","data_final"))
sous_fichier<-gsub(".tar","",fichier)
chemin<-file.path("data","data_final",fichier,sous_fichier)
metadonne<-paste0(chemin,"/",sous_fichier,"_MTL.txt")

metaD<-lapply(metadonne, readTML)

SPACECRAFT_ID<-unlist(lapply(metaD, function(x){tt(x,"SPACECRAFT_ID")}))

DATE_ACQUIRED<-unlist(lapply(metaD, function(x){tt(x,"DATE_ACQUIRED")}))
SENSOR_ID<-unlist(lapply(metaD, function(x){tt(x,"SENSOR_ID")}))
WRS_PATH<-unlist(lapply(metaD, function(x){tt(x,"WRS_PATH")}))
WRS_ROW<-unlist(lapply(metaD, function(x){tt(x,"WRS_ROW")}))
CLOUD_COVER<-unlist(lapply(metaD, function(x){tt(x,"CLOUD_COVER")}))

medaDf<-data.frame(cbind(
  FICHIER=as.character(chemin),
  SPACECRAFT_ID=SPACECRAFT_ID,
  DATE_ACQUIRED=DATE_ACQUIRED,
  SENSOR_ID=SENSOR_ID,
  WRS_PATH=WRS_PATH,
  WRS_ROW=WRS_ROW,
  CLOUD_COVER=CLOUD_COVER
),stringsAsFactors = FALSE)

medaDf<-medaDf %>% mutate(YEAR=substr(DATE_ACQUIRED,1,5),
                          MONTH=substr(DATE_ACQUIRED,7,8),
                          CLOUD_COVER=as.numeric(CLOUD_COVER),
                          WRS_ROW=as.numeric(WRS_ROW)
)

touse<-medaDf %>% group_by(SPACECRAFT_ID,YEAR,MONTH) %>%
  # filter(CLOUD_COVER==min(CLOUD_COVER)) %>% 
  # ungroup() %>% 
  #filter(DATE_ACQUIRED %in% c("2018-12-18")) %>% 
  filter(SPACECRAFT_ID %in% c(" LANDSAT_8")) %>% 
  filter(CLOUD_COVER<=2)

touse$SPACECRAFT_ID<-gsub(" ","",touse$SPACECRAFT_ID)
touse$YEAR<-as.numeric(gsub(" ","",touse$YEAR))

saloum<-touse %>% filter(WRS_ROW==50 & WRS_PATH==" 205")
# saloum<-touse %>% filter(WRS_ROW==50 & WRS_PATH==" 205")


data_ldst_8<-lapply(saloum$FICHIER[c(20,25)],readLandsat2)

# data<-stack(data_ldst_8)

NDVI<-lapply(data_ldst_8, function(x){NDVI_func(x,"LANDSAT_8")})
NDWI<-lapply(data_ldst_8, function(x){NDWI_func(x,"LANDSAT_8")})
BSI<-lapply(data_ldst_8, function(x){BSI_func(x,"LANDSAT_8")})

MVI<-lapply(data_ldst_8, function(x){MVI_func(x)})

MI<-lapply(data_ldst_8, function(x){MI_func(x)})
DIFF<-lapply(data_ldst_8, function(x){DIFF_func(x)})
SI5<-lapply(data_ldst_8, function(x){SI5_func(x)})

AMSA<-lapply(data_ldst_8, function(x){AMSA_func(x,"LANDSAT_8")})
# writeRaster(AMSA,"indices/AMSA.tif",overwrite=TRUE)

#The Normalized Difference Soil Moisture Index, NDSMI--------------
NDSMI<-lapply(data_ldst_8, function(x){NDSMI_func(x)})
# writeRaster(NDSMI,"indices/NDSMI.tif",overwrite=TRUE)

NRI<-lapply(data_ldst_8, function(x){NRI_func(x)})
# writeRaster(NRI,"indices/NRI.tif",overwrite=TRUE)

PSRI<-lapply(data_ldst_8, function(x){PSRI_func(x)})
# writeRaster(PSRI,"indices/PSRI.tif",overwrite=TRUE)


##Salinity index - SI1
SI1<-lapply(data_ldst_8, function(x){SI1_func(x)})
# writeRaster(SI1,"indices/SI1.tif",overwrite=TRUE)

#Normalized Difference infrared Index
NDII<-lapply(data_ldst_8, function(x){NDII_func(x)})
# writeRaster(NDII,"indices/NDII.tif",overwrite=TRUE)


NDII<-lapply(NDII,normImage)
SI1<-lapply(SI1,normImage)
PSRI<-lapply(PSRI,normImage)
NRI<-lapply(NRI,normImage)
NDSMI<-lapply(NDSMI,normImage)
AMSA<-lapply(AMSA,normImage)
NDVI<-lapply(NDVI,normImage)
NDWI<-lapply(NDWI,normImage)
BSI<-lapply(BSI,normImage)
MVI<-lapply(MVI,normImage)
SI5<-lapply(SI5,normImage)
SI5<-lapply(SI5,normImage)


cellStats(MVI[[7]],min)
cellStats(MVI[[7]],max)

cellStats(MI[[7]],min)
cellStats(MI[[7]],max)

cellStats(DIFF[[7]],min)
cellStats(DIFF[[7]],max)


#  polyg <- spTransform(polyg, CRS(proj4string(MVI[[1]])))
# 
# 
# data_ndvi<-stack(NDVI)
# MVI<-stack(MVI)
# 
# 
# writeRaster(MVI[[7]],"indices/MVI.tif",overwrite=TRUE)
# 
# writeRaster(MI[[7]],"indices/MImang.tif",overwrite=TRUE)
# writeRaster(DIFF[[7]],"indices/DIFF.tif",overwrite=TRUE)
# 
# 
# 
# ggplot(d,aes(x=MVI,color=as.factor(class)))+geom_density()



covs<-vector(mode="list",length = length(MVI))

for (i in 1:length(covs)) {
  covs[[i]] <- raster::stack(NDVI[[i]],NDWI[[i]],AMSA[[i]],NRI[[i]],MVI[[i]],
                             SI5[[i]],SI1[[i]],PSRI[[i]],NDII[[i]],NDSMI[[i]])
  
  names(covs[[i]]) <- c('NDVI','NDWI','AMSA','NRI', 'MVI','SI5','SI1','PSRI','NDII','NDSMI')
}



samp <- readOGR('data/test/test.shp')
samp <- spTransform(samp, CRS(proj4string(MVI[[1]])))

saveRDS(samp,"samp.rds")

testing<-readRDS("samp.rds")

testing@data$Code<-as.numeric(testing@data$id)

classes <- rasterize(testing, MVI[[1]], field='Code')



r=covs[[1]]
covmasked <- mask(r, classes)

## Combine this new brick with the classes layer to make our input training dataset
names(classes) <- "class"
#names(classesID) <- "classID"

trainingbrick <- addLayer(covmasked, classes)
#plot(trainingbrick)

## Extract all values into a matrix
valuetable <- getValues(trainingbrick)
valuetable <- na.omit(valuetable)
valuetable <- as.data.frame(valuetable)

for (col in names(valuetable)[1:length(names(valuetable))-1]) {
  valuetable<-rm_outlier(valuetable,col)
}



valuetable$class <- factor(valuetable$class, levels = c(1:length(unique(valuetable$class))))

subset_vect<-c(1,length(names(valuetable))-1)

#<-setdiff(subset_vect,6)

train <- sample(nrow(valuetable), 0.7*nrow(valuetable), replace = FALSE)
TrainSet <- valuetable[train,c(subset_vect,length(names(valuetable)))]
ValidSet <- valuetable[-train,c(subset_vect,length(names(valuetable)))]
#descriptive analysis
#pairs(r)

#Photointerpretation quality check

modelRF <- randomForest(x=TrainSet[ ,1:length(subset_vect)], y=TrainSet$class,
                        importance = TRUE)


# to make the confusion matrix more readable
colnames(modelRF$confusion) <- c("Mangrove", "Eaux", "Tanne claire","tanne fonce", "sol","autres veg", "class.error")
rownames(modelRF$confusion) <- c("Mangrove", "Eaux", "Tanne claire","tanne fonce", "sol","autres veg")
modelRF$confusion

training_ac<-sum(diag(modelRF$confusion[,1:6]))/sum(modelRF$confusion[,1:6])
training_ac

sensi_mang<-modelRF$confusion[1:1]/sum(modelRF$confusion[,1])
sensi_mang

d=as.data.frame(modelRF$confusion)

varImpPlot(modelRF)
#testing in the testing set
predValid <- predict(modelRF, ValidSet[,1:length(subset_vect)], type = "class")
table(predValid,ValidSet$class)

e=as.matrix(table(predValid,ValidSet$class))

colnames(e) <- c("Mangrove", "Eaux", "Tanne claire","tanne fonce", "sol","autres veg")
rownames(e) <- c("Mangrove", "Eaux", "Tanne claire","tanne fonce", "sol","autres veg")

test_ac<-sum(diag(e))/sum(e)
test_ac

sensi_mangt<-e[1,1]/sum(e[,1])
sensi_mangt


predLC <- raster::predict(subset(r,subset_vect), model=modelRF, na.rm=TRUE)

writeRaster(predLC,"indices/Random_forest_saloum.tif",overwrite=TRUE)

predLC2 <- raster::predict(subset(covs[[2]],subset_vect), model=modelRF, na.rm=TRUE)

writeRaster(predLC2,"indices/Random_forest_saloum20.tif",overwrite=TRUE)



a<-NDVI[[4]]

a[predLC!=1]<-NA

b=a
b[a<0.2]<-NA

hist(a,breaks=60)

writeRaster(b,"indices/ndvi_salou.tif",overwrite=TRUE)










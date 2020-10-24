#### Import packages
library(rgdal)
library(raster)
library(dplyr)
library(RStoolbox)
library(plyr)
library(keras)
library(tfruns)
library(tfestimators)
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

source("R/fonctions.R")
shp<-readRDS('data/gadm36_SEN_4_sf.rds')
senegal<- readOGR('data/SEN_adm_shp/SEN_adm4.shp')

landuse<-readOGR("data/occ_sol/Occ_sol.shp")
#polyg<- readOGR('data/studied_area/studied_area.shp')
polyg<-landuse

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

saloum<-touse %>% filter(WRS_ROW==50 & WRS_PATH==" 205")
casa1<-touse %>% filter(WRS_ROW==51 & WRS_PATH==" 205")
casa2<-touse %>% filter(WRS_ROW==51 & WRS_PATH==" 204")



satellite<-c(saloum$SPACECRAFT_ID[20],
             casa1$SPACECRAFT_ID[10],
             casa2$SPACECRAFT_ID[1])
ldst_8<-c(saloum$FICHIER[20],
          casa1$FICHIER[10],
          casa2$FICHIER[1])

ldst_8_date<-c(saloum$DATE_ACQUIRED[20],
               casa1$DATE_ACQUIRED[10],
               casa2$DATE_ACQUIRED[1])

data_ldst_8<-lapply(ldst_8,readLandsat)

#Mosaic-ing

data_mosaic<-vector(mode="list",length = 7)
pairs <- mapply(list, as.list(data_ldst_8[[1]]), 
                as.list(data_ldst_8[[2]]),
                as.list(data_ldst_8[[3]]), SIMPLIFY=F)

mosaic<-lapply(pairs, function(lst){
  lst$fun <- mean
  do.call("mosaic",lst)}
  )
data_ldst_8<-stack(mosaic)




NDVI<-NDVI_func(data_ldst_8,"LANDSAT_8")
NDWI<-NDWI_func(data_ldst_8,"LANDSAT_8")
AMSA<-AMSA_func(data_ldst_8,"LANDSAT_8")
BSI<-BSI_func(data_ldst_8,"LANDSAT_8")
AVI<-AVI_func(data_ldst_8)
SATVI<-SATVI_func(data_ldst_8)

MSAVI<-MSAVI_func(data_ldst_8,"LANDSAT_8")
SAVI<-SAVI_func(data_ldst_8,"LANDSAT_8")
NDBI<-NDBI_func(data_ldst_8,"LANDSAT_8")
NDMI<-NDMI_func(data_ldst_8,"LANDSAT_8")
CSI<-CSI_func(data_ldst_8)

CSI<-normImage(CSI)
SATVI<-normImage(SATVI)
NDBI<-normImage(NDBI)
MSAVI<-normImage(MSAVI)
SAVI<-normImage(SAVI)
NDMI<-normImage(NDMI)

writeRaster(SATVI,"indices/SATVI_NEW.tif",overwrite=TRUE)
writeRaster(CSI,"indices/CSI_NEW.tif",overwrite=TRUE)
writeRaster(NDBI,"indices/NDBI_NEW.tif",overwrite=TRUE)
writeRaster(MSAVI,"indices/MSAVI_NEW.tif",overwrite=TRUE)
writeRaster(SAVI,"indices/SAVI_NEW.tif",overwrite=TRUE)
writeRaster(NDMI,"indices/NDMI_NEW.tif",overwrite=TRUE)


# Graphicx
# trellis.device('png', file = 'myPic%02d.png')
# rasterVis::levelplot(stack(NDVI),scales=list(draw=FALSE)) 
# # levelplot(s, layout = c(3, 4))
# dev.off()

AVI<-normImage(AVI)
BSI<-normImage(BSI)
NDWI<-normImage(NDWI)
AMSA<-normImage(AMSA)
NDVI<-normImage(NDVI)
#data_ldst_8<-normImage(data_ldst_8)



writeRaster(AVI,"indices/AVI_NEW.tif",overwrite=TRUE)
writeRaster(NDVI,"indices/NDVI_NEW.tif",overwrite=TRUE)
writeRaster(AMSA,"indices/AMSA_NEW.tif",overwrite=TRUE)
writeRaster(NDWI,"indices/NDWI_NEW.tif",overwrite=TRUE)
writeRaster(BSI,"indices/BSI_NEW.tif",overwrite=TRUE)

#covs<-vector(mode="list",length = length(NDVI))

#for (i in 1:length(covs)) {
  covs<- raster::stack(NDVI,NDWI,BSI,AVI,AMSA,CSI,SAVI,MSAVI,
                             data_ldst_8)
  
  names(covs) <- c('ndvi', 'ndwi','bsi','avi','amsa','csi','savi','msavi',
                        'band1','band2','band3','band4',
                        'band5','band6','band7')
#}
  
occ_sol <- readOGR('data/occ_sol_4326/occ_sol4326.shp')
occ_sol <- spTransform(occ_sol, CRS(proj4string(NDVI)))
  
saveRDS(occ_sol,"occ_sol.rds")
  
occ_sol<-readRDS("occ_sol.rds") 
  
occ_sol@data<-occ_sol@data %>% 
  mutate(dummy=ifelse(NATURE %in% c("Savane arborÃ©e ou arbustive","ForÃªt"),1,0),
         dummy1=ifelse(NATURE %in% c("Zone humide"),1,0))

Dummy <- rasterize(occ_sol, NDVI, field='dummy')
Dummy[is.na(Dummy)]<-0
# Dummy1 <- rasterize(occ_sol, NDVI, field='dummy')
# Dummy1[is.na(Dummy1)]<-0


samp <- readOGR('data/Niodior_shp/testing/test.shp')
samp <- spTransform(samp, CRS(proj4string(NDVI)))

# occ_sol <- readOGR('data/occ_sol_4326/occ_sol_4326.shp')
# occ_sol <- spTransform(samp, CRS(proj4string(NDVI)))


saveRDS(samp,"samp.rds")

testing<-readRDS("samp.rds")

testing@data$dummy<-0

testing@data<-testing@data %>% 
  mutate(dummy=ifelse(NATURE %in% c("Savane arborÃ©e ou arbustive","ForÃªt"),1,0))


testing@data$Code<-as.numeric(testing@data$id)

classes <- rasterize(testing, NDVI, field='Code')

classes[classes==4]<-3
classes[classes==5]<-4
classes[classes==6]<-5

r=stack(covs,Dummy)
#subset_vect<-c(8,2,3,4,5)
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

valuetable$class <- factor(valuetable$class, levels = c(1:5))

# ggplot(valuetable,aes(y=bsi,fill=as.factor(class)))+
#   geom_boxplot()

#pairs(r)

# graph<-function(df,xx,yy){
# 
#   df1<-df %>%
#     mutate(Classes=case_when(class=="1" ~ "Mangrove",
#                              class=="2" ~ "Water",
#                              class=="3" ~ "Tanne",
#                              class=="4" ~ "Soil/Built Up",
#                              class=="5" ~ "Other vegetation"))
# 
# 
#   ggplot(df1,aes_string(x=xx,y=yy,color="Classes"))+
#     geom_point()+theme_bw()+xlim(-2,2)
# }

# plt1<-graph(valuetable,"ndvi","ndwi")
# plt2<-graph(valuetable,"ndvi","bsi")
# plt3<-graph(valuetable,"bsi","ndwi")
# plt4<-graph(valuetable,"ndvi","avi")
# plt5<-graph(valuetable,"ndwi","avi")
# plt6<-graph(valuetable,"bsi","avi")
# 
# ggarrange(plt1,plt2,plt3,plt4,plt5,plt6,
#           nrow = 3,ncol = 2,common.legend=TRUE)

#### Convert Class to dummay variables
valuetable[,length(names(valuetable))] <- as.numeric(valuetable[,length(names(valuetable))]) -1
#subset_vect<-c(8,2,3,5,16)
point.df<- data.matrix(valuetable[,c(subset_vect,length(names(valuetable)))], rownames.force = NA)
grid.df <- data.matrix(valuetable[,subset_vect], rownames.force = NA)
#### Set  `dimnames` to `NULL`
dimnames(point.df) <- NULL
#dimnames(grid.df) <- NULL

#### Standardize_the data ((x-mean(x))/sd(x))
#point.df[, 1:3] = scale(point.df[, 1:3])
#grid.df[, 1:10] = scale(grid.df[, 1:10])

### Split data 
##  Determine sample size
ind <- sample(2, nrow(point.df), replace=TRUE, prob=c(0.70, 0.30))
# Split the `Split data
training <- point.df[ind==1, 1:length(subset_vect)]
test <- point.df[ind==2, 1:length(subset_vect)]
# Split the class attribute
trainingtarget <- as.numeric(point.df[ind==1,length(subset_vect)+1])
testtarget <- point.df[ind==2, length(subset_vect)+1]

#### Hyperparameter flag
FLAGS <- flags(
  flag_numeric('dropout_1', 0.2, 'First dropout'),
  flag_numeric('dropout_2', 0.2, 'Second dropout'),
  flag_numeric('dropout_3', 0.1, 'Third dropout'),
  flag_numeric('dropout_4', 0.1, 'Forth dropout')
)

### Define model parameters with 4 hidden layers with 200 neuron
model <- keras_model_sequential()
model %>% 
  # Imput layer
  layer_dense(units = 100, activation = 'relu', 
              kernel_regularizer =regularizer_l1_l2(l1 = 0.00001, l2 = 0.00001),input_shape = c(length(subset_vect))) %>% 
  layer_dropout(rate = FLAGS$dropout_1,seed = 1) %>% 
  # Hidden layers
  layer_dense(units =100, activation = 'relu',
              kernel_regularizer = regularizer_l1_l2(l1 = 0.00001, l2 = 0.00001)) %>%
  layer_dropout(rate = FLAGS$dropout_2,seed = 1) %>%
  layer_dense(units = 100, activation = 'relu',
              kernel_regularizer = regularizer_l1_l2(l1 = 0.00001, l2 = 0.00001)) %>%
  layer_dropout(rate = FLAGS$dropout_3,seed = 1) %>%
  layer_dense(units =100, activation = 'relu',
              kernel_regularizer = regularizer_l1_l2(l1 = 0.0001, l2 = 0.00001)) %>%
  layer_dropout(rate = FLAGS$dropout_4) %>%
  # Output layer
  layer_dense(units = 6, activation = 'softmax')
summary(model)

#### Define an optimizer (Stochastic gradient descent optimizer)
optimizer <- optimizer_sgd(lr=0.01, decay=1e-6, momentum=0.9)

#### Compile the model
model %>% compile(
  loss = 'sparse_categorical_crossentropy',
  optimizer = optimizer,
  metrics = 'accuracy'
)

####  Fit the model to the data 
history<-model %>% fit(
  training, trainingtarget, 
  epochs = 100, 
  batch_size = 100, 
  shuffle = TRUE,
  validation_split = 0.2,
  callbacks = callback_tensorboard()
)

### Plot history

#plot(history)

#### Evaluate the model
score <- model %>% evaluate(test, testtarget, batch_size = 100)
cat('Test loss:', score[[1]], '\n')
cat('Test accuracy:', score[[2]], '\n')

#### Prediction & confusion matrix - test data
class.test <- model %>%
  predict_classes(test, batch_size = 100)
table(testtarget,class.test)

#### Predicted Class Probability

prob.test <- model %>%
  predict_proba(test, batch_size = 100)

#### Prediction at grid data

Predict_func<-function(ras) {
  ras<- model %>%
    predict_classes(as.matrix(ras), batch_size = 100)
  ras <- setValues(NDVI, as.numeric(ras))
  return(ras)
}


#clear some space
#rm(list = c("BSI","MSAVI","data_ldst_8",
#            "NDWI","NDMI","NDBI","CSI","AVI"))
a=subset(r,subset_vect)

resultat<-Predict_func(a)

resultSN<-mask(resultat,polyg)
#pairs <- mapply(list, resultat, ldst_8_date, SIMPLIFY=F)
rsToDf2(resultSN)
#df8<-do.call("rbind",df8)


saloum<- readOGR('data/studied_area/studied_area.shp')
saloum <- spTransform(saloum, CRS(proj4string(resultat)))

raster_saloum<-crop(resultat,saloum)
raster_saloum<-mask(resultat,saloum)

rsToDf2(raster_saloum)
#after <- setValues(NDVI[[1]], as.numeric(after))

myPalette <- c("#228B22","blue","yellow", "#FF8C00","#00FF00")
class.name=c("Mangrove", "Eaux", "Tanne","sol","Other vegetations")

# for (i in 1:length(a)) {
#   levels(a[[i]])=data.frame(ID=0:4, code=class.name)
#   
# }
# 
# a=stack(a)
# rasterVis::levelplot(a,col.regions=myPalette,
#                      colorkey=list(space="bottom"))

# before<-resultat[[1]]
# after<-resultat[[2]]
# 
# formask <- setValues(NDVI[[1]], NA)
# ## Assign 1 to formask to all cells corresponding to the forest class
# formask[before==0 & after>0] <- -1
# formask[before==0 & after==0] <- 0
# formask[before>0 & after==0] <- 1
# #plot(formask,col=c("red","yellow"," dark green"))
# levels(formask)=data.frame(ID=-1:1, code=c("Loss","Stable","Gain"))
# 
# rasterVis::levelplot(formask,col.regions=c("red","yellow"," dark green"))
# 
writeRaster(resultat,"indices/mangrove_new2.tif",overwrite=TRUE)
q# writeRaster(before,"indices/before.tif",overwrite=TRUE)
# writeRaster(after,"indices/after.tif",overwrite=TRUE)
# 
# crosstab(before,after)*0.09

#writeRaster(,"Neureul_nectwork2013.tif",overwrite=TRUE)
#### Detach keras, tfruns, tftestimators

detach(package:keras, unload=TRUE)
detach(package:tfruns, unload=TRUE)
detach(package:tfestimators, unload=TRUE)

#### Convert to raster


#### Clean everyrhing

gc()


dd=function(x,y){ x$y}

tt<-function(df,y){
  df<-as.data.frame(df)
  df$V1<-gsub(" ","",df$V1)
  
  df<-df %>%filter(V1==y)
  as.character(df$V2[1])
}

readTML<-function(f){
  read.table(f,sep = "=",fill = TRUE,col.names = c("V1","V2"),stringsAsFactors = FALSE)
}



readLandsat<-function(folder) {
  list_file0<-paste0(folder,"/",
                    list.files(path = folder,pattern = '.TIF'))
  list_file<-as.list(gsub('TIF','tif',list_file0[c(1,4:9,11)]))
  list_file1<-as.list(gsub('TIF','tif',list_file0[2:3]))
  list_filePan<-as.list(gsub('TIF','tif',list_file0[10]))
  
  
  mtlFile<-paste0(folder,"/",
                  list.files(path = folder,pattern = 'MTL.txt'))
  metaData <- readMeta(mtlFile)
  
  amsa<-vector(mode = 'list',length = 8)
  gain<-metaData$CALRAD$gain
  offset<-metaData$CALRAD$offset
  elev<-metaData$SOLAR_PARAMETERS[2]
  d<-metaData$SOLAR_PARAMETERS[3]
  ####
  mtt<-readTML(mtlFile)
  rad<-mtt[98:119,]
  ref<-mtt[122:139,]
  qat<-mtt[142:163,]
  kdata<-mtt[208:211,]
  
  K1<-kdata[grep("K1_CONSTANT_BAND",kdata$V1),c("V2")]
  K1<-as.numeric(K1)
  K2<-kdata[grep("K2_CONSTANT_BAND",kdata$V1),c("V2")]
  K2<-as.numeric(K2)
  
  
  radMax<-rad[grep("RADIANCE_MAXIMUM_BAND",rad$V1),]
  RefMax<-ref[grep("REFLECTANCE_MAXIMUM_BAND",ref$V1),]
  DNmin<-qat[grep("QUANTIZE_CAL_MIN_BAND",qat$V1),]
  
  ESUN<-(pi*d*d)*as.numeric(radMax$V2[1:8])/as.numeric(RefMax$V2[1:8])
  
  Lp<-metaData$CALRAD$gain[1:8]*as.numeric(DNmin$V2[1:8])+
    metaData$CALRAD$offset[1:8]-0.01*ESUN*sin(elev*pi/180)/(pi*d*d)
  
  Pan<-raster(list_filePan[[1]])
  
  
  Resample_list<-function(j){
    
    if(j<=9){
      
      if(j==8){
        
        res<-raster(list_filePan[[1]])
        
      }else if (j==9){
        res<-raster(list_file[[j-1]])
        res<-raster::resample(res, Pan, method='bilinear')
      }else{
        res<-raster(list_file[[j]])
        res<-raster::resample(res, Pan, method='bilinear')
      }
      
    }else {
      
      k=j-9
      res<-raster(list_file1[[k]])
    
      res<-raster::resample(res, Pan, method='bilinear')
      
    }
    
    return(res)
    
  }
  
  amsa<-lapply(1:11, Resample_list)
  
  
  I<-(0.42*amsa[[2]]+0.98*amsa[[3]]+0.6*amsa[[4]])/2
  M<-amsa[[8]]
  
  # cl <- makeCluster(4)
  # clusterEvalQ(cl, library(raster))
  # clusterExport(cl, c('amsa','M'))
  amsa_r<-lapply(amsa,function(S){ S*(M/I)})
  # stopCluster(cl)
  
  amsa_r[[8]]<-amsa[[8]]
  
  rm(I)
  rm(M)
  rm(amsa)
  #for (j in 1:11) {
  
  convertir<-function(j){
    res<-amsa_r[[j]]
    
    if(j<=9){
      
      res[res==0]<-NA
      #ref<-res
      #res<-getValues(res)
      #Radiance at the Sensors Aperture
      res<-pi*(gain[j]*res+offset[j]-Lp[j])*d*d/(ESUN[j]*sin(elev*pi/180))

    }else {
      
      k=j-9
      
      res[res==0]<-NA
      res<-gain[j]*res+offset[j]
      
      res<-K2[k]/(log(K1[k]/res)+1)
      
    }
    
    return(res)
   
  }
  
  #resample
# library(parallel)
#   cl <- makeCluster(4)
#   clusterEvalQ(cl, library(raster))
#   clusterExport(cl, c('list_file1','Pan','list_file','list_filePan','ESUN','gain','offset','Lp','elev','d'))


  amsa<-lapply(1:8,convertir)
  rm(amsa_r)
  #stopCluster(cl)

  forcrs<-raster(list_file[[1]])
  polyg <- spTransform(polyg, CRS(proj4string(forcrs)))
  stackedRaster<-crop(stack(amsa), polyg)
  #stackedRaster<-mask(stackedRaster,polyg)
  
  return(stackedRaster)
}

stand<-function(r){
  
  (r-cellStats(r,min))/(cellStats(r,max)-cellStats(r,min))
}

NDVI_func <- function(img,satellite) {
  
  if (satellite=="LANDSAT_8") {
    bk <- img[[5]]
    bi <- img[[4]]
  }else{
    bk <- img[[4]]
    bi <- img[[3]]
  }
  
  vi <- (bk - bi) / (bk + bi)
  #vi=normImage(vi)
  #vi<-stand(vi)
  return(vi)
}

MSAVI_func <- function(img,satellite) {
  
  if (satellite=="LANDSAT_8") {
    bk <- img[[5]]
    bi <- img[[4]]
  }else{
    bk <- img[[4]]
    bi <- img[[3]]
  }
  
  vi <- (2*bk+1-sqrt((2*bk+1)^2-8*(bk-bi)))/2
  #vi=normImage(vi)
  return(vi)
}




NDWI_func <- function(img,satellite) {
  
  if (satellite=="LANDSAT_8") {
    bk <- img[[3]]
    bi <- img[[6]]
  }else{
    bk <- img[[2]]
    bi <- img[[5]]
  }
  
  vi <- (bk - bi) / (bk + bi)
  
  #vi<-normImage(vi)
  #vi<-stand(vi)
  return(vi)
}


BSI_func <- function(img,satellite) {
  
  if(satellite=="LANDSAT_8"){
    bk <- img[[6]]
    bi <- img[[5]]
    bj <- img[[4]]
    bl <- img[[2]]
  }else{
    bk <- img[[5]]
    bi <- img[[4]]
    bj <- img[[3]]
    bl <- img[[1]]
  }
  bii <- ((bk+bj)-(bi+bl))/((bk+bj)+(bi+bl))*100+100
  #bii<-normImage(bii)
  #bii<-stand(bii)
  return(bii)
}



rsToDf<-function(pair=a){
  r=pair[[1]]
  year=unlist(pair[[2]])
  df<-data.frame(x=raster::getValues(r))
  df<-df %>% mutate(classe=case_when(x==0 ~ "Mangrove",
                                      x==1 ~ "Eaux",
                                      x==2 ~ "Tanne",
                                      x==3 ~ "Sol",
                                      x==4 ~ "Autres vegetation")) %>%
    dplyr::group_by(classe) %>% dplyr::summarise(area=n()*0.09) %>% 
    mutate(Year=year)
  return(as.data.frame(df))
}

# pairs <- mapply(list, result8, ldst_8_date, SIMPLIFY=F)
# # #pairs1 <- mapply(list, result7, 2007:2013, SIMPLIFY=F)
# # 
# df8<-lapply(pairs, rsToDf)
# # #df7<-lapply(pairs1, rsToDf)
# # 
#  df8<-do.call("rbind",df8)
# # #df7<-do.call("rbind",df7)
# # 
# # #df_final<-df8 %>% bind_rows(df7 %>% filter(Year!=2013)) %>% arrange(Year) %>% filter(!is.na(classe))
# df8<-df8 %>% mutate(Year=as.Date(Year)) %>% filter(!is.na(classe))
# ggplot(df8 %>% filter(classe %in% c("Mangrove")))+geom_line(aes(x=Year,y=area, color=as.factor(classe)),size=1)+ geom_point(aes(x=Year,y=area, color=as.factor(classe)),shape=21,                            fill= "White",size =3,                      stroke=1.5)+theme_bw()
# # 

SAVI_func <- function(img,satellite) {
  
  if (satellite=="LANDSAT_8") {
    bk <- img[[5]]
    bi <- img[[4]]
  }else{
    bk <- img[[4]]
    bi <- img[[3]]
  }
  
  vi <- ((bk - bi) / (bk + bi+0.5))*1.5
  #vi=normImage(vi)
  return(vi)
}


NDBI_func <- function(img,satellite) {
  
  if (satellite=="LANDSAT_8") {
    bk <- img[[6]]
    bi <- img[[5]]
  }else{
    bk <- img[[5]]
    bi <- img[[4]]
  }
  
  vi <- (bk - bi) / (bk + bi)
  
  #vi<-normImage(vi)
  return(vi)
}




PCA_raster<-function(x,y) {
  pcaRaster<-stack(x,y)
  set.seed(1)
  sr <- sampleRandom(pcaRaster, 10000)
  pca <- prcomp(sr, scale = TRUE)
  VD<- predict(pcaRaster, pca, index = 1:2)[[1]]+predict(pcaRaster, pca, index = 1:2)[[2]]
  #VD<-normImage(VD)
  return(VD)
  
}


NDMI_func <- function(img,satellite) {
  
  if (satellite=="LANDSAT_8") {
    bk <- img[[5]]
    bi <- img[[6]]
  }else{
    bk <- img[[4]]
    bi <- img[[5]]
  }
  
  vi <- (bk - bi) / (bk + bi)
  
  #vi<-normImage(vi)
  #vi<-stand(vi)
  return(vi)
}


rm_outlier<-function(df,var){
  
  df1<-as.data.table(df)
  quant<-function(x,type) {
    Q1<-quantile(x,0.25,na.rm=TRUE)
    Q3<-quantile(x,0.75,na.rm=TRUE)
    LB<-Q1-1.5*(Q3-Q1)
    UB<-Q3+1.5*(Q3-Q1)
    r=ifelse(type=="L",LB,UB)
    return(r)
  }
  
  df1[,L:=quant(get(var),"L"),by=c("class")]
  df1[,U:=quant(get(var),"U"),by=c("class")]
  df1<-df1[get(var)>=L & get(var)<=U,]
  df1[,L:=NULL]
  df1[,U:=NULL]
  return(as.data.frame(df1))
  
}



CSI_func <- function(img,DNmax=1) {
  bk <- img[[4]]
  bi <- img[[3]]
  bl <- img[[2]]
  ssi <- ((DNmax-bl)*(DNmax-bi)*(DNmax-bk))^(1/3)
  return(ssi)
}


AVI_func <- function(img,DNmax=1) {
  bk <- img[[5]]
  bi <- img[[4]]
  
  avi <- ((bk)*(DNmax-bi)*(bk-bi)+1)^(1/3)
  
  avi[is.na(avi) & !is.na(bi)]<-0
  
  
  return(avi)
}


NDWI3_func <- function(img,satellite) {
  
  if (satellite=="LANDSAT_8") {
    bk <- img[[3]]
    bi <- img[[6]]
  }else{
    bk <- img[[2]]
    bi <- img[[5]]
  }
  
  vi <- (bk - bi)^3 / (bk + bi)^3
  
  #vi<-normImage(vi)
  #vi<-stand(vi)
  return(vi)
}

AMSA_func <- function(img,satellite) {
  
  if (satellite=="LANDSAT_8") {
    bk <- img[[7]]
    bi <- img[[6]]
  }else{
    bk <- img[[2]]
    bi <- img[[5]]
  }
  
  vi <- (bk - bi)/ (bk + bi)
  
  #vi<-normImage(vi)
  #vi<-stand(vi)
  return(vi)
}


ADD_func <- function(img,DNmax=1) {
  
  avi <- img[[7]]+img[[3]]-img[[6]]-img[[5]]-img[[4]]
  
  return(avi)
}

rsToDf2<-function(r){
  # r=pair[[1]]
  # year=unlist(pair[[2]])
  df<-data.frame(x=raster::getValues(r))
  df<-df %>% mutate(classe=case_when(x==1 ~ "Mangrove",
                                     x==2 ~ "Eaux",
                                     x==3 ~ "Tanne Claire",
                                     x==4 ~ "Tanne fonce",
                                     x==5 ~ "Sol",
                                     x==6 ~ "Autres vegetation")) %>%
    dplyr::group_by(classe) %>% dplyr::summarise(area=n()*0.000225) 
  return(as.data.frame(df))
}


SATVI_func <- function(img) {
 
  
  satvi <- (img[[5]]-img[[3]])/(img[[5]]+img[[3]]+0.5)*1.5-img[[7]]/2

  
  return(satvi)
}



graph2<-function(df,xx,yy){
  
  df1<-df %>%
    mutate(Classes=case_when(class=="1" ~ "Mangrove",
                             class=="2" ~ "Water",
                             class=="3" ~ "Tanne claire",
                             class=="4" ~ "Tanne foncée",
                             class=="5" ~ "Soil/Built Up",
                             class=="6" ~ "Other vegetation"))
  
  
  ggplot(df1,aes_string(x=xx,y=yy,color="Classes"))+
    geom_point()+theme_bw()+xlim(-2,2)
}


# Nitrogen Reflectance Index

NRI_func <- function(img) {
  
  avi <- (img[[3]]-img[[4]])/(img[[3]]+img[[4]])
  

  return(avi)
}

MVI_func <- function(img) {
  
  avi <- (1+img[[5]]-img[[3]])/(1+img[[6]]-img[[3]])
  
  return(avi)
}


DIFF_func <- function(img) {
  
  ndvi <- (img[[5]]-img[[4]])/(img[[5]]+img[[4]])
  ndwi<-(img[[3]]-img[[6]])/(img[[3]]+img[[6]])
  
  avi<-ndvi-ndwi
  return(avi)
}

# MI = (NIR − SWIR/NIR × SWIR) × 10000


MI_func <- function(img) {
  
  avi <- (img[[5]]-img[[6]]/img[[5]]*img[[6]])*10
  
  return(avi)
}














# Plant Senescence Reflectance Index

PSRI_func <- function(img) {
  
  avi <- (img[[4]]-img[[2]])/img[[5]]
  return(avi)
}


# Brightness index
BI_func <- function(img) {
  
  avi <- (img[[5]]+img[[4]]+img[[3]])/sqrt(3)
  return(avi)
}



# Renormalized diference vegetation index

RDVI_func <- function(img) {
  
  avi <- (img[[5]]-img[[1]])/sqrt(img[[5]]+img[[1]])
  return(avi)
}

#The Normalized Difference Soil Moisture Index, NDSMI

NDSMI_func <- function(img) {
  
  avi <- (img[[6]]-img[[7]])/img[[6]]+img[[7]]
  return(avi)
}

#The Normalized Difference Soil Moisture Index, NDSMI

NDII_func <- function(img) {
  
  avi <- (img[[2]]-img[[6]])/img[[2]]+img[[6]]
  return(avi)
}

#Salinity index - SI1
SI1_func <- function(img) {
  
  avi <- sqrt(img[[3]]^2+img[[4]]^2)
  return(avi)
}

#Salinity index - SI2
SI2_func <- function(img) {
  
  avi <- sqrt(img[[3]]*img[[4]])
  return(avi)
}


#Salinity index - SI3
SI3_func <- function(img) {
  
  avi <- img[[2]]*img[[4]]
  return(avi)
}

#Salinity index - SI4
SI4_func <- function(img) {
  
  avi <- (img[[4]]*img[[5]])/img[[3]]
  return(avi)
}


#Salinity index - SI5
SI5_func <- function(img) {
  
  avi <- img[[2]]/img[[4]]
  return(avi)
}

#Vegetation Soil Salinity Index   - VSSI
VSSI_func <- function(img) {
  
  avi <- 2*img[[3]]-5*(img[[4]]+img[[5]])
  return(avi)
}


#Moisture Stress Index
MSI_func <- function(img) {
  
  avi <- img[[7]]/img[[5]]
  return(avi)
}


#Visible and Shortwave infrared Drought Index
VSIDI_func <- function(img) {
  
  avi <- 1-((img[[7]]-img[[2]])+(img[[4]]-img[[2]]))
  return(avi)
}


# Simple ratio pigment index

SRPI_func <- function(img) {
  
  avi <- img[[1]]/img[[5]]
  return(avi)
}


Ratio_func <- function(img,k,i) {
  
  bk <- img[[k]]
  bi <- img[[i]]
  
  avi <- img[[k]]/img[[i]]
  return(avi)
}


CMtoDf<-function(tab,methode){
res<-c(diag(tab)/colSums(tab),sum(diag(tab))/sum(colSums(tab)))

nam<- c("Mangroves", "Water", "Mudflats/Salt flats","Bare soil/Built up/agriculture","Other vegetations","Overall Accuracy")
df<-data.frame(cbind(class=nam,Method=methode,CF=res))
return(df)

}





readLandsat2<-function(folder) {
  list_file0<-paste0(folder,"/",
                     list.files(path = folder,pattern = '.TIF'))
  list_file<-as.list(gsub('TIF','tif',list_file0[c(1,4:9,11)]))
  list_file1<-as.list(gsub('TIF','tif',list_file0[2:3]))
  list_filePan<-as.list(gsub('TIF','tif',list_file0[10]))
  
  
  mtlFile<-paste0(folder,"/",
                  list.files(path = folder,pattern = 'MTL.txt'))
  metaData <- readMeta(mtlFile)
  
  amsa<-vector(mode = 'list',length = 8)
  gain<-metaData$CALRAD$gain
  offset<-metaData$CALRAD$offset
  elev<-metaData$SOLAR_PARAMETERS[2]
  d<-metaData$SOLAR_PARAMETERS[3]
  ####
  mtt<-readTML(mtlFile)
  rad<-mtt[98:119,]
  ref<-mtt[122:139,]
  qat<-mtt[142:163,]
  kdata<-mtt[208:211,]
  
  K1<-kdata[grep("K1_CONSTANT_BAND",kdata$V1),c("V2")]
  K1<-as.numeric(K1)
  K2<-kdata[grep("K2_CONSTANT_BAND",kdata$V1),c("V2")]
  K2<-as.numeric(K2)
  
  
  radMax<-rad[grep("RADIANCE_MAXIMUM_BAND",rad$V1),]
  RefMax<-ref[grep("REFLECTANCE_MAXIMUM_BAND",ref$V1),]
  DNmin<-qat[grep("QUANTIZE_CAL_MIN_BAND",qat$V1),]
  
  ESUN<-(pi*d*d)*as.numeric(radMax$V2[1:8])/as.numeric(RefMax$V2[1:8])
  
  Lp<-metaData$CALRAD$gain[1:8]*as.numeric(DNmin$V2[1:8])+
    metaData$CALRAD$offset[1:8]-0.01*ESUN*sin(elev*pi/180)/(pi*d*d)
  
  Pan<-raster(list_filePan[[1]])
  
  polyg <- spTransform(polyg, CRS(proj4string(Pan)))
  Pan<-crop(Pan, polyg)
  
  
  Resample_list<-function(j){
    
    if(j<=9){
      
      if(j==8){
        
        res<-raster(list_filePan[[1]])
        res<-crop(res, polyg)
        
      }else if (j==9){
        res<-raster(list_file[[j-1]])
        res<-crop(res, polyg)
        
        res<-raster::resample(res, Pan, method='bilinear')
      }else{
        res<-raster(list_file[[j]])
        res<-crop(res, polyg)
        
        res<-raster::resample(res, Pan, method='bilinear')
      }
      
    }else {
      
      k=j-9
      res<-raster(list_file1[[k]])
      res<-crop(res, polyg)
      
      
      res<-raster::resample(res, Pan, method='bilinear')
      
    }
    
    return(res)
    
  }
  
  amsa<-lapply(1:11, Resample_list)
  
  
  I<-(0.42*amsa[[2]]+0.98*amsa[[3]]+0.6*amsa[[4]])/2
  M<-amsa[[8]]
  
  # cl <- makeCluster(4)
  # clusterEvalQ(cl, library(raster))
  # clusterExport(cl, c('amsa','M'))
  amsa_r<-lapply(amsa,function(S){ S*(M/I)})
  # stopCluster(cl)
  
  amsa_r[[8]]<-amsa[[8]]
  
  rm(I)
  rm(M)
  rm(amsa)
  #for (j in 1:11) {
  
  convertir<-function(j){
    res<-amsa_r[[j]]
    
    if(j<=9){
      
      res[res==0]<-NA
      #ref<-res
      #res<-getValues(res)
      #Radiance at the Sensors Aperture
      res<-pi*(gain[j]*res+offset[j]-Lp[j])*d*d/(ESUN[j]*sin(elev*pi/180))
      
    }else {
      
      k=j-9
      
      res[res==0]<-NA
      res<-gain[j]*res+offset[j]
      
      res<-K2[k]/(log(K1[k]/res+1))
      
    }
    
    return(res)
    
  }
  
  #resample
  # library(parallel)
  #   cl <- makeCluster(4)
  #   clusterEvalQ(cl, library(raster))
  #   clusterExport(cl, c('list_file1','Pan','list_file','list_filePan','ESUN','gain','offset','Lp','elev','d'))
  
  
  amsa<-lapply(1:11,convertir)
  rm(amsa_r)
  #stopCluster(cl)
  # 
  # forcrs<-raster(list_file[[1]])
  # polyg <- spTransform(polyg, CRS(proj4string(forcrs)))
  # stackedRaster<-crop(stack(amsa), polyg)
  #stackedRaster<-mask(stackedRaster,polyg)
  
  return(amsa)
}


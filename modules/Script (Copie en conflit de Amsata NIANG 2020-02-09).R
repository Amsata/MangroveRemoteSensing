
dir.create('data', showWarnings = FALSE)
if (!file.exists('data/rs/samples.rds')) {
  download.file('https://biogeo.ucdavis.edu/data/rspatial/rsdata.zip', 
                dest = 'data/rsdata.zip')
  unzip('data/rsdata.zip', exdir='data')
}

library(rgdal)
library(raster)

#Bleue
b2<-raster('data/rs/LC08_044034_20170614_B2.tif')
b3 <- raster('data/rs/LC08_044034_20170614_B3.tif')

b4 <- raster('data/rs/LC08_044034_20170614_B4.tif')

b5 <- raster('data/rs/LC08_044034_20170614_B5.tif')


# Systeme de coordonne de reference

crs(b2)

#e cellule

ncell(b2)

dim(b2)
res(b2)
nlayers(b2)


# Do the bands have the same extent, number of rows and columns, projection,
# ˓→resolution, and origin
compareRaster(b2,b3)

s<-stack(b5,b4,b3)
s

filenames <- paste0('data/rs/LC08_044034_20170614_B', 1:11, ".tif")
filenames

landsat <- stack(filenames)
landsat

par(mfrow = c(2,2))
plot(b2, main = "Blue", col = gray(0:100 / 100))
plot(b3, main = "Green", col = gray(0:100 / 100))
plot(b4, main = "Red", col = gray(0:100 / 100))
plot(b5, main = "NIR", col = gray(0:100 / 100))

par(mfrow = c(1,1))
landsatRGB <- stack(b4, b3, b2)
plotRGB(landsatRGB, axes = TRUE, stretch = "lin", main = "Landsat True Color Composite")

par(mfrow = c(1,2))
plotRGB(landsatRGB, axes=TRUE, stretch="lin", main="Landsat True Color Composite")
landsatFCC <- stack(b5, b4, b3)
plotRGB(landsatFCC, axes=TRUE, stretch="lin", main="Landsat False Color Composite")


#Principal component analysis

set.seed(1)

sr<-sampleRandom(landsat,10000)

plot(sr[,c(4,5)], main = "NIR-Red plot")

pca<-prcomp(sr,scale. = TRUE)
pca
screeplot(pca)

pci<-predict(landsat,pca,index=1:2)

plot(pci[[1]])

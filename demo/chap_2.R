# setup and demo processes

#install packages
ls <- c("reshape", "Hmisc", "rgdal", "raster", "sf", "GSIF", "plotKML", "nnet", "plyr", "ROCR", "randomForest", "quantregForest",
        "psych", "mda", "h2o", "h2oEnsemble", "dismo", "grDevices","snowfall", "hexbin", "lattice", "ranger", "e1071", "ggplot2",
        "soiltexture", "aqp", "colorspace", "Cubist","randomForestSRC", "ggRandomForests", "scales", "gstat", "devtools",
        "xgboost", "parallel", "doParallel", "caret","gam", "glmnet", "matrixStats", "SuperLearner",
        "quantregForest", "intamap", "fasterize", "viridis", "sp","terra")
new.packages <- ls[!(ls %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# use devtools to install packages and dependencies that cannot be resolved by CRAN
library(devtools)
install.packages("gdalUtilities")
install_github("gearslaboratory/gdalUtils")
install.packages("maptools", repos="http://R-Forge.R-project.org")
install_github("envirometrix/landmap")
install.packages("plotKML", repos=c("http://R-Forge.R-project.org"))

# include answer by Landak to downloaded package : https://stackoverflow.com/questions/73559636/unable-to-install-gsif-package-in-r
devtools::install("C:/.../Downloads/GSIF_0.5-5.1/GSIF") # download from https://cran.r-project.org/src/contrib/Archive/GSIF/

#test creation of soil maps and viz on Google Earth
library(GSIF)
library(sp)
library(boot)
library(aqp)
library(plyr)
library(rpart)
library(splines)
library(gstat)
library(quantregForest)
library(plotKML)

setwd("D:/r/KenyaDSM/demo") # set working directory to demo folder

# demo for modelling soil organic matter in quantile regression forest model
demo(meuse, echo=FALSE)
omm <- fit.gstatModel(meuse, om~dist+ffreq, meuse.grid, method="quantregForest")
om.rk <- predict(omm, meuse.grid)
plot(om.rk)
plotKML(om.rk) # plot on Google Earth

# demo of OK predictions
library(sp)
library(gstat)
data(meuse)
coordinates(meuse) = ~x+y
data(meuse.grid)
gridded(meuse.grid) = ~x+y
m <- vgm(.59, "Sph", 874, .04)
# ordinary kriging:
x <- krige(log(zinc)~1, meuse, meuse.grid, model = m)
spplot(x["var1.pred"], main = "ordinary kriging predictions")


# setup SAGA GIS
if(Sys.info()['sysname']=="Windows"){
  saga_cmd = "D:/saga-9.3.0_x64/saga_cmd.exe"
} else {
  saga_cmd = "saga_cmd"
}

# send R processes to SAGA GIS
library(GSIF)
library(plotKML)
library(rgdal)
library(raster)

# load grid in the plotKML package
data("eberg_grid")
gridded(eberg_grid) <- ~x+y
proj4string(eberg_grid) <- CRS("+init=epsg:31467")

# write grid to GeoTiff
writeGDAL(eberg_grid["DEMSRT6"], "DEMSRT6.sdat", "SAGA")
system(paste(saga_cmd, 'ta_lighting 0 -ELEVATION "DEMSRT6.sgrd" -SHADE "hillshade.sgrd" -EXAGGERATION 2'))
plot(raster("hillshade.sdat"), col=SAGA_pal[[3]])

# R to GDAL
if(.Platform$OS.type == "windows"){
  gdal.dir <- shortPathName("C:/Program Files/GDAL")
  gdal_translate <- paste0(gdal.dir, "/gdal_translate.exe")
  gdalwarp <- paste0(gdal.dir, "/gdalwarp.exe")
} else {
  gdal_translate = "gdal_translate"
  gdalwarp = "gdalwarp"
}
# test functionality
system(paste(gdalwarp, "--help"))
# reproject grid with GDAL
system(paste(gdalwarp, ' DEMSRT6.sdat DEMSRT6_ll.tif -t_srs \"+proj=longlat +datum=WGS84\"'))
library(raster)
plot(raster("DEMSRT6_ll.tif"))
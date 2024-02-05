#set project directory
proj_dir = 'D:/r/KenyaDSM'

#select food data
install.packages('tidyverse', 'fs', 'readxl')
library(tidyverse)
library(fs)
library(readxl)
library(dplyr)

#import raw data
setwd(paste(proj_dir,'data', sep="/"))
raw_foods <- as.data.frame(read_excel("food_composition_tables.xlsx", 4))

# clean data
raw_foods <- raw_foods[307:470, c(2, 12,14,16,17)] # select vegetables and nutrients
colnames(raw_foods) <- c('Vegetable', 'Calcium', 'Magnesium', 'Potassium', 'Sodium')

duplicated(raw_foods$Vegetable) #check for duplicates
is.null(raw_foods) #check for nulls
str(raw_foods) #check data types

raw_foods <- raw_foods[!(is.na(raw_foods$Vegetable) | raw_foods$Vegetable==""), ] # remove null foods
raw_foods <- raw_foods[!duplicated(raw_foods$Vegetable), ] # remove duplicate foods
raw_foods <- raw_foods %>% mutate_at(c('Calcium', 'Magnesium', 'Potassium', 'Sodium'), as.numeric) # convert nutrients to num
raw_foods <- raw_foods %>% mutate(across(where(is.numeric), round, 3)) # round off to 3.dp
write.csv(raw_foods, "veg_nutrients.csv", row.names=FALSE) # export csv

# descriptive stats of veg nutrients
summary(raw_foods)
boxplot(raw_foods[,2:5])
# select potassium because of uniform distribution and reject sodium 
boxplot(raw_foods$Potassium)
# compare Mg and Ca since they are similar
hist(raw_foods$Magnesium)
hist(raw_foods$Calcium)
# select Magnesium for slightly more varied distribution between Mg and Ca

#export veg, potassium and magnesium data
write.csv(raw_foods[,c(1,3,4)], "veg_k_mg.csv", row.names=FALSE) # export csv

#prepare soil data
#read TIFF files
library(raster)
library(terra)
library(sf)

af_k <- raster("af250m_nutrient_k_m_agg30cm.tif")
af_mg <- raster("af250m_nutrient_mg_m_agg30cm.tif")

# bind KE based on coordinates
ken<- st_read("./ke_shp", "ken_admbnda_adm1_iebc_20191031")
ke_crs <- "+proj=longlat +datum=WGS84 +no_defs" # set crs
ke_epsg <- 32737 # "+init=EPSG:32737" # world espg: 4326 (recommended for covariates)
ken_k <- crop(af_k, ken)
ken_mg <- crop(af_mg,ken)
writeRaster(ken_k, 'ken_k.tif') # export .tif files to relieve memory usage
writeRaster(ken_mg, 'ken_mg.tif')

# derive data points from raster layers
ken_k <- raster("kenya_k.tif")
ken_mg <- raster("kenya_mg.tif")
ken_k_spdf <- extract(ken_k, SpatialPoints(ken_k),sp=T)
ken_mg_spdf <- extract(ken_mg, SpatialPoints(ken_mg), sp=T)
ken_k_mg_df <- cbind(ken_k_spdf@coords, ken_k_spdf@data, ken_mg_spdf@data)

# prepare full soil dataset
colnames(ken_k_mg_df) <- c('long', 'lat', 'k', 'mg')
summary(ken_k_mg_df)
ken_k_mg_df <- na.omit(ken_k_mg_df) # remove null values
write.csv(ken_k_mg_df, "soil_k_mg.csv", row.names=FALSE) # export csv

## SOIL DATA PREPARATION DONE IN QGIS ##

# covariate preparation and selection
library(raster)
library(terra)
library(sf)
library(corrplot)
setwd(paste(proj_dir,'data/covariates_shp', sep="/"))
# depth - 15-30 cm, 250m resolution
covs_path <- getwd()

# create raster stack
epsg <- "+init=epsg:4326"
files <- list.files(path = covs_path, pattern = "tif*$", full.names = TRUE)
covs <- stack(files)

# transform varying rasters in case of varying rasters error
ph20 <- raster(paste(covs_path, "ph_in_h20_30cm_depth.tif", sep="/"))
occ <- raster(paste(covs_path, "org_c_con_30cm_depth.tif", sep="/"))
ph20 <- projectRaster(from = ph20, to = covs$annual_precip, method="ngb")
occ <- projectRaster(from = occ, to = covs$annual_precip, method="ngb")
writeRaster(ph20, 'ph_in_h20_30cm_depth.tif', overwrite=TRUE)
writeRaster(occ, 'org_c_con_30cm_depth.tif', overwrite=TRUE)
remove(ph20)
remove(occ)

#retry co-variate stacking
files <- list.files(path = covs_path, pattern = "tif*$", full.names = TRUE)
covs <- stack(files)
# confirm selection
names(covs)
plot(covs$sand)

# load soil training data for appending: convert to SPDF
k_dat <- read.csv(paste(proj_dir,'data/soil_shp/k_train/k_train.csv', sep="/"))
mg_dat <- read.csv(paste(proj_dir,'data/soil_shp/mg_train/mg_train.csv', sep="/"))
k_dat <- k_dat[, -4]
mg_dat <- mg_dat[, -4]
coordinates(k_dat) <- ~ X + Y
coordinates(mg_dat) <- ~ X + Y
proj4string(k_dat) <- CRS(epsg)
proj4string(mg_dat) <- CRS(epsg)
# confirm that points lie within covariates
plot(covs$occ)
points(mg_dat)

#  extract values from covariates
k_covs <- extract(x = covs, y = k_dat, sp=TRUE)
mg_covs <- extract(x = covs, y = mg_dat, sp=TRUE)
summary(k_covs)
summary(mg_covs)

# do correlation tests
k_covs <- as.data.frame(k_covs) 
mg_covs <- as.data.frame(mg_covs) 
k_covs <- k_covs[complete.cases(k_covs),] # remove nulls
mg_covs <- mg_covs[complete.cases(mg_covs),]
k_cor <- corrplot(round(cor(k_covs[,3:18]),2),method = "color")
mg_cor <- corrplot(round(cor(mg_covs[,3:18]),2),method = "color")

# select suitable covariates
# set to absolute values to capture both +ve and -ve correlations
k_cor <- abs(round(cor(x = as.matrix(k_covs[,3]), y = as.matrix(k_covs[,c(4:18)])),2))
mg_cor <- abs(round(cor(x = as.matrix(mg_covs[,3]), y = as.matrix(mg_covs[,c(4:18)])),2))
row.names(k_cor) <- c('corr')
row.names(mg_cor) <- c('corr')
k_cor <- as.data.frame(k_cor[,k_cor["corr",] >= 0.3])
mg_cor <- as.data.frame(mg_cor[,mg_cor["corr",] >= 0.3])

# create and export training sets with selected covariates
k_list <- row.names(k_cor) %>% c('X', 'Y', 'k') 
mg_list <- row.names(mg_cor) %>% c('X', 'Y', 'mg')
k_train <- subset(k_covs, select=k_list)
mg_train <- subset(mg_covs, select=mg_list)
write.csv(k_train, "k_train_cov.csv", row.names=FALSE)
write.csv(mg_train, "mg_train_cov.csv", row.names=FALSE)
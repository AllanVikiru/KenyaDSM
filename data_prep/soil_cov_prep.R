#set project directory
proj_dir <- 'D:/r/KenyaDSM'

# covariate preparation and selection
library(raster)
library(terra)
library(sf)
library(corrplot)
library(psych)
setwd(paste(proj_dir,'data/covariates_shp/train_cov', sep="/"))
# depth - 15-30 cm, 250m resolution
covs_path <- getwd()

# create raster stack
epsg <- "epsg:4326"
files <- list.files(path = covs_path, pattern = "tif*$", full.names = TRUE)
covs <- stack(files)
summary(covs)

# clean rasters
ph20 <- raster(paste(covs_path, "ph20.tif", sep="/"))
occ <- raster(paste(covs_path, "occ.tif", sep="/"))
# transform varying rasters in case of varying rasters error
ph20 <- projectRaster(from = ph20, to = covs$annual_precip, method="ngb")
occ <- projectRaster(from = occ, to = covs$annual_precip, method="ngb")
# replace NA with 0s
ph20[is.na(ph20[])]<- 0
occ[is.na(occ[])]<- 0
#export cleaned rasters
plot(ph20)
summary(occ)
writeRaster(ph20, 'ph20.tif', overwrite=TRUE)
writeRaster(occ, 'occ.tif', overwrite=TRUE)
remove(ph20)
remove(occ)

#retry co-variate stacking
files <- list.files(path = covs_path, pattern = "tif*$", full.names = TRUE)
covs <- stack(files)
# confirm selection
summary(covs)
plot(covs$occ)

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
points(mg_dat)

#  extract values from covariates
k_covs <- extract(x = covs, y = k_dat, sp=TRUE)
mg_covs <- extract(x = covs, y = mg_dat, sp=TRUE)
k_covs <- as.data.frame(k_covs) 
mg_covs <- as.data.frame(mg_covs) 
summary(k_covs)
summary(mg_covs)
k_covs <- k_covs[complete.cases(k_covs),] # remove nulls
mg_covs <- mg_covs[complete.cases(mg_covs),]

#export cleaned covs data
write.csv(k_covs, "k_full_covs.csv", row.names=FALSE)
write.csv(k_covs, "mg_full_covs.csv", row.names=FALSE)

# do correlation tests
skew(k_covs[3:18], na.rm=TRUE) # check skewness to see variation from normal distribution
skew(mg_covs[3:18], na.rm=TRUE)

# large variations for most covs so apply spearman to select
corrplot(cor(k_covs[3:18], method="spearman"), method="color") # do correlation plots
corrplot(cor(mg_covs[3:18], method="spearman"), method="color")

# as matrix format for selection
k_cor <- abs(round(cor(x = as.matrix(k_covs[,3]), y = as.matrix(k_covs[,c(4:18)], method="spearman")),2)) # set to absolute values to capture both +ve and -ve correlations
mg_cor <- abs(round(cor(x = as.matrix(mg_covs[,3]), y = as.matrix(mg_covs[,c(4:18)], method="spearman")),2))
row.names(k_cor) <- c('corr')
row.names(mg_cor) <- c('corr')
k_cor <- as.data.frame(k_cor[,k_cor["corr",] >= 0.2]) # select weak to strong correlations: check Mukaka, 2012
mg_cor <- as.data.frame(mg_cor[,mg_cor["corr",] >= 0.7]) # select strong correlations

# create and export training sets with selected covariates
k_list <- row.names(k_cor) %>% c('X', 'Y', 'k') 
mg_list <- row.names(mg_cor) %>% c('X', 'Y', 'mg')
k_train <- subset(k_covs, select=k_list)
mg_train <- subset(mg_covs, select=mg_list)

setwd(paste(proj_dir,'data/soil_shp/', sep="/"))
write.csv(k_train, "k_train/k_train_cov.csv", row.names=FALSE)
write.csv(mg_train, "mg_train/mg_train_cov.csv", row.names=FALSE)
#set project directory
proj_dir <- 'D:/r/KenyaDSM'
epsg <- "epsg:4326"

# covariate preparation and selection
library(raster)
library(terra)
library(sf)
library(corrplot)
library(psych)
setwd(paste(proj_dir,'data/covariates_shp/soil_cov', sep="/"))
# depth - 15-30 cm, 250m resolution
covs_path <- getwd()

# create raster stack
files <- list.files(path = covs_path, pattern = "tif*$", full.names = TRUE)
covs <- stack(files)
summary(covs) # it is normal to have NoData rasters check Fan et al. use location skipping
plot(covs$occ) # null values are in the UG extent of the map (not covered in main covs)

# clean rasters: transform varying rasters in case of varying rasters error
# cov1 <- raster(paste(covs_path, "ph20.tif", sep="/"))
# cov2 <- raster(paste(covs_path, "occ.tif", sep="/"))
# cov3 <- raster(paste(covs_path, "annual_ppt.tif", sep="/"))
# cov1 <- projectRaster(from = cov1, to = cov3, method="ngb")
# cov2 <- projectRaster(from = cov2, to = cov3, method="ngb")
# 
# clean rasters : fill rasters of NA values with 0 (avoid errors in kriging)
# cov2 <- raster(paste(covs_path, "ph20.tif", sep="/"))
# cov3 <- raster(paste(covs_path, "occ.tif", sep="/"))
# cov4 <- raster(paste(covs_path, "h20cap_pf2_5.tif", sep="/"))
# 
cov2[is.na(cov2[])] <- 0
cov3[is.na(cov3[])] <- 0
# cov4[is.na(cov4[])] <- 0
# summary(cov2)

writeRaster(cov2, 'ph20.tif', overwrite=TRUE)
writeRaster(cov3, 'occ.tif', overwrite=TRUE)
# writeRaster(cov4, 'h20cap_pf2_5.tif',overwrite=TRUE)

# clean rasters: remove matching covs : h20cap
# covs <- dropLayer(covs, c(4,5))
# summary(covs)

# retry co-variate stacking
files <- list.files(path = covs_path, pattern = "tif*$", full.names = TRUE)
covs <- stack(files)
# confirm selection
summary(covs)

# load soil training data for appending: convert to SPDF
k_dat <- read.csv(paste(proj_dir,'data/soil_shp/k_pts/sample/k_sample.csv', sep="/"))
mg_dat <- read.csv(paste(proj_dir,'data/soil_shp/mg_pts/sample/mg_sample.csv', sep="/"))
coordinates(k_dat) <- ~ X + Y
coordinates(mg_dat) <- ~ X + Y
proj4string(k_dat) <- CRS(epsg)
proj4string(mg_dat) <- CRS(epsg)
# confirm that points lie within covariates
points(k_dat)

#  extract values from covariates
k_covs <- extract(x = covs, y = k_dat, sp=TRUE)
mg_covs <- extract(x = covs, y = mg_dat, sp=TRUE)
k_covs <- as.data.frame(k_covs) 
mg_covs <- as.data.frame(mg_covs) 
summary(k_covs)
summary(mg_covs)
k_covs <- k_covs[complete.cases(k_covs),] # remove nulls (location skipping)
mg_covs <- mg_covs[complete.cases(mg_covs),]

#export cleaned covs data
setwd(paste(proj_dir,'data/soil_shp/', sep="/"))
soil_path <- getwd()
write.csv(k_covs, "k_pts/sample/k_sample_full_covs.csv", row.names=FALSE)
write.csv(mg_covs, "mg_pts/sample/mg_sample_full_covs.csv", row.names=FALSE)

# do correlation tests
skew(k_covs[3:15], na.rm=TRUE) # check skewness to see variation from normal distribution
skew(mg_covs[3:15], na.rm=TRUE)

# small variations for most covs so apply pearson to select
corrplot(cor(k_covs[3:15], method="pearson"), method="color") # do correlation plots
corrplot(cor(mg_covs[3:15], method="pearson"), method="color")

# as matrix format for selection
k_cor <- abs(round(cor(x = as.matrix(k_covs[,3]), y = as.matrix(k_covs[,c(4:15)], method="spearman")),2)) # set to absolute values to capture both +ve and -ve correlations
mg_cor <- abs(round(cor(x = as.matrix(mg_covs[,3]), y = as.matrix(mg_covs[,c(4:15)], method="spearman")),2))
row.names(k_cor) <- c('corr')
row.names(mg_cor) <- c('corr')
k_cor <- as.data.frame(k_cor[,k_cor["corr",] >= 0.3]) # select weak to strong correlations: check Mukaka, 2012
mg_cor <- as.data.frame(mg_cor[,mg_cor["corr",] >= 0.3]) # select weak to strong correlations

# create and export training sets with selected covariates
k_list <- row.names(k_cor) %>% c('X', 'Y', 'k') 
mg_list <- row.names(mg_cor) %>% c('X', 'Y', 'mg')
k_dat <- subset(k_covs, select=k_list)
mg_dat <- subset(mg_covs, select=mg_list)

write.csv(k_dat, "k_pts/sample/k_sample_model.csv", row.names=FALSE)
write.csv(mg_dat, "mg_pts/sample/mg_sample_model.csv", row.names=FALSE)

### RUN FOR SUBSAMPLE ###
# load soil training data for appending: convert to SPDF
k_dat <- read.csv(paste(soil_path,'k_pts/subsample/k_subsample.csv', sep="/"))
mg_dat <- read.csv(paste(soil_path,'mg_pts/subsample/mg_subsample.csv', sep="/"))
coordinates(k_dat) <- ~ X + Y
coordinates(mg_dat) <- ~ X + Y
proj4string(k_dat) <- CRS(epsg)
proj4string(mg_dat) <- CRS(epsg)
# confirm that points lie within covariates
plot(covs$elevation)
points(k_dat)

#  extract values from covariates
k_covs <- extract(x = covs, y = k_dat, sp=TRUE)
mg_covs <- extract(x = covs, y = mg_dat, sp=TRUE)
k_covs <- as.data.frame(k_covs) 
mg_covs <- as.data.frame(mg_covs) 
summary(k_covs)
summary(mg_covs)
k_covs <- k_covs[complete.cases(k_covs),] # remove nulls (location skipping)
mg_covs <- mg_covs[complete.cases(mg_covs),]

#export cleaned covs data
write.csv(k_covs, "k_pts/subsample/k_subsample_full_covs.csv", row.names=FALSE)
write.csv(mg_covs, "mg_pts/subsample/mg_subsample_full_covs.csv", row.names=FALSE)

# create and export training sets with selected covariates
k_dat <- subset(k_covs, select=k_list)
mg_dat <- subset(mg_covs, select=mg_list)

write.csv(k_dat, "k_pts/subsample/k_subsample_model_cov.csv", row.names=FALSE)
write.csv(mg_dat, "mg_pts/subsample/mg_subsample_model_cov.csv", row.names=FALSE)
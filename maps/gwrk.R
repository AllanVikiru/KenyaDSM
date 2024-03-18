# set project directory and retrieve soil datasets
proj_dir <- 'D:/r/KenyaDSM'
wsg84 <- "+init=epsg:4326"
utm <- "+init=epsg:32737"
setwd(paste(proj_dir,'data/soil_shp/', sep="/"))
soil_path <- getwd()
k_train <- as.data.frame(read.csv("k_train/k_train_cov.csv", header = TRUE, sep = ","))
mg_train <- as.data.frame(read.csv("mg_train/mg_train_cov.csv", header = TRUE, sep = ","))

# select covariates
library(raster)
setwd(paste(proj_dir,'data/covariates_shp/train_cov', sep="/"))
covs_path<- getwd()
k_covs <-  names(k_train[1:5])
mg_covs <- names(mg_train[1:4])
k_files <- paste0(covs_path,"/",k_covs, ".tif")
mg_files <- paste0(covs_path,"/",mg_covs, ".tif")
k_covs <- stack(k_files)
mg_covs <- stack(mg_files)
summary(k_covs)
summary(mg_covs)

# data prep: convert to spatial format
library(sp)
coordinates(k_train) <- ~ X + Y
coordinates(mg_train) <- ~ X + Y
k_train@proj4string <- CRS(wsg84)
mg_train@proj4string <- CRS(wsg84)

# EDA
hist(k_train$k) # slightly right skewed
hist(mg_train$mg) # right skewed
#log transformation of targets for normal distribution
hist(log(k_train$k))
hist(log(mg_train$mg))
k_df <- k_train@data # prepare dfs
mg_df <- mg_train@data
k_df <- k_df[, c("k", names(k_covs))] # reorder columns
mg_df <- mg_df[, c("mg", names(mg_covs))]

# run MLR
k_mlr <- lm(log(k) ~ annual_precip+tree_cover+clay+occ+ph20, data = k_df)
mg_mlr <- lm(log(mg) ~ annual_precip+occ+ph20+tree_cover, data = mg_df) 
summary(k_mlr) # included more predictors at 0.2 correlation for K due to low MLR performance : 21.5 -> 30.6
summary(mg_mlr) #: 83.9

# run GWRK
library(spgwr)
mg_gwr_bandwidth <- gwr.sel(mg ~ annual_precip+occ+ph20+tree_cover, data=mg_train) # kernel bandwidth to determine how GWR will subset data
k_gwr_bandwidth <- gwr.sel(k ~ annual_precip+tree_cover+clay+occ+ph20, data=k_train) # kernel bandwidth to determine how GWR will subset data

mg_gwr <- gwr(mg ~ annual_precip+occ+ph20+tree_cover, data=mg_train, bandwidth = mg_gwr_bandwidth, hatmatrix = TRUE, se.fit = TRUE)
  
# transform CRS to projected coordinates for kriging
k_train <- spTransform(k_train, CRS(utm))
k_covs <- projectRaster(k_covs, crs = CRS(utm),method='ngb')
k_covs <- as(k_covs, "SpatialGridDataFrame")

mg_train <- spTransform(mg_train, CRS(utm))
mg_covs <- projectRaster(mg_covs, crs = CRS(utm),method='ngb')
mg_covs <- as(mg_covs, "SpatialGridDataFrame")

# regression-kriging prediction.
library(automap)
# run universal Kriging function 
k_krige <- autoKrige(formula =
                       as.formula(k_mlr$call$formula),
                     input_data = k_train,
                     new_data = k_covs,
                     verbose = TRUE)
mg_krige <- autoKrige(formula =
                        as.formula(mg_mlr$call$formula),
                      input_data = mg_train,
                      new_data = mg_covs,
                      verbose = TRUE)

plot(mg_krige) # retrieve log prediction, standard error and variograms

# transformed plots
k_rkpred <- exp(raster(k_krige$krige_output[1])) # predicted K map
k_rkpredsd <- exp(raster(k_krige$krige_output[3])) # st error of predictions
mg_rkpred <- exp(raster(mg_krige$krige_output[1])) # predicted mg map
mg_rkpredsd <- exp(raster(mg_krige$krige_output[3])) # st error of predictions

# plot and export maps
plot(k_rkpred)
plot(k_rkpredsd)
plot(mg_rkpred)
plot(mg_rkpredsd)

setwd(soil_path)
writeRaster(k_rkpred, filename = "k_train/rk_pred.tif")
writeRaster(k_rkpredsd, filename = "k_train/rk_pred_sd.tif")
writeRaster(mg_rkpred, filename = "mg_train/rk_pred.tif")
writeRaster(mg_rkpredsd, filename = "mg_train/rk_pred_sd.tif")

#export models
setwd(paste(proj_dir,'map_model', sep="/"))
saveRDS(k_mlr, file="models/k_mlr.Rds")
saveRDS(mg_mlr, file="models/mg_mlr.Rds")
saveRDS(k_krige, file="models/k_mlrk.Rds")
saveRDS(mg_krige, file="models/mg_mlrk.Rds")

# cross validation
k_train <- k_train[which(!duplicated(k_train@coords)),] #remove any duplicates

# MLRK with 10-fold CV
kkrige_cv <- autoKrige.cv(formula =
                            as.formula(k_mlr$call$formula),
                          input_data = k_train,
                          nfold=10)
mgkrige_cv <- autoKrige.cv(formula =
                             as.formula(mg_mlr$call$formula),
                           input_data = mg_train,
                           nfold=10)
summary(kkrige_cv)
summary(mgkrige_cv)

mg_mlkr_cv <- lm(mgkrige_cv$krige.cv_output@data$var1.pred ~ mgkrige_cv$krige.cv_output@data$observed)
summary(mg_mlkr_cv)
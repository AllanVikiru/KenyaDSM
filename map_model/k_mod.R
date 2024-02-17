# set project directory projections and retrieve soil potassium data
proj_dir <- 'D:/r/KenyaDSM'
wsg84 <- "+init=epsg:4326"
utm <- "+init=epsg:32236"

setwd(paste(proj_dir,'data/soil_shp/model', sep="/"))
soil_path <- getwd()
k_dat <- as.data.frame(read.csv("k_model_cov.csv", header = TRUE, sep = ","))

# select covariates
library(raster)
setwd(paste(proj_dir,'data/covariates_shp/train_cov/', sep="/"))
covs_path<- getwd()
k_covs <-  names(k_dat[1:7])
k_files <- paste0(covs_path,"/",k_covs, ".tif")
k_covs <- stack(k_files)
plot(k_covs$clay)
summary(k_covs)

# data prep: convert to spatial format
library(sp)
coordinates(k_dat) <- ~ X + Y
k_dat@proj4string <- CRS(wsg84)
points(k_dat)

# EDA
hist(k_dat$k) # right skewed
#log transformation of targets for normal distribution
hist(log(k_dat$k))
k_df <- k_dat@data # prepare df
k_df <- k_df[, c("k", names(k_covs))] # reorder columns

# run MLR
k_mlr <- lm(log(k) ~ ., data = k_df)
summary(k_mlr) # included more predictors at 0.2 correlation for K due to low MLR performance : 21.5 -> 30.6

# transform CRS to projected coordinates for kriging
k_train <- spTransform(k_train, CRS(utm))
k_covs <- projectRaster(k_covs, crs = CRS(utm),method='ngb')
k_covs <- as(k_covs, "SpatialGridDataFrame")

# regression-kriging prediction.
library(automap)
# run universal Kriging function 
k_krige <- autoKrige(formula =
                       as.formula(k_mlr$call$formula),
                     input_data = k_train,
                     new_data = k_covs,
                     verbose = TRUE)

# transformed plots
k_rkpred <- exp(raster(k_krige$krige_output[1])) # predicted K map
k_rkpredsd <- exp(raster(k_krige$krige_output[3])) # st error of predictions

# plot and export maps
plot(k_rkpred)
plot(k_rkpredsd)

setwd(soil_path)
writeRaster(k_rkpred, filename = "k_train/rk_pred.tif")
writeRaster(k_rkpredsd, filename = "k_train/rk_pred_sd.tif")

#export models
setwd(paste(proj_dir,'map_model', sep="/"))
saveRDS(k_mlr, file="models/k_mlr.Rds")
saveRDS(k_krige, file="models/k_mlrk.Rds")

# cross validation
k_train <- k_train[which(!duplicated(k_train@coords)),] #remove any duplicates

# MLRK with 10-fold CV
kkrige_cv <- autoKrige.cv(formula =
                            as.formula(k_mlr$call$formula),
                          input_data = k_train,
                          nfold=10)

summary(kkrige_cv)

mg_mlkr_cv <- lm(mgkrige_cv$krige.cv_output@data$var1.pred ~ mgkrige_cv$krige.cv_output@data$observed)
summary(mg_mlkr_cv)
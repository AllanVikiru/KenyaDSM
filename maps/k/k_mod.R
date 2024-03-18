# set project directory projections and retrieve soil potassium data
proj_dir <- 'D:/r/KenyaDSM'
wsg84 <- "+init=epsg:4326"
utm <- "+init=epsg:32236"

# select sample data - 1080 :: TODO - ADD NOISE AND SELECT FEATURES
setwd(paste(proj_dir,'data/soil/k', sep="/"))
soil_path <- getwd()
k_dat <- as.data.frame(read.csv("sample/k_sample_model.csv", header = TRUE, sep = ","))
k_dat <- subset(k_dat, select=-c(occ))

# convert to 70:30 training/testing, fill noise in training to avoid generalisation
library(caret)
part <- createDataPartition(k_dat$k, times = 1, p = 0.7, list = FALSE)
k_train <- k_dat[part, ]
k_test <- k_dat[-part, ]
head(k_train)
head(k_test)
# write data
#write.csv(k_train, "sample/k_train.csv", row.names=FALSE)
#write.csv(k_test, "sample/k_test.csv", row.names=FALSE)

# select training covariates
library(raster)
setwd(paste(proj_dir,'data/covs/local/', sep="/"))
covs_path<- getwd()
k_covs <-  names(k_dat[1:6])
k_files <- paste0(covs_path,"/",k_covs, ".tif")
k_covs <- stack(k_files)
summary(k_covs)
plot(k_covs$elevation)

# data prep: convert to spatial format
library(sp)
library(RColorBrewer) # for mapping 
coordinates(k_train) <- ~ X + Y
k_train@proj4string <- CRS(wsg84)
points(k_train)
# transform CRS to projected coordinates for kriging
k_train <- spTransform(k_train, CRS(utm))
k_covs <- projectRaster(k_covs, crs = CRS(utm),method='bilinear')
k_covs <- as(k_covs, "SpatialGridDataFrame")
k_train <- k_train[which(!duplicated(k_train@coords)),] #remove any duplicates

k_train_sf <- st_as_sf(k_train, coords = c("X", "Y"), crs = 4326)

#### run OK
library(gstat)

k_spdf <- SpatialPointsDataFrame(coords = cbind(k_train$X, k_train$Y),
                                 data = k_train,
                                 proj4string = CRS(projargs = "+init=epsg:32236"))
var <- variogram(object = k~1, locations = k_spdf)
plot(var)
fit_var <- fit.variogram(object = var, model = vgm(psill=7000, nugget= 0,
                                                   range = 0.3, model="Lin"))
plot(var, model = fit_var)

smp <- spsample(x = k_spdf, n = 1000, type="random")
krig <- krige(formula = k ~ 1, locations = k_spdf, newdata = smp, model = fit_var)
# locations - k column and location data; new - unknown locations

tmap = tm_shape(shp=krig) + tm_dots(col="var1.pred")
tmap





















# variogram: closer things are more predictable and have less variability
# set K to variogram model - for each pair of points, the gamma value/semivariance between them is plotted against the distance between them

k_expvgm <- variogram(k ~ 1, data = k_train_sf)
plot(k_expvgm) # experimental variogram

# nugget: start of points at x; range: point at y that flats out
# sill : point at x that flats out; p-sill: sill - nugget

# theoretical variogram
k_thvgm <- vgm(psill = 7000, model="Lin", range=37500, nugget = 0) # N - 0.02, R: 38000, S: 0.07 PS: 0.055 
plot(k_expvgm, k_thvgm)

# fit theoretical and experimental variograms
k_fitvgm <- fit.variogram(k_expvgm, vgm("Sph"), fit.kappa=TRUE)
plot(k_expvgm, k_fitvgm, main = "Soil K variogram")

#ordinary kriging
k_ok <- krige(k ~ 1, 
              locations = k_train_sf,
              newdata = k_covs, 
              model= k_fitvgm)

brewer <- brewer.pal(9,"Blues")

plot(k_ok$var1.pred, col = brewer, main = "Log Soil Potassium (OK)", na.rm=TRUE)
k_ok$pred_k <- exp(k_ok$var1.pred)
plot(k_ok["pred_k"], main = "Predicted Soil Potassium (OK)")




library(automap)
k_ok <- autoKrige(log(k) ~ 1, 
                  input_data = k_train, 
                  verbose = TRUE)
plot(k_ok)
summary(k_ok)
## AUTOKRIGE BRINGS UNREALISTIC OUTCOMES
# 5 fold cv of OK model
k_ok_cv <- autoKrige.cv(formula = log(k) ~ 1,
                        input_data = k_train,
                        verbose = c(interactive(), interactive()),
                        nfold = 5)
summary(k_ok_cv) # RMSE 0.111
k_ok_mod <- lm(k_ok_cv$krige.cv_output@data$var1.pred ~ k_ok_cv$krige.cv_output@data$observed)
summary(k_ok_mod) # R2 = 0.936

# generate OK maps
k_okpred <- exp(raster(k_ok$krige_output[1])) # predicted K map
# k_okpredsd <- exp(raster(k_ok$krige_output[3])) # st error of predictions

k_okpred <- projectRaster(k_okpred, crs = wsg84)
# k_okpredsd <- projectRaster(k_okpredsd, crs = wsg84)

plot(k_okpred)
# plot(k_okpredsd)

# export model and map
setwd(model_path)
saveRDS(k_ok, file="k_ok.Rds")

setwd(soil_path)
writeRaster(k_okpred, filename = "models/kok_pred.tif")

# run MLRK
k_mlrk <- autoKrige(formula = as.formula(k_eqn), 
                    input_data = k_train, 
                    new_data = k_covs,
                    verbose = TRUE)
plot(k_mlrk)

# k_mlrk_cv <- autoKrige.cv(formula = as.formula(k_eqn),
#                        input_data = k_train, 
#                        verbose = c(interactive(), interactive()), 
#                        nfold = 5)
# summary(k_mlrk_cv) # RMSE 0.1075
# 
# k_mlkr_mod <- lm(k_mlrk_cv$krige.cv_output@data$var1.pred ~ k_mlrk_cv$krige.cv_output@data$observed)
# summary(k_mlkr_mod) # R2 = 0.941

# generate MLRK maps
k_mlrkpred <- exp(raster(k_mlrk$krige_output[1])) # predicted K map
# k_mlrkpredsd <- exp(raster(k_mlrk$krige_output[3])) # st error of predictions

k_mlrkpred <- projectRaster(k_mlrkpred, crs = wsg84)

plot(k_mlrkpred)

# export model and map
setwd(model_path)
saveRDS(k_mlrk, file="k_mlrk.Rds")

setwd(soil_path)
writeRaster(k_mlrkpred, filename = "models/kmlrk_pred.tif")



#### EDA#####
hist(k_dat$k) # right skewed
hist(log(k_dat$k)) #log transformation of targets for normal distribution

# regression equation
k_eqn <- as.formula(log(k) ~ annual_ppt+elevation+slope+temp_annual_range+treecover+wetness_index)
# cv <- trainControl(method = "cv", number = 5) # 5 fold CV (80% training, 20% testing)

### run MLR
k_mlr <- train(k_eqn, data = k_train@data, method= "lm")
print(k_mlr) # R-squared  - 0.657 RMSE - 0.261

# generate MLR model maps
k_mlrpred <- exp(predict(k_covs, k_mlr))
plot(k_mlrpred)

# export model and map
setwd(model_path)
saveRDS(k_mlr, file="k_mlr.Rds")

setwd(soil_path)
writeRaster(k_mlrpred, filename = "models/kmlr_pred.tif")

### run SVM
library(e1071)
k_svm <- train(k_eqn, data = k_train@data, method = 'svmRadial')
print(k_svm) # RMSE - 0.165 R2 - 0.864, gamma = 0.222, C = 1

# confirm tuned hyperparameters of gamma and cost in 5 fold cv
# ct <- tune.control(cross = 5) # 5 fold CV (80% training, 20% testing)
svm_tune <- tune(svm, 
                 k_eqn, 
                 data = k_train@data[,c("k",names(k_covs))],
                 ranges = list(
                   gamma = seq(0.2,0.5,0.05),
                   cost = seq(0.1,1,0.1)))
plot(svm_tune) # towards 0.2 gamma, 1 cost
svm_tune$best.parameters

# generate svm maps
k_svmpred <- exp(predict(k_covs, k_svm))
plot(k_svmpred)

# export model and map
setwd(paste(proj_dir,'map_model/models', sep="/"))
model_path <- getwd()
saveRDS(k_svm, file="k_svm.Rds")

setwd(soil_path)
writeRaster(k_svmpred, filename = "models/ksvm_pred.tif")

# transform CRS to projected coordinates for kriging
k_train <- spTransform(k_train, CRS(utm))
k_covs <- projectRaster(k_covs, crs = CRS(wsg84),method='bilinear')
k_covs <- as(k_covs, "SpatialGridDataFrame")
k_train <- k_train[which(!duplicated(k_train@coords)),] #remove any duplicates
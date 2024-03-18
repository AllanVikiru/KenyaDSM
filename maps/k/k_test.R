# set project directory projections and retrieve soil potassium data
proj_dir <- 'D:/r/KenyaDSM'
wsg84 <- "+init=epsg:4326"
utm <- "+init=epsg:32236"

# select test data - 323
setwd(paste(proj_dir,'data/soil_shp/k_pts', sep="/"))
soil_path <- getwd()
k_test <- as.data.frame(read.csv("sample/k_test.csv", header = TRUE, sep = ","))
coordinates(k_test) <- ~ X + Y
k_test@proj4string <- CRS(wsg84)

# select generated maps
library(raster)
ksvm_pred <- raster("models/ksvm_pred.tif", sep="/")
kmlr_pred <- raster("models/kmlr_pred.tif", sep="/")
kok_pred <- raster("models/kok_pred.tif", sep="/")
kmlrk_pred <- raster("models/kmlrk_pred.tif", sep="/")

# estimate prediction errors
k_test <- extract(x = ksvm_pred, y = k_test, sp = TRUE)
k_test <- extract(x = kmlr_pred, y = k_test, sp = TRUE)
k_test <- extract(x = kok_pred, y = k_test, sp = TRUE)
k_test <- extract(x = kmlrk_pred, y = k_test, sp = TRUE)
k_test$PE_svm <- k_test$ksvm_pred - k_test$k
k_test$PE_mlr <- k_test$kmlr_pred - k_test$k
k_test$PE_ok <- k_test$kok_pred - k_test$k
k_test$PE_mlrk <- k_test$kmlrk_pred - k_test$k

summary(k_test)
# export validation tests
write.csv(k_test, "models/validation.csv", row.names = F)

# calculate R-squared
R2_svm <- lm(k_test$ksvm_pred ~ k_test$k)
summary(R2_svm) # Adjusted R-squared:  0.92 **
R2_mlr <- lm(k_test$kmlr_pred ~ k_test$k)
summary(R2_mlr) # Adjusted R-squared:  0.46 ** 
R2_ok <- lm(k_test$kok_pred ~ k_test$k)
summary(R2_ok)# Adjusted R-squared:  0.96
R2_mlrk <- lm(k_test$kmlrk_pred ~ k_test$k)
summary(R2_mlrk) # Adjusted R-squared:  0.97

# calculate Root Mean Squared Error (RMSE)
RMSE_svm <- sqrt(sum(k_test$PE_svm^2, na.rm=TRUE) / length(k_test$PE_svm)) # 28.06
RMSE_mlr <- sqrt(sum(k_test$PE_mlr^2, na.rm=TRUE) / length(k_test$PE_mlr)) # 88.08
RMSE_ok <- sqrt(sum(k_test$PE_ok^2, na.rm=TRUE) / length(k_test$PE_ok))# 18.43
RMSE_mlrk <- sqrt(sum(k_test$PE_mlrk^2, na.rm=TRUE) / length(k_test$PE_mlrk)) # 16.84
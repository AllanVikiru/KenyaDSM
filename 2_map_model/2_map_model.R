# set project directory and retrieve datasets
proj_dir <- 'D:/r/KenyaDSM'
epsg <- "epsg:4326"
setwd(paste(proj_dir,'data/covariates_shp', sep="/"))
k_train <- as.data.frame(read.csv("k_train_cov.csv", header = TRUE, sep = ","))
mg_train <- as.data.frame(read.csv("mg_train_cov.csv", header = TRUE, sep = ","))

# EDA
# distribution of k and mg
hist(k_train$k)
hist(mg_train$mg)

# data prep
library(ggplot2)
library(gstat)
library(RColorBrewer)
library(sf)
library(stars)
library(viridis)
k_sf <- st_as_sf(k_train, coords= c("X", "Y"), crs = 4326)
setwd(paste(proj_dir,'data/soil_shp', sep="/"))
train_counties <- st_read("./soil_counties/soil_train_counties.shp")

plot(k_sf["k"])
plot(st_geometry(train_counties), add=TRUE)

# EDA
hist(k_sf$k)
k_sf$log_k <- log(k_sf$k + 1e-1) #log transformation for linear model
hist(k_sf$log_k)

# build linear model
k_lm <- lm(log_k ~ tree_cover+annual_precip+land_elevation+occ+ph20+temp_annual_range, k_sf)
summary(k_lm)

# fit residuals
k_sf$resid <- residuals(k_lm)

# set residuals to variogram
resid.var <- variogram(resid ~ 1, k_sf)
plot(resid.var)

#regression kriging
library(gstat)
library(GSIF)
k_grid <- k_train[7:8]
head(k_train)

# fit linear model
rk_K <- lm(log1p(k)~ tree_cover+annual_precip+land_elevation+occ+ph20+temp_annual_range, k_train)
summary(rk_K)

# derive residuals
rk_K_mod <- fit.gstatModel(
  k_train, 
  log1p(k)~ tree_cover+annual_precip+land_elevation+occ+ph20+temp_annual_range,
  k_grid
  )

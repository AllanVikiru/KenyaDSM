# inspecting soil variables
library(GSIF)
data(soil.legends)
str(soil.legends) # display standard soil legends and properties within GSIF

# convert texture classes to fractions
library(soiltexture)
TT.plot(class.sys = "USDA.TT")
#or
TT.classes.tbl(class.sys = "USDA.TT", collapse = ", ") # defined as points

#create SpatialPolygons object from geometric definitions of classes:
vert <- TT.vertices.tbl(class.sys = "USDA.TT")
vert$x <- 1-vert$SAND+(vert$SAND-(1-vert$SILT))*0.5
vert$y <- vert$CLAY*sin(pi/3)
USDA.TT <- data.frame(TT.classes.tbl(class.sys = "USDA.TT", collapse = ", "))
TT.pnt <- as.list(rep(NA, length(USDA.TT$name)))
poly.lst <- as.list(rep(NA, length(USDA.TT$name)))

# list of polygons
library(sp)
for(i in 1:length(USDA.TT$name)){
  TT.pnt[[i]] <- as.integer(strsplit(unclass(paste(USDA.TT[i, "points"])), ", ")[[1]])
  poly.lst[[i]] <- vert[TT.pnt[[i]],c("x","y")]
  ## add extra point:
  pp <- Polygon(rbind(poly.lst[[i]], poly.lst[[i]][1,]))
  poly.lst[[i]] <- sp::Polygons(list(pp), ID=i)
}
# convert to Spatial object
poly.sp <- SpatialPolygons(poly.lst, proj4string=CRS(as.character(NA)))
poly.USDA.TT <- SpatialPolygonsDataFrame(poly.sp,data.frame(ID=USDA.TT$name), match.ID=FALSE)

slot(slot(poly.USDA.TT, "polygons")[[1]], "labpt") #centroid of a polygon

#converts xy coords to texture values
get.TF.from.XY <- function(df, xcoord, ycoord) {
  df$CLAY <- df[,ycoord]/sin(pi/3)
  df$SAND <- (2 - df$CLAY - 2 * df[,xcoord]) * 0.5
  df$SILT <- 1 - (df$SAND + df$CLAY)
  return(df)
}
#estimate soil fractions based on USDA soil classes
USDA.TT.cnt <- data.frame(t(sapply(slot(poly.USDA.TT, "polygons"), slot, "labpt")))
USDA.TT.cnt$name <- poly.USDA.TT$ID
USDA.TT.cnt <- get.TF.from.XY(USDA.TT.cnt, "X1", "X2")
USDA.TT.cnt[,c("SAND","SILT","CLAY")] <- signif(USDA.TT.cnt[,c("SAND","SILT","CLAY")], 2)
USDA.TT.cnt[,c("name","SAND","SILT","CLAY")]

# run probability of correct estimation of texture fractions based on class
sim.Cl <- data.frame(spsample(poly.USDA.TT[poly.USDA.TT$ID=="clay",],
                              type="random", n=100))
sim.Cl <- get.TF.from.XY(sim.Cl, "x", "y")
sd(sim.Cl$SAND); sd(sim.Cl$SILT); sd(sim.Cl$CLAY) # results show estimation at +/- 15%

# use of africa soil profiles data
require(GSIF)
data(afsp)
tdf <- afsp$horizons[,c("CLYPPT", "SLTPPT", "SNDPPT")]
tdf <- tdf[!is.na(tdf$SNDPPT)&!is.na(tdf$SLTPPT)&!is.na(tdf$CLYPPT),]
tdf <- tdf[runif(nrow(tdf))<.15,]
tdf$Sum <- rowSums(tdf)
for(i in c("CLYPPT", "SLTPPT", "SNDPPT")) { tdf[,i] <- tdf[,i]/tdf$Sum * 100 }
names(tdf)[1:3] <- c("CLAY", "SILT", "SAND")
TT.plot(class.sys = "USDA.TT", tri.data = tdf,
        grid.show = FALSE, pch="+", cex=.4, col="red")
# demonstrates almost varying probabability of estimating conversion of soil texture classes to fractions

#use of munsell colour codes
setwd("D:/r/KenyaDSM/demo")
load("munsell_rgb.rdata")
munsell.rgb[round(runif(1)*2350, 0),]
as(colorspace::RGB(R=munsell.rgb[1007,"R"]/255,
                   G=munsell.rgb[1007,"G"]/255,
                   B=munsell.rgb[1007,"B"]/255), "HSV")
# convert from rgb to munsell

#aqp from munsell to rgb
aqp::munsell2rgb(the_hue = "10B", the_value = 2, the_chroma = 12)
#hex to rgb vals
col2rgb("#003878FF")
#hex to kml
col2kml("#003878FF")

#plot soil colours in AFSP
data(afsp) # https://gsif.r-forge.r-project.org/afsp.html: legacy soil profile data
head(afsp$horizons[!is.na(afsp$horizons$MCOMNS),"MCOMNS"])
# shows text representation of munsell

# merge colour codes of columns and coordinates
mcol <- plyr::join(afsp$horizons[,c("SOURCEID","MCOMNS","UHDICM","LHDICM")],
                   afsp$sites[,c("SOURCEID","LONWGS84","LATWGS84")])
mcol <- mcol[!is.na(mcol$MCOMNS),]
str(mcol)

# convert munsell to hue-saturation-intensity
mcol$Munsell <- sub(" ", "", sub("/", "_", mcol$MCOMNS))
hue.lst <- expand.grid(c("2.5", "5", "7.5", "10"),
                       c("YR","GY","BG","YE","YN","YY","R","Y","B","G"))
hue.lst$mhue <- paste(hue.lst$Var1, hue.lst$Var2, sep="")
for(j in hue.lst$mhue[1:28]){
  mcol$Munsell <- sub(j, paste(j, "_", sep=""), mcol$Munsell, fixed=TRUE)
}
mcol$depth <- mcol$UHDICM + (mcol$LHDICM-mcol$UHDICM)/2
mcol.RGB <- merge(mcol, munsell.rgb, by="Munsell")
str(mcol.RGB)

#plot topsoil colours for africa
mcol.RGB <- mcol.RGB[!is.na(mcol.RGB$R),]
mcol.RGB$Rc <- round(mcol.RGB$R/255, 3)
mcol.RGB$Gc <- round(mcol.RGB$G/255, 3)
mcol.RGB$Bc <- round(mcol.RGB$B/255, 3)
mcol.RGB$col <- rgb(mcol.RGB$Rc, mcol.RGB$Gc, mcol.RGB$Bc)
mcol.RGB <- mcol.RGB[mcol.RGB$depth>0 & mcol.RGB$depth<30 & !is.na(mcol.RGB$col),]
coordinates(mcol.RGB) <- ~ LONWGS84+LATWGS84
load("admin.af.rda")
proj4string(admin.af)<- "+proj=longlat +datum=WGS84"
country <- as(admin.af, "SpatialLines")
par(mar=c(.0,.0,.0,.0), mai=c(.0,.0,.0,.0))
plot(country, col="darkgrey", asp=1)
points(mcol.RGB, pch=21, bg=mcol.RGB$col, col="black")

#plot colours of horizons
library(plyr)
library(aqp)
lon = 3.90; lat = 7.50; id = "ISRIC:NG0017"; FAO1988 = "LXp"
top = c(0, 18, 36, 65, 87, 127)
bottom = c(18, 36, 65, 87, 127, 181)
ORCDRC = c(18.4, 4.4, 3.6, 3.6, 3.2, 1.2)
hue = c("7.5YR", "7.5YR", "2.5YR", "5YR", "5YR", "10YR")
value = c(3, 4, 5, 5, 5, 7); chroma = c(2, 4, 6, 8, 4, 3)
## prepare a SoilProfileCollection:
prof1 <- plyr::join(data.frame(id, top, bottom, ORCDRC, hue, value, chroma),data.frame(id, lon, lat, FAO1988), type='inner')
#> Joining by: id
prof1$soil_color <- with(prof1, aqp::munsell2rgb(hue, value, chroma))
# converting hue to character
depths(prof1) <- id ~ top + bottom
# converting IDs from factor to character
site(prof1) <- ~ lon + lat + FAO1988
coordinates(prof1) <- ~ lon + lat
proj4string(prof1) <- CRS("+proj=longlat +datum=WGS84")
prof1
plotKML(prof1, var.name="ORCDRC", color.name="soil_color")
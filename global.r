#source("leafletmap.r")
library(rgdal)
library(raster)

ce <- read.csv("outputs/coastal_exposure/coastal_exposure.csv", header=T)
aoi <- readOGR(dsn="intermediate/00_preprocessing", layer="hdr_reprojected_aoi")

aoi.wgs84 <- spTransform(aoi, CRS("+proj=longlat +datum=WGS84 +no_defs"))
points.wgs84 <- rgdal::project(as.matrix(ce[,1:2]), proj=projection(aoi), inv=T)

ce <- cbind(ce, points.wgs84)
names(ce)[17:18] <- c("lon", "lat")
# UI needs to take input from server: 
#   Server needs to load files before UI dropdowns populate.
# 
# Add validation to prevent anything from trying to happen before:
#   directory is setup
# files load
# UI of other tabs loads
# server calls in other tabs.

# eliminate need to choose dir. 
# Instead mandate that all InVEST workspaces are in same parent dir
# and have UI list subdirs to choose from (effectively select IV workspace)

#source("leafletmap.r")
library(rgdal)
library(raster)

getdir <- function(){
  fs <- dir()
  ds <- fs[which(file_test("-d", fs))]
  return(ds)
}

# ce <- read.csv(file.path("outputs/coastal_exposure/coastal_exposure.csv"), header=T)
# aoi <- readOGR(dsn=file.path("intermediate/00_preprocessing"), layer="hdr_reprojected_aoi")
# 
# aoi.wgs84 <- spTransform(aoi, CRS("+proj=longlat +datum=WGS84 +no_defs"))
# points.wgs84 <- rgdal::project(as.matrix(ce[,1:2]), proj=projection(aoi), inv=T)
# 
# ce <- cbind(ce, points.wgs84)
# names(ce)[17:18] <- c("lon", "lat")
library(shiny)
runApp(".", display.mode = "showcase")


library(RColorBrewer)
library(ggplot2)
library(plyr)
library(ggmap)
library(rgeos)
library(raster)
library(rasterVis)
library(gridExtra)
library(rgdal)
library(leaflet)

### Read logfile info
setwd(".")
logfile <- readLines(con=list.files(path=".", pattern=glob2rx("coastal_vulnerability-log*.txt")), n=-1)
blanks <- which(logfile=="")
logfile <- logfile[1:(min(blanks) - 1)]
within(logfile, logfile <- data.frame(do.call('rbind', strsplit(logfile, "  ", fixed=TRUE))))

ce <- read.csv("outputs/coastal_exposure/coastal_exposure.csv", header=T)

#### MUST DO DATUM TRANSFORM 
aoi <- readOGR(dsn="intermediate/00_preprocessing", layer="hdr_reprojected_aoi")

aoi.wgs84 <- spTransform(aoi, CRS("+proj=longlat +datum=WGS84 +no_defs"))
points.wgs84 <- rgdal::project(as.matrix(ce[,1:2]), proj=projection(aoi), inv=T)

ce <- cbind(ce, points.wgs84)
names(ce)[17:18] <- c("lon", "lat")

## Leaflet Map
map <- createLeafletMap(session, 'map')

pointsInBounds <- reactive({
#   if (is.null(input$map_bounds))
#     return(uspop2000[FALSE,])
  bounds <- input$map_bounds
  latRng <- range(bounds$north, bounds$south)
  lngRng <- range(bounds$east, bounds$west)
  
  subset(ce,
         lat >= latRng[1] & lat <= latRng[2] &
           lon >= lngRng[1] & lon <= lngRng[2])
})

observe({
  pts <- pointsInBounds()
  
  map$addCircle(
    pts$lat,
    pts$lon,
    100/input$map_zoom,
    list(weight=1.2, fill=TRUE, color='#4A9'))
})

# ## GGmaps
# gg.layer <- ggmap(ggbase) +
#   geom_polygon(data=shp1.df, aes(long, lat, group=group, fill=CLASSIFY)) +
#   geom_path(color=NA) +
#   scale_fill_manual(name="CLASSIFY", values=riskycols)

## Make y variable take user input
th.bar <- theme(panel.background = element_rect(fill="white"), 
                axis.text.y=element_text(size=12),
                axis.text.x=element_text(size=12),
                #axis.title.x=element_blank(),
                #axis.title.y=element_blank(),
                #strip.text=element_blank(), 
                #strip.background=element_blank(), 
                #panel.border = element_rect(color="black", fill=NA), 
                #panel.grid.minor.x=element_line(size=.2, color="gray", linetype="dashed"), 
                panel.grid.major.y=element_line(size=.4, color="gray", linetype="dashed"),
                panel.grid.minor.y=element_blank(),
                panel.grid.minor.x=element_blank(),
                panel.grid.major.x=element_blank(),
                #legend.text=element_blank(),
                legend.position="left")



ggplot(ce, aes(x=x, y=y)) +
  geom_point(aes(color=cut(habitat_role, 5))) +
  scale_color_brewer(palette="YlOrRd", type="qual") +
  th.bar



locale <- bbox(aoi.wgs84)

locale <- matrix(NA, nrow=2, ncol=2, dimnames=list(c("x", "y"), c("min", "max")))
locale[1,1] <- min(ce$lon)
locale[1,2] <- max(ce$lon)
locale[2,1] <- min(ce$lat)
locale[2,2] <- max(ce$lat)

ggbase <- get_map(location=locale, maptype="terrain", color="color", source="google")
gg.layer <- ggmap(ggbase) +
  geom_point(data=ce, aes(x=lon, y=lat, color=cut(habitat_role, 5))) +
  scale_color_brewer(palette="YlOrRd", type="qual")
### Raster stuff

tifs <- list.files(path="HRA2/output/Maps", pattern="*.tif")
tifstack <- list()
for (j in tifs){
  #name1 <- sub(pattern=".shp", replacement="", i)
  rast <- raster(paste("HRA2/output/Maps/", j, sep=""))
  #shp.wgs84 <- spTransform(shp.prj, CRS("+proj=longlat +datum=WGS84 +no_defs"))
  tifstack[[j]] <- rast
}

p2 <- levelplot(rast.swe, margin=FALSE, colorkey=colkey, at=cutgdd, cuts=8, col.regions=rev(brewer.pal(7,"Blues")), main=list(label="SWE (mm) April 1, 1980", cex=.8), scales=list(draw=FALSE))


cutrisk <- c(0,1,2,3,4,5,6,7,8,9,10)
colkey <- list(at=cutrisk, labels=list(at=cutrisk))
levelplot(tifstack[[9]], margin=F, colorkey=colkey, at=cutrisk, cuts=11, col.regions=rev(brewer.pal(10, "RdYlBu")))

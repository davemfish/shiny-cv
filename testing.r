library(shiny)
runApp(".", display.mode = "showcase")
shiny::runGitHub('davemfish/shiny-cv')
runGist('e817fd8b2990220c2427', launch.browser = getOption("shiny.launch.browser", interactive()))
runUrl( "http://sialia.sefs.uw.edu/~dfisher5/shiny-cv-master.zip", filetype=".zip", subdir="shiny-cv-master")


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

ce <- read.csv("CV2/outputs/coastal_exposure/coastal_exposure.csv", header=T)

#### MUST DO DATUM TRANSFORM 
aoi <- readOGR(dsn="CV/intermediate/00_preprocessing", layer="hdr_reprojected_aoi")

aoi.wgs84 <- spTransform(aoi, CRS("+proj=longlat +datum=WGS84 +no_defs"))
points.wgs84 <- rgdal::project(as.matrix(ce[,1:2]), proj=projection(aoi), inv=T)

ce <- cbind(ce, points.wgs84)
names(ce)[17:18] <- c("lon", "lat")

##### Rmaps and leaflet.sync
require(devtools)
install_github('ramnathv/rCharts@dev')
install_github('ramnathv/rMaps')
library(rCharts)
library(yaml)
rMapsConfig <- yaml.load_file(file.path(leafletLib, "config.yml"))
leafletLib <- file.path(find.package("rCharts"), "libraries", "leaflet")
leafsync <- readLines("https://github.com/turban/Leaflet.Sync/blob/master/L.Map.Sync.js")
write(leafsync, file.path(leafletLib, "external", "L.Map.Sync.js"))
rMapsConfig$leaflet$jshead <- union(rMapsConfig$leaflet$jshead , "external/L.Map.Sync.js")
write(as.yaml(rMapsConfig), file.path(leafletLib, "config.yml"))

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
                strip.background=element_blank(), 
                panel.border = element_rect(color="black", fill=NA), 
                #panel.grid.minor.x=element_line(size=.2, color="gray", linetype="dashed"), 
                panel.grid.major.y=element_line(size=.4, color="gray", linetype="dashed"),
                panel.grid.minor.y=element_blank(),
                panel.grid.minor.x=element_blank(),
                panel.grid.major.x=element_blank(),
                #legend.text=element_blank(),
                legend.position="none")

pts <- ce[,5:13]
plotpts <- melt(pts)
gg.hist <- ggplot(plotpts) + 
  geom_bar(aes(x=value, y=..count..*1000, fill=cut(value, c(0,1,2,3,4,5), include.lowest=T)), binwidth=.5) +
  facet_wrap("variable", nrow=3, ncol=3) +
  scale_fill_brewer(palette="YlOrRd", type="qual") +
  scale_x_continuous(breaks=c(0:5)) +
  th.bar
gg.hist

ggplot(ce, aes(x=x, y=y)) +
  geom_point(aes(color=cut(habitat_role, 5))) +
  scale_color_brewer(palette="YlOrRd", type="qual") +
  th.bar

#### Differencing Scenarios ####
Loadtest <- function(inputX){ 
 
    ws <- inputX
    ce <- read.csv(file.path(ws, "outputs/coastal_exposure/coastal_exposure.csv"), header=T)
    aoi <- raster(file.path(ws, "intermediate/00_preprocessing/00_PRE_aoi.tif"))
    
    points.wgs84 <- rgdal::project(as.matrix(ce[,1:2]), proj=projection(aoi), inv=T)
    
    ce <- cbind(points.wgs84, ce)
    names(ce)[1:2] <- c("lon", "lat")
    print("loaded csv")
    return(ce)
}

ce1 <- Loadtest("CV")
ce2 <- Loadtest("CV2")

## radio buttons to select which fields to difference
## populated with only the names found in both
## then click submit and generate the diff dataframe

incommon <- intersect(names(ce1), names(ce2))

samp <- rnorm(200, mean=0, sd=0.5)
cols <- brewer.pal(9, "RdBu")[as.numeric(cut(samp, breaks=c(-10,-0.1,-0.05,-0.01,0,0.01,0.05,0.1,10)))]
df <- cbind(samp, cols)

br <- c(-1.5, -0.5, -0.2, -0.1, 0, 0.1, 0.2, 0.5, 1.5)

th.hist <- theme(panel.background = element_rect(fill="white"), 
                axis.text.y=element_text(size=12),
                axis.text.x=element_text(size=12),
                #axis.title.x=element_blank(),
                #axis.title.y=element_blank(),
                #strip.text=element_blank(), 
                strip.background=element_blank(), 
                panel.border = element_rect(color="black", fill=NA), 
                #panel.grid.minor.x=element_line(size=.2, color="gray", linetype="dashed"), 
                panel.grid.major.y=element_blank(),
                panel.grid.minor.y=element_blank(),
                panel.grid.minor.x=element_blank(),
                panel.grid.major.x=element_blank(),
                #legend.text=element_blank(),
                legend.position="none")

ggplot(data.frame(samp)) +
  geom_bar(aes(x=samp, fill=factor(cut(samp, breaks=br))), stat="bin", binwidth=0.1) +
  scale_fill_brewer(palette="RdBu", type="div") +
  geom_vline(data=data.frame(br), xintercept=br, linetype="dashed") +
  scale_x_continuous(breaks=br) +
  #xlim(min(samp), max(samp)) +
  th.bar

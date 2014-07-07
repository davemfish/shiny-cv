library(shiny)
library(rgdal)
library(sp)
library(RColorBrewer)
library(ggplot2)
library(plyr)
library(ggmap)
library(rgeos)
library(raster)
#library(rasterVis)
library(gridExtra)
library(reshape2)
library(leaflet)
#library(plotGoogleMaps)

## ggplot theme
th.bar <- theme(panel.background = element_rect(fill="white"), 
                axis.text.y=element_text(size=12),
                axis.text.x=element_text(size=12),
                #axis.title.x=element_blank(),
                #axis.title.y=element_blank(),
                #panel.border = element_rect(color="black", fill=NA), 
                #panel.grid.minor.x=element_line(size=.2, color="gray", linetype="dashed"), 
                panel.grid.major.y=element_line(size=.4, color="gray", linetype="dashed"),
                panel.grid.minor.y=element_blank(),
                panel.grid.minor.x=element_blank(),
                panel.grid.major.x=element_blank(),
                #legend.text=element_blank(),
                legend.position="left")

print("start function")
###### Server Function ##############

shinyServer(function(input, output, session) {
  
  logfile <- readLines(con=list.files(pattern=glob2rx("coastal_vulnerability-log*.txt")), n=-1)
  blanks <- which(logfile=="")
  logfile <- logfile[1:(min(blanks) - 1)]
  
  ce <- read.csv("outputs/coastal_exposure/coastal_exposure.csv", header=T)
  
  aoi <- readOGR(dsn="intermediate/00_preprocessing", layer="hdr_reprojected_aoi")
  
  aoi.wgs84 <- spTransform(aoi, CRS("+proj=longlat +datum=WGS84 +no_defs"))
  points.wgs84 <- rgdal::project(as.matrix(ce[,1:2]), proj=projection(aoi), inv=T)
  
  ce <- cbind(ce, points.wgs84)
  names(ce)[17:18] <- c("lon", "lat")
  
  ## Leaflet Map
  map <- createLeafletMap(session, 'map')
  
  pointsInBounds <- reactive({
   if (is.null(input$map_bounds))
     return(ce)
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    subset(ce,
           lat >= latRng[1] & lat <= latRng[2] &
             lon >= lngRng[1] & lon <= lngRng[2])
  })
  
  observe({
    pts <- pointsInBounds()
    print(dim(pts))
    pts$col <- brewer.pal(5, "YlOrRd")[as.numeric(cut(pts$coastal_exposure, breaks=5))]
    map$addCircle(
      pts$lat,
      pts$lon,
      (200/input$map_zoom)^2,
      row.names(pts),
#       lapply(brewer.pal(5, "YlOrRd"), function(x) {
#         list(color = x)
#       })
      list(fill=TRUE, fillOpacity=1, stroke=F, fillColor=pts$col)
    )
  })
  
  output$hist <- renderPlot({
    pts <- pointsInBounds()
    pts <- pts[,5:13]
    plotpts <- melt(pts)
    gg.hist <- ggplot(plotpts) + 
      geom_bar(aes(x=value), width=.4) +
      facet_wrap("variable", nrow=3, ncol=3) +
      #scale_fill_manual(name="RISK", values=riskycols) +
      th.bar
    print(gg.hist)
  })
  
#   locale <- matrix(NA, nrow=2, ncol=2, dimnames=list(c("x", "y"), c("min", "max")))
#   locale[1,1] <- min(ce$lon)
#   locale[1,2] <- max(ce$lon)
#   locale[2,1] <- min(ce$lat)
#   locale[2,2] <- max(ce$lat)

#   riskycols <- c("red", "blue", "yellow")
#   names(riskycols) <- c("HIGH", "LOW", "MED")
  
  
  print("make basemap")
  ## Get parameters for map window from AOI
    
#   output$vulnmap <- renderPlot({
#     ggbase <- get_map(location=locale, maptype="terrain", color="color", source="google", zoom=input$zoom)
#     ce$mapvar <- ce[[input$mapvar]]
#     gg.layer <- ggmap(ggbase) +
#       geom_point(data=ce, aes(x=lon, y=lat, color=cut(mapvar, 5))) +
#       scale_color_brewer(palette="YlOrRd", type="qual")
#     print(gg.layer)
#   })

#   output$hist <- renderPlot({  
#     ## Make y variable take user input
#     ce$histvar <- ce[[input$var]]
#     gg.hist <- ggplot(ce) + 
#       geom_bar(aes(x=histvar), width=.4) +
#       #scale_fill_manual(name="RISK", values=riskycols) +
#       th.bar
#     print(gg.hist)
#   })
  
  output$vulnerability <- renderDataTable({ 
    print(ce)
  })

  output$downloadCSV <- downloadHandler(
    filename = paste('data-', '.csv', sep=''),
    content = function(file) {write.csv(ce, file)}
  )

  output$config <- renderTable({ 
    matrix(logfile)
  })
  
})
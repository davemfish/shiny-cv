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
library(rCharts)
library(xtable)



### ggplot theme ####
th.bar <- theme(panel.background = element_rect(fill="white"), 
                axis.text.y=element_text(size=12),
                axis.text.x=element_text(size=12),
                axis.title.x=element_text(size=14),
                axis.title.y=element_text(size=14),
                strip.text=element_text(size=11), 
                strip.background=element_blank(), 
                panel.border = element_rect(color="black", fill=NA), 
                #panel.grid.minor.x=element_line(size=.2, color="gray", linetype="dashed"), 
                panel.grid.major.y=element_line(size=.4, color="gray", linetype="dashed"),
                panel.grid.minor.y=element_blank(),
                panel.grid.minor.x=element_blank(),
                panel.grid.major.x=element_blank(),
                #legend.text=element_blank(),
                legend.position="none")

print("start function")
###### Server Function ##############

shinyServer(function(input, output, session) {
  
  loadCSV <- reactive ({ 
    if (input$upload == 0)
      return(NULL)

    isolate({
    ws <- input$InVEST
    ce <- read.csv(file.path(ws, "outputs/coastal_exposure/coastal_exposure.csv"), header=T)
    aoi <- raster(file.path(ws, "intermediate/00_preprocessing/00_PRE_aoi.tif"))
    
    points.wgs84 <- rgdal::project(as.matrix(ce[,1:2]), proj=projection(aoi), inv=T)
    
    ce <- cbind(points.wgs84, ce)
    names(ce)[1:2] <- c("lon", "lat")
    print("loaded csv")
    return(ce)
    })
  })
  
  loadLOG <- reactive({
    if (input$upload == 0)
      return(NULL)
#     validate(
#       need(input$InVEST != "", "Please select an InVEST workspace")
#     )

    #isolate({
      #input$upload
      ws <- input$InVEST
      logfile <- readLines(con=file.path(ws, list.files(path=ws, pattern=glob2rx("coastal_vulnerability-log*.txt"))), n=-1)
      blanks <- which(logfile=="")
      log <- logfile[1:(min(blanks) - 1)]
      print("loaded log")
      print(log[6])
      return(log)
    #})
    
  })
  
  output$config <- renderTable({
    if (input$upload == 0)
      return(NULL)
    isolate({ matrix(loadLOG()) })
  })
  output$directory <- renderText({
    if (input$upload == 0)
      return(NULL)
    isolate({
      tail(unlist(strsplit(tail(loadLOG(), 1), split=" ")), 1)
    })
  })

  output$leafmap <- renderUI({
    if (input$upload == 0)
      return(NULL)
    isolate({
      ce <- loadCSV()
    })
    leafletMap(
      "map", "100%", 400,
      initialTileLayer = "https://a.tiles.mapbox.com/v3/geointerest.map-dqz2pa8r/{z}/{x}/{y}.png",
      initialTileLayerAttribution = HTML('OSM & Mapbox'),
      options=list(
        center = c(mean(ce$lat), mean(ce$lon)),
        zoom = 8,
        maxBounds = list(list(min(ce$lat)-1, min(ce$lon)-1), list(max(ce$lat)+1, max(ce$lat)+1))
      )
    )
  })

  observe({
    if (input$upload == 0)
      return(NULL)
    isolate({
      print("updating select")
      ce <- loadCSV()
      updateSelectInput(session, "mapvar",
                            label = "Map Layer",
                            choices = names(ce),
                            selected = "coastal_exposure"
                          )
    })
  })
  
  ##### Leaflet-Shiny Map ######
  
  map <- createLeafletMap(session, 'map')
  

  pointsInBounds <- reactive({
    validate(
      need(input$InVEST != "", "Please select an InVEST workspace")
    )
   if (is.null(input$map_bounds))
     return(loadCSV())
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    subset(loadCSV(),
           lat >= latRng[1] & lat <= latRng[2] &
             lon >= lngRng[1] & lon <= lngRng[2])
  })
  
  ## use the selected input variable to apply a color pallette
  getCol <- reactive({
    if (input$upload == 0)
      return(NULL)
    if (input$mapvar == "")
      return(NULL)
#     validate(
#       need(!is.null(input$mapvar), "Please select a map layer")
#     )
    #isolate({
      ce <- loadCSV()
      print(class(ce))
      print(input$mapvar)
      cols <- brewer.pal(5, "YlOrRd")[as.numeric(cut(ce[[input$mapvar]], breaks=5))]
      #print(head(cols))
      return(cols)
    #})
  })


  observe({
    if (input$upload == 0)
      return(NULL)
#     validate(
#       need(input$mapvar != "", "Please select a map layer")
#     )
    ce <- loadCSV()
    ce$col <- getCol()
    #print(head(ce$col))
    #pts <- pointsInBounds()
    #print(dim(pts))
    map$addCircle(
      ce$lat,
      ce$lon,
      1000/(input$map_zoom^1.2),
      row.names(ce),
#       lapply(brewer.pal(5, "YlOrRd"), function(x) {
#         list(color = x)
#       })
      list(fill=TRUE, fillOpacity=1, stroke=F, fillColor=ce$col)
    )
  })


observe({
#   validate(
#     need(input$mapvar != "", "Please select a map layer")
#   )
  if (input$upload == 0)
    return(NULL)
  
  event <- input$map_shape_click
  if (is.null(event))
    return()
  map$clearPopups()
  
  isolate({
    ce <- loadCSV()
    #cities <- topCitiesInBounds()
    coast <- ce[row.names(ce) == event$id,]
    #selectedcoast <<- coast
    content <- print.xtable(xtable(t(coast[c(1,2,7:ncol(coast))])), type="html")
    map$showPopup(event$lat, event$lng, content, event$id)
  })
})

# Res <- reactive({
#   if (input$upload == 0)
#     return(NULL)
#   #isolate({
#   cellsize <- loadLOG()[6]
#   print(cellsize)
#   resolution <- as.numeric(tail(unlist(strsplit(cellsize, split=" ")), 1))
#   print(resolution)
#   #return(resolution)
#   #})
# })
# 
# observe({print(Res())})


#print(res())
output$hist <- renderPlot({
  pts <- pointsInBounds()
  pts <- pts[,7:ncol(pts)]
  plotpts <- melt(pts)
  #pt2km <- Res()
  gg.hist <- ggplot(plotpts) + 
    geom_bar(aes(x=value, y=..count.., fill=cut(value, 5)), binwidth=.5) +
    facet_wrap("variable", nrow=ceiling(sqrt(ncol(pts))), ncol=ceiling(sqrt(ncol(pts)))) +
    scale_fill_brewer(palette="YlOrRd", type="qual") +
    xlim(0,5) +
    ylab("count") +
    th.bar
  print(gg.hist)
})

##### GGmaps #######
#   locale <- matrix(NA, nrow=2, ncol=2, dimnames=list(c("x", "y"), c("min", "max")))
#   locale[1,1] <- min(ce$lon)
#   locale[1,2] <- max(ce$lon)
#   locale[2,1] <- min(ce$lat)
#   locale[2,2] <- max(ce$lat)
#   ggbase <- get_map(location=locale, maptype="terrain", color="color", source="google", zoom=input$zoom)
# 
# 
# output$ggmaps <- renderPlot({
#   ce$mapvar <- ce[[input$mapvar]]
#   gg.layer <- ggmap(ggbase) +
#     geom_point(data=ce, aes(x=lon, y=lat, color=cut(mapvar, 5))) +
#     scale_color_brewer(palette="YlOrRd", type="qual")
#   print(gg.layer)
# })

# observe({
#   pts <- pointsInBounds()
#   print(dim(pts))
#   pts$col <- brewer.pal(5, "YlOrRd")[as.numeric(cut(pts$wave_exposure, breaks=5))]
#   map2$addCircle(
#     pts$lat,
#     pts$lon,
#     (200/input$map_zoom)^2,
#     row.names(pts),
#     #       lapply(brewer.pal(5, "YlOrRd"), function(x) {
#     #         list(color = x)
#     #       })
#     list(fill=TRUE, fillOpacity=1, stroke=F, fillColor=pts$col)
#   )
# })
  

# ##### Rmaps Map ######
# 
# getCol <- reactive({
#   cols <- brewer.pal(5, "YlOrRd")[as.numeric(cut(ce[[input$Rmapvar]], breaks=5))]
#   print(head(cols))
#   return(cols)
# })
# 
#   plotMap <- function(){
#     ce$fillColor <- getCol()
#     
#     map1 <- Leaflet$new()
#     map1$set(width=400, height=400)
#     map1$setView(c(mean(ce$lat), mean(ce$lon)), 7)
#     map1$tileLayer('http://otile1.mqcdn.com/tiles/1.0.0/sat/{z}/{x}/{y}.jpg')
#     map1$addAssets(jshead = "https://github.com/turban/Leaflet.Sync/blob/master/L.Map.Sync.js")
#     map1$geoJson(toGeoJSON(ce, lat.lon=c("lat","lon")),
#       pointToLayer =  "#! function(feature, latlng){
#       return L.circleMarker(latlng, {
#         radius: 4,
#         fillColor: feature.properties.fillColor || 'blue',    
#         color: '#000',
#         weight: 1,
#         fillOpacity: 0.8
#       })
#     } !#")
#     return(map1)
#     #map1$circle(c(49, -124.15), 500)
#     #       4,
#     #       row.names(ce),
#     #       #       lapply(brewer.pal(5, "YlOrRd"), function(x) {
#     #       #         list(color = x)
#     #       #       })
#     #       list(fill=TRUE, fillOpacity=1, stroke=F, fillColor=ce$col)
#     #       )
#     #leafletLib <- file.path(find.package("rCharts"), "libraries", "leaflet")
#     #HTML(map1$html(chartId = "rmap"))
#   }
#     
# 
# output$mapcontainer <- renderMap({
#   plotMap()
# })

##### Other Tabs ####
  


#   riskycols <- c("red", "blue", "yellow")
#   names(riskycols) <- c("HIGH", "LOW", "MED")
  
  
  ## Get parameters for map window from AOI
    

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
    print(loadCSV())
  })

  output$downloadCSV <- downloadHandler(
    filename = paste('data-', '.csv', sep=''),
    content = function(file) {write.csv(loadCSV(), file)}
  )

})
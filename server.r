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
  
  ##### Leaflet-Shiny Map ######
  
  map <- createLeafletMap(session, 'map')
  #map2 <- map
  
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
  
  ## use the selected input variable to apply a color pallette
  getCol <- reactive({
    cols <- brewer.pal(5, "YlOrRd")[as.numeric(cut(ce[[input$mapvar]], breaks=5))]
    #print(head(cols))
    return(cols)
  })


  observe({
    ce$col <- getCol()
    #print(head(ce$col))
    #pts <- pointsInBounds()
    #print(dim(pts))
    map$addCircle(
      ce$lat,
      ce$lon,
      (1.5*input$map_zoom)^2,
      row.names(ce),
#       lapply(brewer.pal(5, "YlOrRd"), function(x) {
#         list(color = x)
#       })
      list(fill=TRUE, fillOpacity=1, stroke=F, fillColor=ce$col)
    )
  })

observe({
  event <- input$map_shape_click
  if (is.null(event))
    return()
  map$clearPopups()
  
  isolate({
    #cities <- topCitiesInBounds()
    coast <- ce[row.names(ce) == event$id,]
    selectedcoast <<- coast
    content <- print.xtable(xtable(t(coast[5:18])), type="html")
    map$showPopup(event$lat, event$lng, content, event$id)
  })
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
  
  
  print("make basemap")
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
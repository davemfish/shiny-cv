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

## This silly little function gets called inside a reactive function
## It's a hack to get a reactive function to take an argument
LoadSpace <- function(inputX){
  ws <- inputX
  ce <- read.csv(file.path(ws, "outputs/coastal_exposure/coastal_exposure.csv"), header=T)
  aoi <- raster(file.path(ws, "intermediate/00_preprocessing/00_PRE_aoi.tif"))
  points.wgs84 <- rgdal::project(as.matrix(ce[,1:2]), proj=projection(aoi), inv=T)
  ce <- cbind(points.wgs84, ce)
  names(ce)[1:2] <- c("lon", "lat")
  print("loaded csv")
  return(ce)
}

shinyServer(function(input, output, session) {
  
  loadONE <- reactive({ 
    if (input$upload == 0)
      return(NULL)

    isolate({
        ce <- LoadSpace(input$InVEST)
        return(ce)
    })
  })
  
  loadTWO <- reactive({
    if (input$Difference == 0)
      return(NULL)
    isolate({
      ce.base <- LoadSpace(input$Baseline)
      ce.scen <- LoadSpace(input$Scenario)
      return(list(ce.base, ce.scen))
    })
  })
  
  
  loadLOG <- reactive({
    if (input$upload == 0)
      return(NULL)

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
      ce <- loadONE()
    })
    leafletMap(
      "map", "100%", 400,
      initialTileLayer = "https://a.tiles.mapbox.com/v3/geointerest.map-dqz2pa8r/{z}/{x}/{y}.png",
      initialTileLayerAttribution = HTML('OSM & Mapbox'),
      options=list(
        center = c(mean(ce$lat), mean(ce$lon)),
        zoom = 8,
        maxBounds = list(list(min(ce$lat)-1, min(ce$lon)-1), list(max(ce$lat)+1, max(ce$lon)+1))
      )
    )
  })

  observe({
    if (input$upload == 0)
      return(NULL)
    isolate({
      print("updating select")
      ce <- loadONE()
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
#     validate(
#       need(input$InVEST != "", "Please select an InVEST workspace")
#     )
   if (is.null(input$map_bounds))
     return(loadONE())
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    subset(loadONE(),
           lat >= latRng[1] & lat <= latRng[2] &
             lon >= lngRng[1] & lon <= lngRng[2])
  })
  
  ## use the selected input variable to apply a color pallette
  getCol <- reactive({
    if (input$upload == 0)
      return(NULL)
    if (input$mapvar == "")
      return(NULL)

    #isolate({
      ce <- loadONE()
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
    
    ce <- loadONE()
    ce$col <- getCol()
    #print(head(ce$col))
    #pts <- pointsInBounds()
    #print(dim(pts))
    map$addCircle(
      ce$lat,
      ce$lon,
      1000/(input$map_zoom^1.2),
      row.names(ce),
      list(fill=TRUE, fillOpacity=1, stroke=F, fillColor=ce$col)
    )
  })


observe({

  if (input$upload == 0)
    return(NULL)
  
  event <- input$map_shape_click
  if (is.null(event))
    return()
  map$clearPopups()
  
  isolate({
    ce <- loadONE()
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

  output$diffnames <- renderUI({
    if (input$Difference == 0)
      return(NULL)
    isolate({
      df.base <- loadTWO()[[1]]
      df.scen <- loadTWO()[[2]]
    })
    checkboxGroupInput("fieldnames", 
                 label="Select values to compare", 
                 choices=intersect(names(df.base)[-6:-1], names(df.scen)))
  })

  output$difftable <- renderDataTable({
    if (input$diffcalc == 0)
      return(NULL)
    isolate({
      df.base <- loadTWO()[[1]]
      df.scen <- loadTWO()[[2]]
      df.diff <- df.scen[ ,input$fieldnames] - df.base[ ,input$fieldnames]
    })
    print(df.diff)
  })

  
  output$vulnerability <- renderDataTable({ 
    print(loadONE())
  })

  output$downloadCSV <- downloadHandler(
    filename = paste('data-', '.csv', sep=''),
    content = function(file) {write.csv(loadONE(), file)}
  )

})
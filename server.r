library(shiny)
library(rgdal)
library(sp)
library(RColorBrewer)
library(ggplot2)
library(plyr)
library(rgeos)
library(raster)
library(reshape2)
library(leaflet)
library(rCharts)
library(xtable)
library(hwriter)



### ggplot theme ####
th.bar <- theme(panel.background = element_rect(fill="white"), 
                axis.text.y=element_text(size=11),
                axis.text.x=element_text(size=11),
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


#dirnames <- list()

shinyServer(function(input, output, session) {
  
  observe({ 
    if (input$ChooseDir == 0)
      return(NULL)
    
    dirname <- choose.dir()
    #str(input$ChooseDir)
    isolate({
      updateTextInput(session, "InVEST", "InVEST Workspace", value=dirname)
    })
  })
  
#   output$sessname <- renderUI({
#     selectInput("InVEST", "InVEST Workspace", output$session)
#   })
  
  loadONE <- reactive({ 
    if (input$upload == 0)
      return(NULL)

    isolate({
        ce <- LoadSpace(input$InVEST)
        return(ce)
    })
  })
  
observe({ 
  if (input$ChooseBase == 0)
    return(NULL)
  
  dirname <- choose.dir()
  #str(input$ChooseDir)
  isolate({
    updateTextInput("Baseline", "", value=dirname)
  })
})

observe({ 
  if (input$ChooseScen == 0)
    return(NULL)
  
  dirname <- choose.dir()
  #str(input$ChooseDir)
  isolate({
    updateTextInput("Scenario", "", value=dirname)
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

#   output$leafmap <- renderUI({
#     if (input$upload == 0)
#       return(NULL)
#     isolate({
#       ce <- loadONE()
#     })
#     leafletMap(
#       "map", "100%", 400,
#       initialTileLayer = "https://a.tiles.mapbox.com/v3/geointerest.map-dqz2pa8r/{z}/{x}/{y}.png",
#       initialTileLayerAttribution = HTML('OSM & Mapbox'),
#       options=list(
#         center = c(mean(ce$lat), mean(ce$lon)),
#         zoom = 8,
#         maxBounds = list(list(min(ce$lat)-1, min(ce$lon)-1), list(max(ce$lat)+1, max(ce$lon)+1))
#       )
#     )
#   })

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
      updateSelectInput(session, "mapvar2",
                        label = "Map Layer",
                        choices = names(ce),
                        selected = "coastal_exposure"
      )
    })
  })
  
  ##### Leaflet-Shiny Map ######
  
#   map <- createLeafletMap(session, 'map')
  

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
    if (input$mapvar2 == "")
      return(NULL)

    #isolate({
      ce <- loadONE()
      print(class(ce))
      print(input$mapvar2)
      cols <- brewer.pal(5, "YlOrRd")[as.numeric(cut(ce[[input$mapvar2]], breaks=5))]
      #print(head(cols))
      return(cols)
    #})
  })


#   observe({
#     if (input$upload == 0)
#       return(NULL)
#     
#     ce <- loadONE()
#     ce$col <- getCol()
#     #print(head(ce$col))
#     #pts <- pointsInBounds()
#     #print(dim(pts))
#     map$addCircle(
#       ce$lat,
#       ce$lon,
#       700/(input$map_zoom^1.3),
#       row.names(ce),
#       list(fill=TRUE, fillOpacity=1, stroke=F, fillColor=ce$col)
#     )
#   })


# observe({
# 
#   if (input$upload == 0)
#     return(NULL)
#   
#   event <- input$map_shape_click
#   if (is.null(event))
#     return()
#   map$clearPopups()
#   
#   isolate({
#     ce <- loadONE()
#     #cities <- topCitiesInBounds()
#     coast <- ce[row.names(ce) == event$id,]
#     #selectedcoast <<- coast
#     content <- print.xtable(xtable(t(coast[c(1,2,7:ncol(coast))])), type="html")
#     map$showPopup(event$lat, event$lng, content, event$id)
#   })
# })

  L1 <- Leaflet$new()
  #L1$addAssets(jshead = "https://github.com/turban/Leaflet.Sync/blob/master/L.Map.Sync.js")
  L1$tileLayer("https://a.tiles.mapbox.com/v3/geointerest.map-dqz2pa8r/{z}/{x}/{y}.png")
  L1$set(width = 550, height = 400)

plotMap <- reactive({

    if (input$upload == 0)
      return(NULL)
    
    ce <- loadONE()
    ce$col <- getCol()
    tmp.ce <- apply(ce, 1, as.list)
    tmp.ce <- lapply(tmp.ce, function(x){
      mat <- as.matrix(unlist(x))
      mat <- as.matrix(mat[!(rownames(mat) %in% c("x", "y", "array.row", "array.col", "col")),])
      x$popup <- hwrite(mat)
      return(x)
    })
    L1$setView(c(mean(ce$lat), mean(ce$lon)), 9)
    L1$geoJson(toGeoJSON(tmp.ce, lat='lat', lon='lon'), 
               onEachFeature = '#! function(feature, layer){
      layer.bindPopup(feature.properties.popup)
    } !#',
               pointToLayer =  "#! function(feature, latlng){
      return L.circleMarker(latlng, {
        radius: 4,
        fillColor: feature.properties.col || 'white',    
        color: '#000',
        weight: 1,
        fillOpacity: 0.8
      })
    } !#")
    L1$enablePopover(TRUE)
  return(L1)
})


output$Rleafmap <- renderMap({
    if (input$upload == 0)
      return(NULL)
    plotMap()
})





#print(res())
output$hist2 <- renderPlot({
  pts <- pointsInBounds()
  pts <- pts[,7:ncol(pts)]
  plotpts <- melt(pts)
  #pt2km <- Res()
  gg.hist <- ggplot(plotpts) + 
    geom_bar(aes(x=value, y=..count.., fill=cut(value, c(0,1,2,3,4,5), include.lowest=T)), binwidth=.5, color="white") +
    #geom_histogram(aes(x=value, color="white", fill=cut(value, c(0,1,2,3,4,5))), binwidth=.5) +
    facet_wrap("variable", nrow=ceiling(sqrt(ncol(pts))), ncol=ceiling(sqrt(ncol(pts)))) +
    scale_fill_brewer(palette="YlOrRd", type="qual") +
    scale_x_continuous(breaks=c(0:5)) +
    xlim(-0.1, 5.1) +
    ylab("# of Coastline Segments") +
    xlab("Vulnerability Index") +
    th.bar
  print(gg.hist)
})

L2 <- Leaflet$new()
#L1$addAssets(jshead = "https://github.com/turban/Leaflet.Sync/blob/master/L.Map.Sync.js")
L2$tileLayer("https://a.tiles.mapbox.com/v3/geointerest.map-dqz2pa8r/{z}/{x}/{y}.png")
L2$set(width = 550, height = 450)  

Difference <- reactive({
  if (input$Difference == 0)
    return(NULL)
  if (is.null(input$fieldnames))
    return(NULL)
  isolate({
    df.base <- loadTWO()[[1]]
    df.scen <- loadTWO()[[2]]
    diff <- df.scen[ ,input$fieldnames] - df.base[ ,input$fieldnames]
  })
  return(diff)
})

getCol2 <- reactive({
  if (input$Difference == 0)
    return(NULL)
  if (is.null(input$fieldnames))
    return(NULL)
  if (input$Symbolize == 0)
    return(NULL)
  #print(input$Breaks3)
  
  #isolate({
  isolate({
#     df.base <- loadTWO()[[1]]
#     df.scen <- loadTWO()[[2]]
#     diff <- df.scen[ ,input$fieldnames] - df.base[ ,input$fieldnames]
    #names(df.diff) <- input$fieldnames
#  })
  #print(class(ce))
  #print(input$mapvar2)
  diff <- Difference()
  colbrks <- as.numeric(cut(diff, breaks=Breaks(), labels=F))
  print("Diff info")
  print(Breaks())
  print(summary(diff))
  cols <- brewer.pal(7, "RdBu")[colbrks]
  print("cols info")
  print(head(colbrks))
  print(summary(colbrks))
  print(head(cols))
  print(length(cols))
  return(cols)
  })
})




#quantile(Difference(), probs=seq(0,1,1/7))[1]
observe({
  if (input$Difference == 0){
    return(NULL)
  }
  #print(sd(Difference()))
  #isolate({
    updateNumericInput(session,
                    inputId = "Breaks.3",
                    label = "-3",
                    value = min(Difference())
    )
  updateNumericInput(session,
                     inputId = "Breaks.2",
                     label = "-2",
                     value = round(as.numeric(sd(Difference())*-1), digits=3)
  )
  updateNumericInput(session,
                     inputId = "Breaks.1",
                     label = "-1",
                     value = round(as.numeric(sd(Difference())*-0.5), digits=3)
  )
  updateNumericInput(session,
                     inputId = "Breaks0",
                     label = "0",
                     value = 0
  )
  updateNumericInput(session,
                     inputId = "Breaks1",
                     label = "1",
                     value = round(as.numeric(sd(Difference())*0.5), digits=3)
  )
  updateNumericInput(session,
                     inputId = "Breaks2",
                     label = "2",
                     value = round(as.numeric(sd(Difference())*1), digits=3)
  )
  updateNumericInput(session,
                     inputId = "Breaks3",
                     label = "3",
                     value = max(Difference())
  )
  #})
})

Breaks <- reactive({
  brk <- round(as.numeric(c(input$Breaks.3, input$Breaks.2, input$Breaks.1, input$Breaks0, input$Breaks1, input$Breaks2, input$Breaks3)), digits=3)
  return(brk)
})

output$hist_diff <- renderPlot({
  if (input$Difference == 0)
    return(NULL)
  if (input$Symbolize == 0)
    return(NULL)
  #print(input$Breaks3)
  isolate({
    #br <- round(br, digits=3)
    print("Breaks again")
    print(Breaks())
    df <- data.frame(Difference())
    names(df) <- "Delta"
    brk <- Breaks()
    
    colscale <- cut(df$Delta, breaks=brk)
    print(dim(df))
    print(length(colscale))
    df$cols <- colscale
  gghist <- ggplot(df, aes(x=Delta)) +
    geom_bar(aes(fill=cols), stat="bin", binwidth=0.01) +
    scale_fill_brewer(palette="RdBu", type="div") +
    geom_vline(data=data.frame(brk), xintercept=brk, linetype="dashed") +
    scale_x_continuous(breaks=brk) +
    #xlim(min(df), max(df)) +
    th.hist  
  })
  print(gghist)
})

output$diffnames <- renderUI({
    if (input$Difference == 0)
      return(NULL)
    isolate({
      df.base <- loadTWO()[[1]]
      df.scen <- loadTWO()[[2]]
    })
    selectInput("fieldnames", 
                 label="Select values to compare", 
                 choices=intersect(names(df.base)[-6:-1], names(df.scen)),
                 selected = "coastal_exposure")
  })

plotMap2 <- reactive({
  if (input$Difference == 0)
    return(NULL)
  if (is.null(input$fieldnames))
    return(NULL)
  
  #isolate({
    df.base <- loadTWO()[[1]]
    df.scen <- loadTWO()[[2]]
    df.diff <- data.frame(df.scen[ ,input$fieldnames] - df.base[ ,input$fieldnames])
    names(df.diff) <- input$fieldnames
    #print(class(df.diff))
    #print(names(df.diff))
    df.diff <- cbind(df.base[,c("lat", "lon")], df.diff)
  df.diff$col <- getCol2()
  tmp.diff <- apply(df.diff, 1, as.list)
  tmp.diff <- lapply(tmp.diff, function(x){
    mat <- as.matrix(unlist(x))
    mat <- as.matrix(mat[!(rownames(mat) %in% c("x", "y", "array.row", "array.col", "col")),])
    x$popup <- hwrite(mat)
    return(x)
  })
  #})
  L2$setView(c(mean(df.diff$lat), mean(df.diff$lon)), 9)
  L2$geoJson(toGeoJSON(tmp.diff, lat='lat', lon='lon'), 
             onEachFeature = '#! function(feature, layer){
             layer.bindPopup(feature.properties.popup)
} !#',
             pointToLayer =  "#! function(feature, latlng){
             return L.circleMarker(latlng, {
             radius: 4,
             fillColor: feature.properties.col || 'white',    
             color: '#000',
             weight: 1,
             fillOpacity: 0.8
             })
} !#")
  L2$enablePopover(TRUE)
  return(L2)
  })


  
  output$difftable <- renderDataTable({
    if (input$diffcalc == 0)
      return(NULL)
    isolate({
      df.base <- loadTWO()[[1]]
      df.scen <- loadTWO()[[2]]
      df.diff <- data.frame(df.scen[ ,input$fieldnames] - df.base[ ,input$fieldnames])
      names(df.diff) <- input$fieldnames
      print(class(df.diff))
      print(names(df.diff))
      df.diff <- cbind(df.base[,c("lat", "lon")], df.diff)
    })
    return(df.diff)
  })

output$Rleafmap2 <- renderMap({
  if (is.null(input$fieldnames))
    return(NULL)
  plotMap2()
})
  
output$tablenames <- renderUI({
  if (input$upload == 0)
    return(NULL)
  isolate({
    df <- loadONE()
  })
  checkboxGroupInput("tablenames", 
              label="Select values for a table", 
              choices=names(df),
              selected = c("lon", "lat", "coastal_exposure")
  )
})

  FormatTable <- reactive({ 
    if (is.null(input$tablenames))
      return(NULL)
    isolate({
      df <- loadONE()
      print(names(df))
      print(input$tablenames)
      df <- format(df[,input$tablenames], nsmall=3, digits=3)
    })
      #print()
    return(df)
  })

output$printtable <- renderDataTable({
  FormatTable()
})

  output$downloadCSV <- downloadHandler(
    filename = paste('data-', '.csv', sep=''),
    content = function(file) {write.csv(FormatTable(), file)}
  )

})
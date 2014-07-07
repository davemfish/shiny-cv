library(shiny)
library(leaflet)

ce <- read.csv("outputs/coastal_exposure/coastal_exposure.csv", header=T)
# Define UI 
shinyUI(fluidPage(
  
  navbarPage("Coastal Vulnerability Results",
             
    tabPanel("Plots", 
             
     fluidRow(
# 
       column(7,
#               wellPanel(sliderInput('zoom', "Zoom", min=8, max=16, step=1, value=10),
#                         selectInput("mapvar", label="Variable", choices=names(ce), selected=names(ce)[12])),
# #              plotOutput("vulnmap")
leafletMap(
  "map", "100%", 400,
  initialTileLayer = 'http://otile1.mqcdn.com/tiles/1.0.0/sat/{z}/{x}/{y}.jpg',
  initialTileLayerAttribution = HTML('OSM & Mapquest'),
  options=list(
    center = c(49.0, -124.15),
    zoom = 7,
    maxBounds = list(list(17, -180), list(59, 180))
  )
)
),
#              
#       ),
       column(5,
#              wellPanel(selectInput("var", label="Variable", choices=names(ce), selected=names(ce)[12])),
#              #wellPanel(uiOutput("Xplotvar")),
              plotOutput("hist")
       )
     )
    ),

    tabPanel("Tables", 
             
    fluidRow(
      
#       column(2,
#         sidebarPanel(downloadButton("downloadCSV", label = "Download CSV", class = NULL))
#         ),
      
      column(12,
          wellPanel(downloadButton("downloadCSV", label = "Download CSV", class = NULL)),
          dataTableOutput("vulnerability")
        )
    )
    ),
    tabPanel("Help/About",
      h3("Current InVEST Configuration"),
      tableOutput("config"),
      p("This is information from the logfile produced by the InVEST model run"),
      br(),
      h3("About Plots"),
      p("The Plots tab contains maps and graphs summarizing the area of each
        habitat type and its risk classification"),
      br(),
      h3("About Tables"),
      p("The Tables tab displays the data which the maps and graphs visualize. 
        Area calculations are based on the shapefile habitat polygons
        and the Area of Interest shapefile"),
      br(),
      h3("About this application"),
      p("This web application reads workspace for your recent InVEST run, 
loads datasets from the output folder, and performs some calculations with R.
This app is built with the Shiny package for R.")
      )             
  )))
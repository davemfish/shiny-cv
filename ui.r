library(shiny)
library(leaflet)
library(rCharts)
library(xtable)

#ce <- read.csv("outputs/coastal_exposure/coastal_exposure.csv", header=T)
#centermap <- c(mean(ce$lat), mean(ce$lon))

# Define UI 
shinyUI(fluidPage(
  
  navbarPage("Coastal Vulnerability Results",
             
             tabPanel("Get Started",
                      sidebarLayout(
                        sidebarPanel(
                                     selectInput("InVEST", label="InVEST workspace", choices=getdir(), selected=NULL),
                                     tags$br()
                                     #actionButton("upload", "Upload Data")
                                     ),
                        mainPanel(
                                  h3("Current InVEST Configuration"),
                                  tableOutput("config"),
                                  p("This is information from the logfile produced by the InVEST model run")

                        )
                        )
                      ),
                          
             tabPanel("Plots",
                      fluidRow(
                        column(6,
                               leafletMap(
                                 "map", "100%", 400,
                                 initialTileLayer = 'http://otile1.mqcdn.com/tiles/1.0.0/sat/{z}/{x}/{y}.jpg',
                                 initialTileLayerAttribution = HTML('OSM & Mapquest'),
                                 options=list(
                                   center = c(49.5, -125.3),
                                   zoom = 8,
                                   maxBounds = list(list(17, -180), list(59, 180))
                                 )
                               ),
                               selectInput("mapvar", label="Map Layer", choices=NULL, selected=NULL)
                        ),
                        
                        column(6,
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
                      
                      h3("About Plots"),
                      p("The Plots tab contains maps and graphs summarizing the area of each
        habitat type and its risk classification"),
                      br(),
                      h3("About Tables"),
                      p("The Tables tab displays the data which the maps and graphs visualize."),
                      br(),
                      h3("About this application"),
                      p("This web application reads workspace for your recent InVEST run, 
loads datasets from the output folder, and performs some calculations with R.
This app is built with the Shiny package for R.")
             )             
  )))
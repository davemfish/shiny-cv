library(shiny)
library(leaflet)
library(rCharts)
library(xtable)

#ce <- read.csv("outputs/coastal_exposure/coastal_exposure.csv", header=T)
#centermap <- c(mean(ce$lat), mean(ce$lon))

# Define UI 
shinyUI(#fluidPage(
  
  navbarPage("Coastal Vulnerability Results",
             
             tabPanel("Get Started",
                      sidebarLayout(
                        sidebarPanel(
                                     #selectInput("InVEST", label="Choose InVEST workspace", choices=getdir(), selected=NULL),
                                     h4("1) Select an InVEST run to visualize"),
                                     p("Browse to the directory you defined as your workspace when you ran the InVEST model. 
                                       Your workspace contains these 3 subfolders:"),
                                     p("     'intermediate'"),
                                     p("     'outputs'"),
                                     p("     'tmp'"),
                                     actionButton("ChooseDir", "Browse to Workspace"),
                                     tags$br(),
                                     uiOutput("session"),
                                     tags$br(),
                                     h4("2) Upload InVEST results"),
                                     actionButton("upload", "Upload Results"),
                                     h4("3) Explore your results"),
                                     p("After clicking 'Upload Results', information about the parameters of your InVEST run will appear on this page.
                                       Results are visualized on the other tabs at the top. To visualize a different InVEST output, 
                                       return to this tab, browse to a new worksapce, and click upload again.")
                        ),
                        mainPanel(
                                  h3("Current InVEST Configuration"),
                                  h4(textOutput("directory")),
                                  tableOutput("config")
                                  #p("This is information from the logfile produced by the InVEST model run")

                        )
                        )
                      ),
                          
             tabPanel("Plots",
                      fluidRow(
                        column(6,
                               selectInput("mapvar", label="Map Layer", choices=NULL),
                               uiOutput("leafmap")
#                                leafletMap(
#                                  "map", "100%", 400,
#                                  #initialTileLayer = 'http://otile1.mqcdn.com/tiles/1.0.0/sat/{z}/{x}/{y}.jpg',
#                                  initialTileLayer = "https://a.tiles.mapbox.com/v3/geointerest.map-dqz2pa8r/{z}/{x}/{y}.png",
#                                  initialTileLayerAttribution = HTML('OSM & Mapbox'),
#                                  options=list(
#                                    center = c(49.25, -125.8),
#                                    zoom = 8,
#                                    maxBounds = list(list(-90, -180), list(90, 180))
#                                  )
#                                )
                        ),
                        
                        column(6,
                               h5("The histograms display only the points within the current map view"),
                               br(),
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
             tabPanel("Compare Scenarios",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("Baseline", label="Choose Baseline Results", choices=getdir(), selected=NULL),
                          selectInput("Scenario", label="Choose Scenario Results", choices=getdir(), selected=NULL),
                          actionButton("Difference", "Upload Results"),
                          tags$br(),
                          tags$br(),
                          uiOutput("diffnames"),
                          tags$br(),
                          actionButton("diffcalc", "Calculate Differences")
                        ),
                        mainPanel(
                          dataTableOutput("difftable")
                          
                        )
                      )
             ),
             tabPanel("Compare Maps",
#                       sidebarLayout(
#                         sidebarPanel(
#                           selectInput("Baseline", label="Choose Baseline Results", choices=getdir(), selected=NULL),
#                           selectInput("Scenario", label="Choose Scenario Results", choices=getdir(), selected=NULL),
#                           actionButton("Difference", "Upload Results"),
#                           tags$br(),
#                           tags$br(),
#                           uiOutput("diffnames"),
#                           tags$br(),
#                           actionButton("diffcalc", "Calculate Differences")
#                         ),
                        mainPanel("main",
                          uiOutput("mapcompare")
                        )
                      #)
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
  ))#)
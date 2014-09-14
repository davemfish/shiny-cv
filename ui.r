library(shiny)
library(leaflet)
library(rCharts)
library(xtable)
library(RColorBrewer)


textInputRow <- function (inputId, label, value = "", bgcol="red") {
  div(style=paste("display:inline-block; background-color:", bgcol, sep=""),
      tags$label(label, `for` = inputId), 
      tags$input(id = inputId, type = "text", value = value,class="input-mini"))
}

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
                                     textInput("InVEST", "", "Enter Workspace Path"),
                                     actionButton("ChooseDir", "Browse"),
                                     tags$br(),
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
                          
#              tabPanel("Plots",
#                       fluidRow(
#                         column(6,
#                                selectInput("mapvar", label="Map Layer", choices=NULL),
#                                uiOutput("leafmap")
#                         ),
#                         
#                         column(6,
#                                h5("The histograms display only the points within the current map view"),
#                                br(),
#                                plotOutput("hist")
#                         )
#                       )
#              ),
             tabPanel("Plots",
                      fluidRow(
                        column(6,
                               selectInput("mapvar2", label="Map Layer", choices=NULL),
                               mapOutput("Rleafmap")
                        ),
                        
                         column(6,
                                h5(""),
                                br(),
                                plotOutput("hist2")
                         )
                      )
             ),

             
             tabPanel("Tables", 
                      sidebarLayout(
                            sidebarPanel(
                              uiOutput("tablenames"),
                              br(),
                              downloadButton("downloadCSV", label = "Download CSV", class = NULL)
                            ),
                            mainPanel(
                              dataTableOutput("printtable"))
                      )
             ),
             tabPanel("Compare Scenarios",
                      sidebarLayout(
                        sidebarPanel(
                          p("Use this tab to compare results of two InVEST runs.
                            For example, you can visualize the differences between a baseline scenario and an additional scenario."),
                          
                          tags$br(),
                          
                          textInput("Baseline", "", "Baseline workspace"),
                          actionButton("ChooseBase", "Browse"),
                          tags$br(),
                          tags$br(),
                          textInput("Scenario", "", "Scenario workspace"),
                          actionButton("ChooseScen", "Browse"),
                          tags$br(),
                          tags$br(),
                          actionButton("Difference", "Compare Results"),
                    
                          p("After clicking 'Compare Results', values at each coastal segment 
                            of the 'Baseline' workspace are subtracted from corresponding values 
                            in the 'Scenario' workspace.")
                        ),
                        mainPanel(
                          uiOutput("diffnames"),
                          mapOutput("Rleafmap2")
#                           h5("Adjust colors"),
#                           p("Enter numbers to change the range of values assigned to each color. Use the histogram of values and the vertical dashed lines as a guide."),
#                           textInputRow(inputId="Breaks.3", label="", value=-2, bgcol=brewer.pal(7, "RdBu")[1]),
#                           textInputRow(inputId="Breaks.2", label="", value=-0.5, brewer.pal(7, "RdBu")[2]),
#                           textInputRow(inputId="Breaks.1", label="", value=-0.05, brewer.pal(7, "RdBu")[3]),
#                           textInputRow(inputId="Breaks0", label="", value=0, brewer.pal(7, "RdBu")[4]),
#                           textInputRow(inputId="Breaks1", label="", value=0.05, brewer.pal(7, "RdBu")[5]),
#                           textInputRow(inputId="Breaks2", label="", value=0.5, brewer.pal(7, "RdBu")[6]),
#                           textInputRow(inputId="Breaks3", label="", value=2, brewer.pal(7, "RdBu")[7]),
#                           actionButton("Symbolize", "Symbolize"),
#                           plotOutput("hist_diff", width=600, height=200)
                          #dataTableOutput("difftable")
                          
                        )
                      )
             ),
#              tabPanel("Compare Maps",
# #                       sidebarLayout(
# #                         sidebarPanel(
# #                           selectInput("Baseline", label="Choose Baseline Results", choices=getdir(), selected=NULL),
# #                           selectInput("Scenario", label="Choose Scenario Results", choices=getdir(), selected=NULL),
# #                           actionButton("Difference", "Upload Results"),
# #                           tags$br(),
# #                           tags$br(),
# #                           uiOutput("diffnames"),
# #                           tags$br(),
# #                           actionButton("diffcalc", "Calculate Differences")
# #                         ),
#                         mainPanel("main",
#                           uiOutput("mapcompare")
#                         )
#                       #)
#              ),
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

library(leaflet)
library(shiny)
library(shinycssloaders)

navbarPage("Cyclone Track Visalizer", id="nav",
           
           tabPanel("Cyclone Track",
                    
                    div(class="outer",
                        tags$head(
                           #CSS and JS
                           includeCSS("www/style.css"),
                           includeScript("www/global.js")
                        ),#tag$head()
                    leafletOutput("map", width = "100%", height = "100%"),
                    absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE, top = 60, left = "auto", 
                                  right = 20, bottom = "auto", width = 300, height = "auto",
                                  h2("Explore Cyclone"),
                                  
                                  selectInput("region",
                                              label = "Cyclone Region:",
                                              choices = list("Select Cyclone Region" = "r",
                                                             "Atlantic" = "atlantic", 
                                                             "East Pacific" = "e_pacific", "West Pacific" = "w_pacific", 
                                                             "South Pacific" = "s_pacific", "South Indian" = "s_indian", 
                                                             "North Indian" = "n_indian"),
                                              selected = "atlantic"),#selectInput
                                  
                                  textOutput("c_region"),
                                  
                                  selectInput("year", 
                                              label = "Cyclone Year:",
                                              choices = list("Select Cyclone Year" = "y","2014", "2015", "2016", "2017", "2018"),
                                              selected = "2014"),#selectInput
                                  
                                  textOutput("c_year"),
                                  withSpinner(uiOutput("cycloneSelect"), size = 0.25, proxy.height = 100)
                                  # uiOutput("s_url")
                                  
                       
                    )#absolutePanel
                    
                    )#div()
                    ),#tabPanel()
           tabPanel("Data Table",
                    DT::dataTableOutput("cyclonedatatable")
                    )#tabPanel2()
           )#navbarPage
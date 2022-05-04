## Project: Mass Mobilization Analytics/Visualization
## Date: 7/29/2021
## Author: Almira Sadykova
## Requires: server.R, allyears_git_9019.csv,region_git_9019.csv,country_git_9019.csv


library(shiny)
library(ggplot2)
library(leaflet)
library(leaflet.extras)



##  Set working directory and load in the data

##  Loading data from the repository
url1 <- "https://raw.githubusercontent.com/MassMobilization/MM-Analytics/main/allyears_git_9019.csv"
pdata <- read.csv(url1)

url2 <- "https://raw.githubusercontent.com/MassMobilization/MM-Analytics/main/region_git_9019.csv"
mm <- read.csv(url2)

url3 <- "https://raw.githubusercontent.com/MassMobilization/MM-Analytics/main/country_git_9019.csv"
mm_c <- read.csv(url3)


navbarPage("Mass Mobilization Analytics",
           
           tabPanel("World Trends",
                    
                    sidebarLayout(
                      sidebarPanel(
                        selectInput("yearworld","Choose year:",
                                    choices=as.character(unique(pdata$year))
                        
                      ),#selectInput ends
                      downloadButton("downloadWorldData", "Download these data"),
                      helpText("Note: Downloads user-selected year.")
                      ), #sidearPanel ends
                      mainPanel(leafletOutput(outputId = "distPlot")) #mainPanel ends
                      
                    )#sidebarlayout ends
                    
                   
                    ), #World trends end
           tabPanel("Regional Trends",
                    
                    sidebarLayout(
                      sidebarPanel(##Select which region you want
                        selectInput("region", "Choose region:",
                                    choices=as.character(unique(mm$region)),
                                    selected="South America"), #selectInput ends
                        
                        sliderInput("yearslider", "Choose Year",
                                    min = 1990, max = 2019, 
                                    value = c(1990, 2019),
                                    sep=""), #sliderInput ends
                        
                        downloadButton('downloadData', "Download these data"),
                        helpText("Note: Downloads user-selected region and year range.")
                      ), #sidebarPanel ends
                      mainPanel(
                        tabsetPanel(
                          tabPanel("Protest Duration",
                                   plotOutput("tis")), #time in streets end
                          tabPanel("Demands", plotOutput("typedemand"),
                                   ##
                                   ##  Select which demand you want to look at
                                   ##
                                   selectInput("demand", "Type of Demand:",
                                               c("Land" = 'land',
                                                 "Labor" = 'labor',
                                                 "Police Brutality" = 'police',
                                                 "Political" = 'political',
                                                 "Prices" = 'prices',
                                                 "Remove Politician" = 'remove',
                                                 "Social Restrictions" = 'social')), #selectInput ends
                                   
                                   align="center"), #demands end
                          tabPanel("Violence", plotOutput ("typeviolence"),
                                   ##
                                   ##  Select which type of violence you want to look at
                                   ##
                                   selectInput("tvio", "Type of Violence:",
                                               c("Nonviolent" = 'nonviolent',
                                                 "Protester Violence" = 'protester_violence',
                                                 "State Violence" = 'state_violence',
                                                 "Both Violence" = 'both_violence')),#selectInput ends
                                   align="center"
                                   
                                   ) # violence ends
                        )#tabsetPanel ends
                      )#mainPanel ends
                    )#sidebarLayout ends
                    ), #Regional trends end
           tabPanel("Country Trends",
                    
                    sidebarLayout(
                      sidebarPanel(##Select which region you want
                        selectInput("country", "Choose country:",
                                    choices=as.character(unique(mm_c$country)),
                                    selected="Afghanistan"), #selectInput ends
                        sliderInput("countryyearslider", "Choose year",
                                    min=1990, max=2019,
                                    value=c(1990, 2019),
                                    sep=""),#sliderInput ends
                        downloadButton('downloadContryData', "Download these data"),
                        helpText("Note: Downloads user-selected country and year range.")
                      ),#sidebarPanel ends
                      mainPanel(
                        tabsetPanel(
                          tabPanel("Protest Duration",
                                   plotOutput("country_tis")), #time in streets end
                          tabPanel("Demands", plotOutput("country_typedemand"),
                                   ##
                                   ##  Select which demand you want to look at
                                   ##
                                   selectInput("country_demand", "Type of Demand:",
                                               c("Land" = 'land',
                                                 "Labor" = 'labor',
                                                 "Police Brutality" = 'police',
                                                 "Political" = 'political',
                                                 "Prices" = 'prices',
                                                 "Remove Politician" = 'remove',
                                                 "Social Restrictions" = 'social')), #selectInput ends
                                   
                                   align="center"), #demands end
                          tabPanel("Violence", plotOutput ("country_typeviolence"),
                                   ##
                                   ##  Select which type of violence you want to look at
                                   ##
                                   selectInput("country_tvio", "Type of Violence:",
                                               c("Nonviolent" = 'nonviolent',
                                                 "Protester Violence" = 'protester_violence',
                                                 "State Violence" = 'state_violence',
                                                 "Both Violence" = 'both_violence')),#selectInput ends
                                   align="center"
                                   
                          ) # violence ends
                        )#tabsetPanel ends
                        )#mainPanel ends
                    ) #sidebarLayout ends
                    
                    ) #Country trends end
           
           
           )#navbarPage ends

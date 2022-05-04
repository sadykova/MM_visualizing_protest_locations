## Project: Mass Mobilization Analytics/Visualization
## Date: 7/29/2021
## Author: Almira Sadykova
## Requires: ui.R, allyears_git_9019.csv,region_git_9019.csv,country_git_9019.csv

library(shiny)
library(ggplot2)
library(leaflet)
library(leaflet.extras)



##  Loading data from the repository
url1 <- "https://raw.githubusercontent.com/MassMobilization/MM-Analytics/main/allyears_git_9019.csv"
pdata <- read.csv(url1)

url2 <- "https://raw.githubusercontent.com/MassMobilization/MM-Analytics/main/region_git_9019.csv"
mm <- read.csv(url2)

url3 <- "https://raw.githubusercontent.com/MassMobilization/MM-Analytics/main/country_git_9019.csv"
mm_c <- read.csv(url3)

pdata$Latitude0<-as.numeric(pdata$Latitude0)
pdata$Longitude0<-as.numeric(pdata$Longitude0)


## 
##  Custom ggplot2 theme
##
theme_set(theme_classic())
custom <- theme_update(axis.text.x = element_text(colour="black", size=15),
                       axis.text.y = element_text(colour="black", size=15),
                       axis.title.x = element_text(size=20),
                       axis.title.y = element_text(size=20, angle=90),
                       title = element_text(size=20),
                       panel.grid = element_line(colour = NULL, linetype = 1), 
                       panel.grid.major = element_line(colour = "gray78"),
                       panel.grid.major.x = element_blank(), 
                       panel.grid.minor = element_blank()
)  



# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
#######################WORLD TRENDS##################################
    
    output$distPlot<-renderLeaflet({
        ptemp<-subset(pdata, pdata$year==input$yearworld)
        
        # Create a color palette with handmade bins.
        mybins <- seq(1, 65, by=10)
        mypalette <- colorBin( palette="Accent", domain=ptemp$freq, na.color="transparent", bins=mybins)
        
        # Prepare the text for the tooltip:
        mytext <- paste(
          "Latitude: ", ptemp$Latitude0, "Longitude: ", ptemp$Longitude0,  "Name: ", ptemp$X0, "Frequency: ", ptemp$freq,  sep=" ") %>%
          lapply(htmltools::HTML)
        
        
        # Final Map
        m <- leaflet(ptemp, options= leafletOptions(minZoom = -1)) %>% 
          addTiles() %>%
          addProviderTiles(providers$Esri.WorldStreetMap, group = "Esri World Streetmap") %>%
          addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
          addLayersControl(baseGroups = c( "Esri World Streetmap","Toner Lite")) %>%
          #addProviderTiles(providers$Jawg.Terrain,options = providerTileOptions(noWrap = FALSE))%>%
          setView(lat = 0, lng = 0, zoom = 1.25) %>%
          addCircleMarkers(~Longitude0, ~Latitude0, 
                           fillColor = ~mypalette(freq), fillOpacity = 0.7, color="white", radius=4, stroke=FALSE,
                           label = mytext,
                           labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
          ) %>%
          addLegend( pal=mypalette, values=~freq, opacity=0.9, title = "Frequency", position = "bottomright" )
        
        ##  Print out the plot to screen
        ##
        print(m)
        
    })
    
    
    
#######################REGIONAL TRENDS###############################   
    ####Protest Duration
    
    output$tis <- renderPlot({
        
        temp1 <- subset(mm, 
                        mm$region == input$region & mm$year %in% seq(input$yearslider[1], 
                                                                     input$yearslider[2], 1))
        
        tis<-ggplot(temp1, aes(x=year, y=protestdays)) +
          geom_bar(stat="identity")+ xlab("Years") + ylab("Protest Days")+
            ggtitle(paste("Protest Duration", "\n", "(", 
                          paste(input$region, ":", sep=""), 
                          input$yearslider[1], "-", 
                          input$yearslider[2], ")"))
        
        print(tis)
        
    }) #renderPlot duration ends

    ### Protest demands
    
    output$typedemand <- renderPlot({   
        ##
        ##  Subset out the data that the user selects in the app
        ##
        temp1 <- subset(mm, 
                        mm$region == input$region & mm$year %in% seq(input$yearslider[1], 
                                                                         input$yearslider[2], 1))
        
    
        ##  Land  
        ##
        if(input$demand=='land'){
            md <- ggplot(temp1, aes(x=year, y=demand_land)) +
              geom_bar(stat="identity")+ xlab("Years") + ylab("Frequency")+
                ggtitle(paste("Land Protest", "\n", "(", paste(input$region, ":", sep=""), 
                              input$yearslider[1], "-", 
                              input$yearslider[2], ")")) 
            
        }
        
        ##
        ##  Labor
        ##
        if(input$demand=='labor'){
            md<-ggplot(temp1, aes(x=year, y=demand_labor)) +
              geom_bar(stat="identity")+ xlab("Years") + ylab("Frequency")+
                ggtitle(paste("Labor Protest", "\n", "(", 
                              paste(input$region, ":", sep=""), 
                              input$yearslider[1], "-", 
                              input$yearslider[2], ")")) 
            
        }
        
        ##
        ##  Police
        ##
        if(input$demand=='police'){
            md <-ggplot(temp1, aes(x=year, y=demand_policebrutality)) +
              geom_bar(stat="identity")+ xlab("Years") + ylab("Frequency")+
                ggtitle(paste("Police Brutality Protest", "\n", "(", 
                              paste(input$region, ":", sep=""), 
                              input$yearslider[1], "-", 
                              input$yearslider[2], ")")) 
        }
        
        ##
        ##  Political
        ##
        if(input$demand=='political'){
            md <- ggplot(temp1, aes(x=year, y=demand_political)) +
              geom_bar(stat="identity")+ xlab("Years") + ylab("Frequency")+
                ggtitle(paste("Political Behavior, Process Protest", "\n", "(", 
                              paste(input$region, ":", sep=""), 
                              input$yearslider[1], "-", 
                              input$yearslider[2], ")")) 
            
        }
        
        ##
        ##  Prices
        ##
        if(input$demand=='prices'){
            md <- ggplot(temp1, aes(x=year, y=demand_price)) +
              geom_bar(stat="identity")+ xlab("Years") + ylab("Frequency") +
                ggtitle(paste("Price and Wage Dispute Protest", "\n", "(", 
                              paste(input$region, ":", sep=""), 
                              input$yearslider[1], "-", 
                              input$yearslider[2], ")")) 
        }
        
        ##
        ##  Remove
        ##
        if(input$demand=='remove'){
            md <- ggplot(temp1, aes(x=year, y=demand_removal)) +
              geom_bar(stat="identity")+ xlab("Years") + ylab("Frequency") +
                ggtitle(paste("Removal of Politician Protest", "\n", "(", 
                              paste(input$region, ":", sep=""), 
                              input$yearslider[1], "-", 
                              input$yearslider[2], ")")) 
        }
        
        ##
        ##  Social
        ##
        if(input$demand=='social'){
            md <- ggplot(temp1, aes(x=year, y=demand_social)) +
              geom_bar(stat="identity")+ xlab("Years") + ylab("Frequency") +
                ggtitle(paste("Social Restrictions Protest", "\n", "(", 
                              paste(input$region, ":", sep=""), 
                              input$yearslider[1], "-", 
                              input$yearslider[2], ")")) 
        }
        
        ##
        ##  Prints out the plot
        ##
        print(md)
        
        }) #renderPlot type of demand ends  
    
    ##########################################################
    ######                VIOLENCE                      ######
    ##########################################################  
    
    output$typeviolence <- renderPlot({ 
        
        
        ##
        ##  Subset out the data that the user selects in the app
        ##
        temp1 <- subset(mm, 
                        mm$region == input$region & mm$year %in% seq(input$yearslider[1], 
                                                                         input$yearslider[2], 1))
        
        
      
        ##
        ##  Nonviolent
        ##
        if(input$tvio=='nonviolent'){
            vio <-  ggplot(temp1, aes(x=year, y=nonviolent)) +geom_bar(stat="identity")+ xlab("Years") + ylab("Frequency") +
                ggtitle(paste("Nonviolent Protests", "\n", "(", 
                              paste(input$region, ":", sep=""), 
                              input$yearslider[1], "-", 
                              input$yearslider[2], ")")) 
        }
        
        ##
        ##  Protester Violence
        ##
        if(input$tvio=='protester_violence'){
            vio <- ggplot(temp1, aes(x=year, y=protesterviolence)) +geom_bar(stat="identity")+ xlab("Years") + ylab("Frequency") + 
                ggtitle(paste("Protester Violence", "\n", "(", 
                              paste(input$region, ":", sep=""), 
                              input$yearslider[1], "-", 
                              input$yearslider[2], ")"))  
        }
        
        ##
        ##  State Violence
        ##
        if(input$tvio=='state_violence'){
            vio <-ggplot(temp1, aes(x=year, y=state_violence)) +geom_bar(stat="identity")+ xlab("Years") + ylab("Frequency")  +
                ggtitle(paste("State Violence", "\n", "(", 
                              paste(input$region, ":", sep=""), 
                              input$yearslider[1], "-", 
                              input$yearslider[2], ")"))     
        }
        
        ##
        ##  Both Violence
        ##
        if(input$tvio=='both_violence'){
            vio <-ggplot(temp1, aes(x=year, y=state_violence)) +geom_bar(stat="identity")+ xlab("Years")  +ylab("Frequency")+
                ggtitle(paste("Protester and State Violence", "\n", "(", 
                              paste(input$region, ":", sep=""), 
                              input$yearslider[1], "-", 
                              input$yearslider[2], ")")) 
        }
        
        
        ##
        ##  Prints out the plot
        ##
        print(vio) })  # renderPlot for Violence ends 
    
    ##
    ##  Download these data
    ##
    output$downloadData <- downloadHandler(
        filename = function() { paste(input$region, "_", 
                                      input$yearslider[1], "-", 
                                      input$yearslider[2], 
                                      '.csv', 
                                      sep='') },
        content = function(file) {
            write.csv(subset(mm, mm$region == input$region), file)
        }
    )

###################################################################    
###########################COUNTRY TRENDS##########################
    
    
    ####Protest Duration
    
    output$country_tis <- renderPlot({
        
        temp1_c <- subset(mm_c, 
                        mm_c$country == input$country & mm_c$year %in% seq(input$countryyearslider[1], 
                                                                     input$countryyearslider[2], 1))
        
        tis_c<-ggplot(temp1_c, aes(x=year, y=protestdays)) +
            geom_bar(stat="identity")+ xlab("Years") + ylab("Protest Days")+
            ggtitle(paste("Protest Duration", "\n", "(", 
                          paste(input$country, ":", sep=""), 
                          input$countryyearslider[1], "-", 
                          input$countryyearslider[2], ")"))
        
        print(tis_c)
        
    }) #renderPlot duration ends
    
    ### Protest demands
    
    output$country_typedemand <- renderPlot({   
        ##
        ##  Subset out the data that the user selects in the app
        ##
        temp1_c <- subset(mm_c, 
                        mm_c$country == input$country & mm_c$year %in% seq(input$countryyearslider[1], 
                                                                     input$countryyearslider[2], 1))
        
        
        ##  Land  
        ##
        if(input$country_demand=='land'){
            md_c <- ggplot(temp1_c, aes(x=year, y=demand_land)) +
              geom_bar(stat="identity")+ xlab("Years") + ylab("Frequency")+
                ggtitle(paste("Land Protest", "\n", "(", paste(input$country, ":", sep=""), 
                              input$countryyearslider[1], "-", 
                              input$countryyearslider[2], ")")) 
            
        }
        
        ##
        ##  Labor
        ##
        if(input$country_demand=='labor'){
            md_c<-ggplot(temp1_c, aes(x=year, y=demand_labor)) +
              geom_bar(stat="identity")+ xlab("Years") + ylab("Frequency")+
                ggtitle(paste("Labor Protest", "\n", "(", 
                              paste(input$country, ":", sep=""), 
                              input$countryyearslider[1], "-", 
                              input$countryyearslider[2], ")")) 
            
        }
        
        ##
        ##  Police
        ##
        if(input$country_demand=='police'){
            md_c <-ggplot(temp1_c, aes(x=year, y=demand_policebrutality)) +
              geom_bar(stat="identity")+ xlab("Years") + ylab("Frequency")+
                ggtitle(paste("Police Brutality Protest", "\n", "(", 
                              paste(input$country, ":", sep=""), 
                              input$countryyearslider[1], "-", 
                              input$countryyearslider[2], ")")) 
        }
        
        ##
        ##  Political
        ##
        if(input$country_demand=='political'){
            md_c <- ggplot(temp1_c, aes(x=year, y=demand_political)) +
              geom_bar(stat="identity")+ xlab("Years") + ylab("Frequency")+
                ggtitle(paste("Political Behavior, Process Protest", "\n", "(", 
                              paste(input$country, ":", sep=""), 
                              input$countryyearslider[1], "-", 
                              input$countryyearslider[2], ")")) 
            
        }
        
        ##
        ##  Prices
        ##
        if(input$country_demand=='prices'){
            md_c <- ggplot(temp1_c, aes(x=year, y=demand_price)) +
              geom_bar(stat="identity")+ xlab("Years") + ylab("Frequency") +
                ggtitle(paste("Price and Wage Dispute Protest", "\n", "(", 
                              paste(input$country, ":", sep=""), 
                              input$countryyearslider[1], "-", 
                              input$countryyearslider[2], ")")) 
        }
        
        ##
        ##  Remove
        ##
        if(input$country_demand=='remove'){
            md_c <- ggplot(temp1_c, aes(x=year, y=demand_removal)) +
              geom_bar(stat="identity")+ xlab("Years") + ylab("Frequency") +
                ggtitle(paste("Removal of Politician Protest", "\n", "(", 
                              paste(input$country, ":", sep=""), 
                              input$countryyearslider[1], "-", 
                              input$countryyearslider[2], ")")) 
        }
        
        ##
        ##  Social
        ##
        if(input$country_demand=='social'){
            md_c <- ggplot(temp1_c, aes(x=year, y=demand_social)) +
              geom_bar(stat="identity")+ xlab("Years") + ylab("Frequency") +
                ggtitle(paste("Social Restrictions Protest", "\n", "(", 
                              paste(input$country, ":", sep=""), 
                              input$countryyearslider[1], "-", 
                              input$countryyearslider[2], ")")) 
        }
        
        ##
        ##  Prints out the plot
        ##
        print(md_c)
        
    }) #renderPlot type of demand ends  
    
    ##########################################################
    ######                VIOLENCE                      ######
    ##########################################################  
    
   
    output$country_typeviolence<-renderPlot({
        
        temp1_c<-subset(mm_c, 
                        mm_c$country == input$country & mm_c$year %in% seq(input$countryyearslider[1], 
                                                                         input$countryyearslider[2], 1))
        ##
        ##  Nonviolent
        ##
        if(input$country_tvio=='nonviolent'){
            vio_c <-  ggplot(temp1_c, aes(x=year, y=nonviolent)) + geom_bar(stat="identity")+ xlab("Years") + ylab("Frequency") +
                ggtitle(paste("Nonviolent Protests", "\n", "(", 
                              paste(input$country, ":", sep=""), 
                              input$countryyearslider[1], "-", 
                              input$countryyearslider[2], ")")) }
        
        ##  Protester Violence
        ##
        if(input$country_tvio=='protester_violence'){
            vio_c <-  ggplot(temp1_c, aes(x=year, y=protesterviolence)) + geom_bar(stat="identity")+ xlab("Years") + ylab("Frequency") +
                ggtitle(paste("Protests Violence", "\n", "(", 
                              paste(input$country, ":", sep=""), 
                              input$countryyearslider[1], "-", 
                              input$countryyearslider[2], ")")) }
        
        ##  State Violence
        ##
        if(input$country_tvio=='state_violence'){
            vio_c <-  ggplot(temp1_c, aes(x=year, y=state_violence)) + geom_bar(stat="identity")+ xlab("Years") + ylab("Frequency") +
                ggtitle(paste("State Violence", "\n", "(", 
                              paste(input$country, ":", sep=""), 
                              input$countryyearslider[1], "-", 
                              input$countryyearslider[2], ")")) }
        
        ##  Both Violence
        ##
        if(input$country_tvio=='both_violence'){
            vio_c <-  ggplot(temp1_c, aes(x=year, y=state_violence)) + geom_bar(stat="identity")+ xlab("Years") + ylab("Frequency") +
                ggtitle(paste("Protester and State Violence", "\n", "(", 
                              paste(input$country, ":", sep=""), 
                              input$countryyearslider[1], "-", 
                              input$countryyearslider[2], ")")) }
        
            ##
            ##  Prints out the plot
            ##
            print(vio_c)
        
            })
        

    output$downloadContryData <- downloadHandler(
        filename = function() { paste(input$country, "_", 
                                      input$countryyearslider[1], "-", 
                                      input$countryyearslider[2], 
                                      '.csv', 
                                      sep='') },
        content = function(file) {
            write.csv(subset(mm_c, mm_c$country == input$country), file)
        }
    ) #for country trends download
    
    output$downloadWorldData <- downloadHandler(
      filename = function() { paste(input$yearworld,
                                    '.csv', 
                                    sep='') },
      content = function(file) {
        write.csv(subset(pdata, pdata$year == input$yearworld), file)
      }
    ) #for country trends download
    
    
})#Shiny server ends



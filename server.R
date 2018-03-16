library(xml2)
library(plyr)
library(leaflet)
library(stringi)
library(htmltools)
library(htmlwidgets)
library(RColorBrewer)
library(rvest)

ss <-  c("Tropical Depression", "Tropical Storm", "Hurricane-1", "Hurricane-2", "Hurricane-3", "Hurricane-4", "Hurricane-5")
pal <- colorRampPalette(c("blue", "green", "yellow", "orange", "red", "darkred", "black"))(length(ss))

function(input, output, session) {
   
   data_url <- reactive({
      req(input$region)
      req(input$year)
      url_region <- input$region
      url_year <- input$year
      #as.character(trackname)
      paste("http://weather.unisys.com/hurricane",url_region,url_year, sep = "/")
   })
   
   data_tracks <- reactive({
      html <- read_html(data_url())
      links <- html_attr(html_nodes(html, "a"), "href")
      links <- links[grep('track.dat', links)]
      track <- list(links)
      names(track) <- c("cyclonenames")
      trackname <- track$cyclonenames
      # url <- paste("http://weather.unisys.com/hurricane",url_region,url_year,"index.php", sep = "/")
   })
   
   output$cycloneSelect <- renderUI({
      selectInput(inputId = "sname",
                  label = "Cyclone Name:",
                  choices = data_tracks()
                  , selected = NULL)
   })
   

   selection <- reactive({
      input$sname
      # updateSelectInput(session,
      #                   inputId = "sname",
      #                   choices = data_tracks(),
      #                   selected = character(0))
   })

   cyclone_track <- reactive ({
      #working list = url <- paste(data_url(),data_tracks(), sep = "/")
      # req(input$sname)
      url <- paste(data_url(),selection(), sep = "/")
      storm <- readLines(url)
      storm <- read.table(textConnection(gsub("TROPICAL ", "TROPICAL_", storm[3:length(storm)])), header=TRUE, stringsAsFactors=FALSE)
      # make storm type names prettier
      storm$STAT <- stri_trans_totitle(gsub("_", " ", storm$STAT))
      
      # make column names prettier
      colnames(storm) <- c("advisory", "lat", "lon", "time", "wind_speed", "pressure", "status")
      # Storm scale
      # ss <-  c("Tropical Depression", "Tropical Storm", "Hurricane-1", "Hurricane-2", "Hurricane-3", "Hurricane-4", "Hurricane-5")
      # pal <- colorRampPalette(c("blue", "green", "yellow", "orange", "red", "darkred", "black"))(length(ss))
      
      storm$color <- as.character(factor(storm$status, levels = ss, labels = pal))
      # lighten past position colors
      storm$opacity <- 0.8
      storm$opacity[strptime(storm$time, format="%m/%d/%H") <= Sys.time()] <- 0.1
      
      # make windspeeds useful for point sizes
      storm$wind_speed[storm$wind_speed == "-"] <- 0
      storm$wind_speed <- as.integer(storm$wind_speed)
      storm$wind_speed[is.na(storm$wind_speed)] <- 0
      
      storm$time <- gsub("Z", "", storm$time)
      storm$date <- strftime(strptime(storm$time, format="%m/%d/%H"), '%m/%d %Hh')
      
      # separate complete and intermediate advisories (assuming they come in pairs - TODO)
      storm$adv <- gsub("A", "", storm$advisory)
      storm <- ddply(storm, "adv", head, 1)
      storm <- storm[order(storm$date),]
      
      storm$time <- as.POSIXct(storm$time, format='%m/%d/%H', tz="UTC")
      storm$localtime <- as.POSIXct(format(storm$time, tz=Sys.timezone(), usetz = TRUE))
      storm$day <- paste(weekdays(storm$localtime, abbreviate = TRUE), format(storm$localtime, "%H:%M"), " ")
      storm
   })
   
   # output$s_url <- renderText({
   #    cyclone_track()
   #    # selection()
   # })

   

   # Create the map
   output$map <- renderLeaflet({
         # Create leaflet map.
          
         leaflet(cyclone_track()) %>%
         
         addTiles(group = "OSM (default)") %>%
         addProviderTiles(providers$Esri.WorldImagery, group = "Esri World Imagery") %>%
         addProviderTiles(providers$OpenStreetMap.HOT, group = "HOT OSM") %>%
         addProviderTiles(providers$NASAGIBS.ModisTerraTrueColorCR, group = "NASA GIBS") %>%
         
         addPolylines(~lon, ~lat, color = 'grey', weight=3) %>%
         
         addCircleMarkers(~lon, ~lat, radius = ~wind_speed / 3, stroke = TRUE, color = 'grey',
                          opacity = 1, weight = 2, fill = true, fillColor = ~color,
                          fillOpacity = ~opacity,
                          popup = ~sprintf("<b>Advisory forecast %s (%s)</b><hr noshade size='1'/>
                                           Local time: %s<br/>
                                           Position: %3.2f, %3.2f<br/>
                                           Strength: <strong>%s</strong><br/>
                                           Wind: %s (knots)<br/>Pressure: %s",
                                           htmlEscape(advisory), htmlEscape(date), htmlEscape(format(localtime, "%b %d %H:%M")),
                                           htmlEscape(lon), htmlEscape(lat),
                                           htmlEscape(status), htmlEscape(wind_speed), htmlEscape(pressure))
         ) %>%
         addLegend("bottomright", colors = pal, labels = ss) %>%
         
         addLayersControl(
            baseGroups = c("OSM (default)", "Esri World Imagery", "HOT OSM", "NASA GIBS"),
            options = layersControlOptions(),
            position = "topleft"
         )
      
      #setView(lng = 90.85, lat = 8.45, zoom = 3)
      
   }) #renderLeaflet
}
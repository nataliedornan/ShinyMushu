
library(tidyverse)
library(shiny)
library(shinythemes)
library(RColorBrewer)
library(maps)
library(mapproj)
library(leaflet)
library(sf)
library(tmap)
library(tmaptools) # ADDED THIS
library(ggrepel)
library(ggspatial)
library(raster)

# load the data here
# as long as your selections have the exact same notation as the columns,
# if this is the same as you list in your widget lists
# in the widgets (in the ui), need to call them 
# in the server, you create a new reaction that will call "input$location" from the ui

# use read.csv?

HAB <- read.csv("clean_hab.csv", stringsAsFactors = F)


#### FROM MAP #####


#Load the California County sf
ca_counties <- read_sf(".", layer = "california_county_shape_file")



#Load Coast Counties
coast_counties <- ca_counties %>%
  filter(NAME %in% c("San Luis Obispo", "Santa Barbara", "Ventura", "Los Angeles", "Orange", "San Diego"))

st_crs(coast_counties) = 4326


#Load clean_hab as sf
sites_hab <- st_as_sf(HAB, coords = c("longitude", "latitude"), crs = 4326)

###### FROM MAP ABOVE #####

###### FROM APP_ABUN ######

# variables <- clean_hab %>% 
#   dplyr::select(year,
#                 month,
#                 location,
#                 akashiwo,
#                 alexandrium,
#                 ammonia,
#                 chlorophyll,
#                 domoic_acid,
#                 phosphate,
#                 silicate,
#                 water_temp,
#                 n_n,
#                 pseudo_nitzschia_spp)


# # Allows me to call this in the widget without listing it out
# year <- unique(variables$year)
# location <- unique(variables$location)
# akashiwo <- unique(variables$akashiwo)
# alexandrium <- unique(variables$alexandrium)
# ammonia <- unique(variables$ammonia)
# chlorophyll <- unique(variables$chlorophyll)
# domoic_acid <- unique(variables$domoic_acid)
# phosphate <- unique(variables$phosphate)
# silicate <- unique(variables$silicate) 
# water_temp <- unique(variables$water_temp)
# n_n <- unique(variables$n_n)
# pseudo_nitzschia_spp <- unique(variables$pseudo_nitzschia_spp)

# Define UI for application that has tabs (navbarPage function)
# Apply a shinytheme
ui <- navbarPage(theme = shinytheme("superhero"),
                 title = "Harmful Algal Blooms along the California Coast (2008-2018)",
                 
                 tabsetPanel(
                   # Create Summary tab
                   tabPanel(title = "Summary",
                            sidebarLayout(
                              # Create sidebar panel containing information about the app
                              sidebarPanel(
                                h4("App Summary"),
                                p("This app visually explores spatial and temporal distributions of harmful algal blooms (HABs) at various research stations located in California coastal waters between the years of 2008 and 2018."),
                                p("This app allows the user to look at the data graphically and spatially over time, to understand the role different environmental factors have in HAB occurrence over the past decade."),
                                hr(),
                                h4("Data"),
                                p("The data used for the app was sourced from an open access database courtesy of the Southern California Coastal Ocean Observing System (SCCOOS), found",
                                  a(href = "http://www.sccoos.org/query/", "here."),
                                  "The objective of SCCOOS is to collect data at six stations to better understand the environmental factors surrounding HABs, as well as their impacts on marine ecosystems, ecosystem services, and human livelihood. Samples for this project are collected weekly at each station via water grabs and net tows. Parameters collected include latitude, longitude, algal toxins, temperature, salinity, and nutrients.")
                                
                              ),
                              
                              # Main panel displays red tide image
                              mainPanel(
                                img(src = "red_tide.jpg"),
                                p("Red algal bloom at Leigh, near Cape Rodney.",
                                  em("Photo: SERC Carleton College."))
                              )
                            )
                   ),
                   
                   # Create tab for HAB Abundance Chart
                   tabPanel(title = "HAB Abundance Chart",
                            # Move sidebar containing widgets to right side of screen
                            sidebarLayout(position = "right",
                                          sidebarPanel(
                                            # Create select widget for monitoring locations
                                            selectInput("selectlocation_abun",
                                                        label = h4("Monitoring Location"),
                                                        choices = list("Cal Poly Pier" = "Cal Poly Pier", 
                                                                       "Goleta Pier" = "Goleta Pier", 
                                                                       "Stearns Wharf" = "Stearns Wharf", 
                                                                       "Santa Monica Pier" = "Santa Monica Pier", 
                                                                       "Newport Pier" = "Newport Pier", 
                                                                       "Scripps Pier" = "Scripps Pier"),
                                                        selected = 1),
                                            
                                            # Create select widget for year
                                            selectInput("selectyear_abun",
                                                        label = h4("Year"),
                                                        choices = c(2008:2018), 
                                                        selected = 1),
                                            
                                            # Create select widget for variable
                                            selectInput("selectvar_abun",
                                                        label = h4("Variable"),
                                                        choices = list("Akashiwo sp." = "akashiwo", 
                                                                       "Alexandrium spp." = "alexandrium", 
                                                                       "Ammonia" = "ammonia", 
                                                                       "Chlorophyll" = "chlorophyll", 
                                                                       "Domoic Acid" = "domoic_acid", 
                                                                       "N+N" = "n_n", 
                                                                       "Phosphate" = "phosphate",
                                                                       "Pseudo Nitzschia spp." = "pseudo_nitzschia_spp",
                                                                       "Silicate" = "silicate", 
                                                                       "Water Temp" = "water_temp"), 
                                                        selected = 1)
                                            
                                          ),
                                          
                                          # Show a plot ("abunPlot") of the generated distribution of HAB variables in the main panel
                                          mainPanel(
                                            plotOutput("abunPlot"))
                                          
                            )
                            
                   ),
                   
                   
                   # Create tab for Correlation Plot
                   tabPanel(title = "Correlation Plot"),
                   
                   
                   
                   # Create tab for Interactive Map
                   tabPanel(title = "Interactive Map",
                            # Sidebar with a slider, radio, and select inputs 
                            sidebarLayout(
                              sidebarPanel(
                                radioButtons("radioyear_map", 
                                             label = "Year:",
                                             choices = c(2008:2018), 
                                             selected = 1), 
                                sliderInput("slidermonth_map",
                                            "Month:",
                                            min = 1,
                                            max = 12,
                                            value = 1),
                                
                                selectInput("selectvariable_map",
                                            label = "Choose a HAB Variable:",
                                            choices = list("Akashiwo sp." = "akashiwo", 
                                                           "Alexandrium spp." = "alexandrium", 
                                                           "Ammonia" = "ammonia", 
                                                           "Chlorophyll" = "chlorophyll", 
                                                           "Domoic Acid" = "domoic_acid", 
                                                           "N+N" = "n_n", 
                                                           "Phosphate" = "phosphate",
                                                           "Pseudo Nitzschia spp." = "pseudo_nitzschia_spp",
                                                           "Silicate" = "silicate", 
                                                           "Water Temp" = "water_temp"),
                                            selected = 1)
                              ),
                              
                              # Show a plot of the generated distribution
                              mainPanel(
                                plotOutput(outputId = "Map")
                                           
                                  )
                                )
                              )
                              
                            )
                            
                            
                            )
                   
                   
                   
                 

#
#
#
########### SERVER ############
# Define server logic required for each tab
server <- function(input, output) {
  
  
  # Create reactive function to isolate reactions related to the HAB abundance chart
  MapInput <- reactive({
    #create new df for filtering input$year, input$month, and select the variables we want,
    map_sites <- sites_hab %>%   
    filter(year == input$radioyear_map,
             month == input$slidermonth_map) %>%
      select(akashiwo,
             alexandrium,
             ammonia,
             chlorophyll,
             domoic_acid,
             n_n,
             phosphate,
             pseudo_nitzschia_spp,
             silicate,
             water_temp,
             geometry)
    
  })
  
  #clean_hab
  # HABgraph <- filter(input$year, input$month, and select the variable)
  # name it something else and then call it
  # could make a separate data frame for color for each input (e.g. green for chlorophyll); when you call it in the plot, you say look at this data frame, if it's input 3, grab row 3 from this column
  
  # HAB_chart <- filter(input$location, input$year)
  
  # Send HAB abundance chart to the ui as "abunPlot"
  output$abunPlot <- renderPlot({
    
    # use print to troubleshoot
    # print(class(input$variable))
    
    # filtered <- HAB %>%
    #   filter(location == input$selectlocation_abun,
    #          year == input$selectyear_abun) %>%
    #   select(month,
    #          akashiwo, 
    #          alexandrium, 
    #          ammonia, 
    #          chlorophyll, 
    #          domoic_acid, 
    #          n_n, 
    #          phosphate,
    #          pseudo_nitzschia_spp,
    #          silicate, 
    #          water_temp)
    
    # filters by location only
    filtered <- HAB %>%
      filter(location == input$selectlocation_abun) %>%
      select(year,
             month,
             akashiwo, 
             alexandrium, 
             ammonia, 
             chlorophyll, 
             domoic_acid, 
             n_n, 
             phosphate,
             pseudo_nitzschia_spp,
             silicate, 
             water_temp)
    
    View(filtered)
    
    # maybe don't allow to select by year, still show month by variable but do facet_wrap and show all the months
    # compare seasonality of these species across all years
    # TIME SERIES GRAPH INSTEAD?? see assignment 3
    
    ggplot(filtered, aes_string(x = "month", y = input$selectvar_abun)) +
      geom_col(fill = "seagreen3", color = "seagreen") +
      facet_wrap(~year, scale = "free") +
      labs(x = "Month", y = "Variable") +
      theme_bw()
    
    # need location (selectlocation_abun), then year (selectyear_abun), then variable (selectvar_abun) (with all months in the year)
    
    # ggplot() +
    #       geom_col(clean_hab, aes(x = input$year, y = input$selectvar_abun)fill = "seagreen3", color = "seagreen") +
    #        labs(x = "Month", y = "Variable") +
    #       theme_bw() 
    
    
    
  })
  
  
######## MAP ##################################################
  
  output$Map <- renderPlot({
    
    ggplot() +
      geom_sf(data = coast_counties, fill = "gray40") +
      geom_sf(data = map_sites, aes(color = input$selectvariable_map), size = 0.3)
    
    # color <- switch(input$selectvariable_map,
    #                 
    #                 "Akashiwo sp." = "red",
    #                 "Alexandrium spp." = "blue",
    #                 "Ammonia" = "purple",
    #                 "Chlorophyll" = "green",
    #                 "Domoic Acid" = "yellow",
    #                 "N+N" = "cyan",
    #                 "Phosphate" = "maroon",
    #                 "Pseudo Nitzschia spp." = "darkolivegreen",
    #                 "Silicate" = "darkseagreen",
    #                 "Water Temp" = "coral" )
    
    
    # ggplot()+
    #   geom_sf(data = coast_counties, fill = "white") +
    #   geom_sf(data = new_hab(), aes_string(fill = input$variable), size = 10) +
    #   #scale_color_manual(values = color) +
    #   theme_classic() +
    #   coord_sf(datum = NA)
    
    # tm_map <- tm_shape(MapInput)+
    #   tm_bubbles(size = input$selectvariable_map) +
    #   tm_shape(coast_counties) +
    #   tm_fill("COUNTY", palette = "Set1", alpha = 0.5, legend.show = FALSE)+
    #   tm_view(basemaps = "Stamen.TerrainBackground")
    # 
    # tmap_mode("view")
    # 
    # tmap_leaflet(tm_map)
    # 
    
    
    
    
    # tmap_mode("view")
    #basemaps in leaflet::providers
    
    
    # leaflet() %>%
    #   addTiles() %>%
    #   addPolygons(data=coast_counties, 
    #               fill = "red") %>% 
    #   addCircleMarkers(data=new_hab$geometry, fillColor = color     )  
    
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)


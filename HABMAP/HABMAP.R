
library(shiny)
library(tidyverse)
library(janitor)
library(RColorBrewer)
library(sf)
library(tmap)
library(sp)
library(spatstat)
library(gstat)
library(raster)
library(maptools)
library(rgeos)
library(leaflet)


clean_hab_map <- read.csv("clean_hab.csv")



#Load the California County sf
ca_counties <- read_sf(".", layer = "california_county_shape_file")



#Load Coast Counties
coast_counties <- ca_counties %>%
  filter(NAME %in% c("San Luis Obispo", "Santa Barbara", "Ventura", "Los Angeles", "Orange", "San Diego"))

st_crs(coast_counties) = 4326


#Load clean_hab as sf
sites_hab <- st_as_sf(clean_hab_map, coords = c("longitude", "latitude"), crs = 4326)

gathered_hab <- sites_hab %>%
  gather(Variable,
         Data,
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


# Define UI for application 
ui <- fluidPage(
   
   # Application title
   titlePanel("HABs of Southern California"),
   
   # Sidebar with a slider, radio, and select inputs 
   sidebarLayout(
      sidebarPanel(
        radioButtons("year",
                     label = "Year:",
                     choices = list("2008" = 1,
                                    "2009" = 2,
                                    "2010" = 3,
                                    "2011" = 4,
                                    "2012" = 5,
                                    "2013" = 6,
                                    "2014" = 7,
                                    "2015" = 8,
                                    "2016" = 9,
                                    "2018" = 10),
                     selected = 1),
         sliderInput("month",
                     "Month:",
                     min = 1,
                     max = 12,
                     value = 1),
          
         selectInput("variable",
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
        tabsetPanel(
          tabPanel("HAB Map",
            plotOutput(outputId = "Map")
            
          )
          )
        )
         
   )
)
#########################################################################
# Define server logic 
server <- function(input, output) {
  
  
  
  
  #create new df for filtering input$year, input$month, and select the variables we want,
  #selected_var <- reactive({
    
    
    
  # 
  #   
  #   
  #   
  #   # %>%
  #   #   select(akashiwo,
  #   #          alexandrium,
  #   #          ammonia,
  #   #          chlorophyll,
  #   #          domoic_acid,
  #   #          n_n,
  #   #          phosphate,
  #   #          pseudo_nitzschia_spp,
  #   #          silicate,
  #   #          water_temp)
  #   # 
  #   
  #   
  # 
  # 
  # 
  # 
    # akashiwo,
    # alexandrium,
    # ammonia,
    # chlorophyll,
    # domoic_acid,
    # n_n,
    # phosphate,
    # pseudo_nitzschia_spp,
    # silicate,
    # water_temp
  # 
  # 
  # 
  # 
  # 
  # })
    
    output$Map <- renderPlot({  
    
      
     selected_var <-  gathered_hab %>%
        filter(year == input$year &
                 month == input$month &
                 Variable == input$variable)
      
      
      mapcolor <- switch(input$variable,

                      "Akashiwo sp." = "red",
                      "Alexandrium spp." = "blue",
                      "Ammonia" = "purple",
                      "Chlorophyll" = "green",
                      "Domoic Acid" = "yellow",
                      "N+N" = "cyan",
                      "Phosphate" = "maroon",
                      "Pseudo Nitzschia spp." = "darkolivegreen",
                      "Silicate" = "darkseagreen",
                      "Water Temp" = "coral" )
      
      
      # if (packageVersion("tmap") >= 2.0) {
    #   tm <- tm_basemap(leaflet::providers$Stamen.TerrainBackground) +
    #     tm_shape(new_hab())+
    #     tm_bubbles(input$variable, col = color, border.col = color) +
    #     tm_shape(coast_counties) +
    #     tm_fill("COUNTY", palette = "Set1", alpha = 0.5, legend.show = FALSE)
    # }
    # else{
    #   tm <- tm_shape(new_hab()) +
    #     tm_bubbles(input$variable)+
    #     tm_view(basemaps = "Stamen.TerrainBackground")
  
    # }
  
      # ggplot(selected_var$geometry)+
      #   geom_sf(data = coast_counties, color = "gray80") +
      #   geom_sf(data = selected_var, aes_string(fill = mapcolor), size = 4) +
      #   theme_minimal()+
      #   coord_sf(datum = NA)
      
      
      

       tm <- tm_shape(selected_var$geometry) +
        tm_bubbles(size = input$variable, col = mapcolor, border.col = mapcolor)+
        tm_shape(coast_counties) +
        tm_fill("COUNTY", palette = "Set1", alpha = 0.5, legend.show = FALSE)+
        tm_view(basemaps = "Stamen.TerrainBackground")



      tmap_mode("view")

      tmap_leaflet(tm)

      

      
      
    
      
     
    }) 
}

# Run the application 
shinyApp(ui = ui, server = server)


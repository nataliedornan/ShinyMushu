
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
HAB <- read.csv("HAB.csv") 

#load the data clean_hab
clean_hab <- HAB %>% 
  clean_names(., case = c("snake")) %>% # Convert to snake case
  dplyr::select(year, month, day, latitude, longitude, location, 
                akashiwo_sanguinea_cells_l, alexandrium_spp_cells_l, 
                ammonia_u_m, chlorophyll_mg_m3, domoic_acid_ng_m_l, 
                nitrate_u_m, nitrite_u_m, phosphate_u_m, 
                pseudo_nitzschia_delicatissima_group_cells_l,
                pseudo_nitzschia_seriata_group_cells_l, 
                silicate_u_m, water_temperature_c) %>% 
  rename(akashiwo = akashiwo_sanguinea_cells_l,
         alexandrium = alexandrium_spp_cells_l,
         ammonia = ammonia_u_m,
         chlorophyll = chlorophyll_mg_m3,
         domoic_acid = domoic_acid_ng_m_l,
         nitrate = nitrate_u_m,
         nitrite = nitrite_u_m,
         phosphate = phosphate_u_m,
         pseudo_nitzschia_delicatissima = pseudo_nitzschia_delicatissima_group_cells_l,
         pseudo_nitzschia_seriata = pseudo_nitzschia_seriata_group_cells_l,
         silicate = silicate_u_m,
         water_temp = water_temperature_c) %>% # Rename variables
  mutate(year_month = str_c(year, month, sep = "-")) %>% # Create one column of year and month of sample together
  mutate("n_n" = nitrate + nitrite) %>%
  mutate(pseudo_nitzschia_spp = pseudo_nitzschia_delicatissima + pseudo_nitzschia_seriata)## sum N+N concentrations



#Load the California County sf
ca_counties <- read_sf(".", layer = "california_county_shape_file")



#Load Coast Counties
coast_counties <- ca_counties %>%
  filter(NAME %in% c("San Luis Obispo", "Santa Barbara", "Ventura", "Los Angeles", "Orange", "San Diego"))

st_crs(coast_counties) = 4326


#Load clean_hab as sf
sites_hab <- st_as_sf(clean_hab, coords = c("longitude", "latitude"), crs = 4326)




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
            leafletOutput(outputId = "Map")
            
          )
          )
        )
         
   )
)
#########################################################################
# Define server logic 
server <- function(input, output) {
  
  
  
  
  #create new df for filtering input$year, input$month, and select the variables we want,
  # new_hab <- reactive({
  # 
  #   #filter_hab <- 
  #     
  #     sites_hab %>%
  #     
  #     filter(year %in% input$year & 
  #            month %in% input$month)
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
    
    output$Map <- renderLeaflet({  
    
      
     select_var <- gathered_hab %>%
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
      
      
      # ggplot()+
      #   geom_sf(data = coast_counties, fill = "white") +
      #   geom_sf(data = new_hab(), aes_string(fill = input$variable), size = 10) +
      #   #scale_color_manual(values = color) +
      #   theme_classic() +
      #   coord_sf(datum = NA)
      
      
      
     
      
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
  
      
      
       tm <- tm_shape(select_var) +
         tm_bubbles(size = input$variable, col = mapcolor, border.col = mapcolor)+
        tm_shape(coast_counties) +
        tm_fill("COUNTY", palette = "Set1", alpha = 0.5, legend.show = FALSE)+
        tm_view(basemaps = "Stamen.TerrainBackground")
      
     
      
      tmap_mode("view")
      
      tmap_leaflet(tm)

      
      
      
      
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


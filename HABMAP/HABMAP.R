# 
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(RColorBrewer)
library(maps)
library(mapproj)
HAB <- read_csv("HAB.csv") 

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

ca_counties <- read_sf(".", layer = "california_county_shape_file")

# Load raster package and an example SpatialPolygonsDataFrame
data("ca_counties", package="maptools")


#This worked, as well, sort of. Able to get a map of the six locations! 
coast_counties <- ca_counties %>%
  filter(NAME %in% c("San Luis Obispo", "Santa Barbara", "Ventura", "Los Angeles", "Orange", "San Diego"))



#example from lab 6, sort of works.. still trying to figure out how to post each data point
sites_hab <- st_as_sf(clean_hab, coords = c("longitude", "latitude"))


str(clean_hab)


# Define UI for application 
ui <- fluidPage(
   
   # Application title
   titlePanel("HABs of Southern California"),
   
   # Sidebar with a slider input for number of bins 
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
                     choices = c("Akashiwo" = "akashiwo" ,
                                 "Alexandrium" = "alexandrium",
                                 "Ammonia" = "ammonia" ,
                                 "Chlorophyll" = "chlorophyll",
                                 "Domoic Acid" = "domoic_acid",
                                 "Nitrate" = "nitrate",
                                 "Phosphate" = "phosphate",
                                 "Pseudo Nitzschia Delicatissima" = "pseudo_nitzschia_delicatissima" ,
                                 "Pseudo Nitzschia Seriata" = "pseudo_nitzschia_seriata" ,
                                 "Silicate" = "silicate" ,
                                 "Water Temperature" = "water_temp"),
                     selected = "Akashiwo")
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

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #create new df for filtering input$year, input$month, and select the variables we want, 

  
    
    output$Map <- renderPlot({
      data <- switch(input$variable, 
                     "Akashiwo" = clean_hab$akashiwo ,
                     "Alexandrium" = clean_hab$alexandrium,
                     "Ammonia" = clean_hab$ammonia ,
                     "Chlorophyll" = clean_hab$chlorophyll,
                     "Domoic Acid" = clean_hab$domoic_acid,
                     "Nitrate" = clean_hab$nitrate,
                     "Phosphate" = clean_hab$phosphate,
                     "Pseudo Nitzschia Delicatissima" = clean_hab$pseudo_nitzschia_delicatissima ,
                     "Pseudo Nitzschia Seriata" = clean_hab$pseudo_nitzschia_seriata,
                     "Silicate" = clean_hab$silicate ,
                     "Water Temperature" = clean_hab$water_temp)
      
      color <- switch(input$variable, 
                      "Akashiwo" = "red" ,
                      "Alexandrium" = "blue",
                      "Ammonia" = "purple" ,
                      "Chlorophyll" = "green",
                      "Domoic Acid" = "yellow",
                      "Nitrate" = "cyan",
                      "Phosphate" = "maroon",
                      "Pseudo Nitzschia Delicatissima" = "hotpink" ,
                      "Pseudo Nitzschia Seriata" = "darkolivegreen" ,
                      "Silicate" = "darkseagreen" ,
                      "Water Temperature" = "coral")
      
      
      
      ggplot(data, color)+
        geom_sf(data = coast_counties, fill = "gray80") +
        geom_sf(data = data, aes(fill = color), size = 4) +
        theme_minimal()
    })
    
    
    
    
    
    
    
  
  
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)


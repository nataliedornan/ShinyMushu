######################### HABs Along the California Coast Shiny App #########################

# Load necessary packages
library(tidyverse)
library(shiny)
library(shinythemes)
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


# Read in data using read.csv instead of read_csv to specify strings as factors and solve issue of R assigning integer classes
HAB <- read.csv("clean_hab.csv", stringsAsFactors = F)

clean_hab_map <- read.csv("clean_hab.csv", stringsAsFactors = F)


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



########## ui ##########################################################

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
                   
############## HAB ABUNDANCE PLOT TAB #############################################################
                   # Create tab for HAB Abundance Chart
                   tabPanel(title = "Abundance Chart",
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
                   
                   
############## CORRELATION PLOT TAB #############################################################
                   # Create tab for Correlation Plot
                   tabPanel(title = "Correlation Plot",
sidebarLayout(
  sidebarPanel(
    
    ##create select box with locations
    selectInput(inputId = "location", label = h3("Station Name"), 
                choices = list("Cal Poly Pier" = "Cal Poly Pier", 
                               "Goleta Pier" = "Goleta Pier", 
                               "Stearns Wharf" = "Stearns Wharf", 
                               "Santa Monica Pier" = "Santa Monica Pier", 
                               "Newport Pier" = "Newport Pier", 
                               "Scripps Pier" = "Scripps Pier"), 
                selected = 1),
    
    ##create group checkbox for x variables
    radioButtons(inputId = "yvar", label = h3("Dependent Variables"), 
                 choices = list("Akashiwo sp." = "akashiwo", 
                                "Alexandrium spp." = "alexandrium", 
                                "Chlorophyll" = "chlorophyll", 
                                "Domoic Acid" = "domoic_acid",
                                "Pseudo Nitzschia Spp." = "pseudo_nitzschia_spp"),
                 selected = "chlorophyll"),
    
  #  ##create group checkbox for y variables   
  #  checkboxInput("logy", "Log Y", TRUE),
    
    radioButtons(inputId = "xvar", label = h3("Independent Variables"), 
                 choices = list("Akashiwo sp." = "akashiwo", 
                                "Alexandrium spp." = "alexandrium", 
                                "Ammonia" = "ammonia", 
                                "Chlorophyll" = "chlorophyll", 
                                "Domoic Acid" = "domoic_acid", 
                                "N+N" = "n_n", 
                                "Phosphate" = "phosphate",
                                "Pseudo Nitzschia Spp." = "pseudo_nitzschia_spp",
                                "Silicate" = "silicate", 
                                "Water Temp" = "water_temp"),
                 selected = "akashiwo")
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
               plotOutput("scatter"),
               tableOutput("values")
  )
)
),
                   
             
############## INTERACTIVE MAP TAB #############################################################   
                   # Create tab for Interactive Map
                   tabPanel(title = "Interactive Map",
                            # Sidebar with a slider, radio, and select inputs 
                            sidebarLayout(
                              sidebarPanel(
                                radioButtons("year_map",
                                             label = "Year:",
                                             choices = list("2008" = "2008",
                                                            "2009" = "2009",
                                                            "2010" = "2010",
                                                            "2011" = "2011",
                                                            "2012" = "2012",
                                                            "2013" = "2012",
                                                            "2014" = "2014",
                                                            "2015" = "2015",
                                                            "2016" = "2016",
                                                            "2017" = "2017",
                                                            "2018" = "2018"),
                                             selected = "2008"),
                                sliderInput("month_map",
                                            "Month:",
                                            min = 1,
                                            max = 12,
                                            value = 1),
                                
                                selectInput("variable_map",
                                            label = "Choose a HAB Variable:",
                                            choices = list("Akashiwo sp." = "akashiwo", 
                                                           "Alexandrium spp." = "alexandrium", 
                                                           "Chlorophyll" = "chlorophyll", 
                                                           "Domoic Acid" = "domoic_acid", 
                                                           "Pseudo Nitzschia spp." = "pseudo_nitzschia_spp"),
                                            selected = 1)
                              ),
                              
                              # Show a plot of the generated distribution
                              mainPanel(
                                           leafletOutput(outputId = "Map")
                                           
                                  )
                                )
                              )
                   
######### END UI ###########           
                 ))



############## SERVER #############################################################

# Define server logic required to draw a histogram
server <- function(input, output) {
   
########## HAB ABUNDANCE PLOT OUTPUT #############
  output$abunPlot <- renderPlot({
    
    # filters by location only
    filtered <- HAB %>%
      filter(location == input$selectlocation_abun) %>%
      dplyr::select(year,
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
    
    ggplot(filtered, aes_string(x = "month", y = input$selectvar_abun)) +
      geom_col(fill = "seagreen3", color = "seagreen3") +
      scale_y_continuous(expand = c(0,0)) +
      scale_x_continuous(expand = c(0,0),
                         breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
                         labels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"),
                         limits = c(0,12.5)) +
      labs(x = "Month", y = "Variable") +
      theme_bw() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            panel.border = element_blank(),
            axis.line = element_line(colour = "black"))
    
  })
   
########## CORRELATION PLOT OUTPUT #############
   
  mydat <- reactive({
    
    HAB %>%
      filter(location == input$location) %>%
      select("location",
             "akashiwo", 
             "alexandrium",
             "ammonia", 
             "chlorophyll", 
             "domoic_acid", 
             "n_n", 
             "phosphate",
             "pseudo_nitzschia_spp", 
             "silicate", 
             "water_temp")
  })
  
  #output$logy <- reactive({
  #  log <- ln(input$var)
  #})
  
  
  output$values <- renderTable({
    mydat()
  })
  
  lm1 <- reactive({
    lm(HAB[,names(HAB) %in% input$yvar] ~ HAB[,names(HAB) %in% input$xvar])
  })  
  
  
  output$scatter <- renderPlot({
    
    ggplot() +
      geom_point(data = mydat(), aes_string(x = input$xvar, y = input$yvar)) +
      geom_smooth(data = mydat(), aes_string(x = input$xvar, y = input$yvar), method = "lm", color = "seagreen3")+
      labs(title = paste("Adj R2 = ",signif(summary(lm1())$adj.r.squared, 5),
                         "Intercept =",signif(lm1()$coef[[1]],5 ),
                         " Slope =",signif(lm1()$coef[[2]], 5),
                         " p =",signif(summary(lm1())$coef[2,4], 5))) +
      theme_bw()+
      xlab(print(input$xvar))+
      ylab(print(input$yvar))+
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            panel.border = element_blank(),
            axis.line = element_line(colour = "black"))
    
  })
   
########## INTERACTIVE MAP OUTPUT #############
  
  #create new df for filtering input$year, input$month, and select the variables we want,
  
  selected_var <- reactive({
    
    sites_hab %>%
      filter(year == input$year_map | month == input$month_map) %>%
      select("location",
             "year",
             "month",
             "akashiwo", 
             "alexandrium",
             "ammonia", 
             "chlorophyll", 
             "domoic_acid", 
             "n_n", 
             "phosphate",
             "pseudo_nitzschia_spp", 
             "silicate", 
             "water_temp",
             "geometry")
  })
  
  
  # mapcolor <- reactive({
  #   
  #   switch(input$variable,
  #          
  #          "Akashiwo sp." = "red",
  #          "Alexandrium spp." = "blue",
  #          "Ammonia" = "purple",
  #          "Chlorophyll" = "green",
  #          "Domoic Acid" = "yellow",
  #          "N+N" = "cyan",
  #          "Phosphate" = "maroon",
  #          "Pseudo Nitzschia spp." = "darkolivegreen",
  #          "Silicate" = "darkseagreen",
  #          "Water Temp" = "coral" )
  #   
  #   
  # })
  # 
  
  
  output$Map <- renderLeaflet({
    
    tm <-
      tm_shape(coast_counties) +
      tm_fill("COUNTY", palette = "Set1", alpha = 0.5, legend.show = FALSE)+
      tm_shape(selected_var()) +
      tm_bubbles(col = "location", size = input$variable_map)+
      tm_view(basemaps = "Stamen.TerrainBackground")


    tmap_mode("view")

    tmap_leaflet(tm)
    
  })
  
######### DONE ################
   
}


############## RUN THE APPLICATION ############################################################# 

# Run the application 
shinyApp(ui = ui, server = server)


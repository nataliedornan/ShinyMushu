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

# Read in raw data: "Harmful Algal Blooms 2008-2018"
HAB_data <- read_csv("HAB.csv")

# Clean up the data
clean_hab <- HAB_data %>% 
  # Convert to snake case
  clean_names(., case = c("snake")) %>% 
  # Selected species that are correlated with HABs
  dplyr::select(year, month, day, latitude, longitude, location, akashiwo_sanguinea_cells_l, alexandrium_spp_cells_l, ammonia_u_m, chlorophyll_mg_m3, domoic_acid_ng_m_l, nitrate_u_m, nitrite_u_m, phosphate_u_m, pseudo_nitzschia_delicatissima_group_cells_l, pseudo_nitzschia_seriata_group_cells_l, silicate_u_m, water_temperature_c) %>% 
  # Remove "NaNs"
  na.omit() %>%  
  # Rename variables
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
         water_temp = water_temperature_c) %>%
  # Create one column of year and month of sample together
  mutate(year_month = str_c(year, month, sep = "-")) %>% 
  # sum N+N concentrations
  mutate("n_n" = nitrate + nitrite) %>%
  mutate(pseudo_nitzschia_spp = pseudo_nitzschia_delicatissima + pseudo_nitzschia_seriata)

# Create csv file of clean_hab df
write.csv(clean_hab, "clean_hab.csv")

# Read in clean_hab csv using read.csv instead of read_csv to specify strings as factors and resolve issue of R assigning integer classes
HAB <- read.csv("clean_hab.csv", stringsAsFactors = F)

# Load the California County sf
ca_counties <- read_sf(".", layer = "california_county_shape_file")

# Filter out just the California coast counties our monitoring stations are located in
coast_counties <- ca_counties %>%
  filter(NAME %in% c("San Luis Obispo", "Santa Barbara", "Ventura", "Los Angeles", "Orange", "San Diego"))

# Set a coordinate reference from scratch
st_crs(coast_counties) = 4326

# Read in cleaned up "HAB" df by longitude and latitude in CSV, and set CRS
sites_hab <- st_as_sf(HAB, coords = c("longitude", "latitude"), crs = 4326)



########## BEGIN UI ##########################################################

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
                   
############## HAB ABUNDANCE CHART TAB #######################################################
                   # Create tab for HAB Abundance Chart
                   tabPanel(title = "HAB Abundance Chart",
                            # Move sidebar containing widgets to right side of screen
                            sidebarLayout(position = "right",
                                          sidebarPanel(
                                            # Create select widget for monitoring locations
                                            selectInput("selectlocation_abun",
                                                        label = h4("Monitoring Location:"),
                                                        choices = list("Cal Poly Pier" = "Cal Poly Pier", 
                                                                       "Goleta Pier" = "Goleta Pier", 
                                                                       "Stearns Wharf" = "Stearns Wharf", 
                                                                       "Santa Monica Pier" = "Santa Monica Pier", 
                                                                       "Newport Pier" = "Newport Pier", 
                                                                       "Scripps Pier" = "Scripps Pier"),
                                                        selected = 1),
                                            
                                            
                                            # Create select widget for variable
                                            selectInput("selectvar_abun",
                                                        label = h4("Variable:"),
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
                   
                   
############## LINEAR REGRESSION MODEL TAB #############################################################
                   # Create tab for Linear Regression Model
                   tabPanel(title = "Linear Regression Model",
sidebarLayout(
  sidebarPanel(
    
    # Create select box with monitoring locations
    selectInput(inputId = "location", label = h4("Monitoring Location:"), 
                choices = list("Cal Poly Pier" = "Cal Poly Pier", 
                               "Goleta Pier" = "Goleta Pier", 
                               "Stearns Wharf" = "Stearns Wharf", 
                               "Santa Monica Pier" = "Santa Monica Pier", 
                               "Newport Pier" = "Newport Pier", 
                               "Scripps Pier" = "Scripps Pier"), 
                selected = 1),
    
    # Create group checkbox for x variables (dependent variables)
    radioButtons(inputId = "yvar", label = h4("Dependent Variable:"), 
                 choices = list("Akashiwo sp." = "akashiwo", 
                                "Alexandrium spp." = "alexandrium", 
                                "Chlorophyll" = "chlorophyll", 
                                "Domoic Acid" = "domoic_acid",
                                "Pseudo Nitzschia Spp." = "pseudo_nitzschia_spp"),
                 selected = "chlorophyll"),
    
  ## Create checkboxInput ("logy", "Log Y", TRUE) ## NOTE: did not end up using this checkbox
  
    # Create group checkbox for y variables (independent variables)
    radioButtons(inputId = "xvar", label = h4("Independent Variable:"), 
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
  
  # Show a scatter plot with linear regression
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
                                             label = h4("Year:"),
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
                                            label = h4("Month:"),
                                            min = 1,
                                            max = 12,
                                            value = 1),
                                
                                selectInput("variable_map",
                                            label = h4("Variable:"),
                                            choices = list("Akashiwo sp." = "akashiwo", 
                                                           "Alexandrium spp." = "alexandrium", 
                                                           "Chlorophyll" = "chlorophyll", 
                                                           "Domoic Acid" = "domoic_acid", 
                                                           "Pseudo Nitzschia spp." = "pseudo_nitzschia_spp"),
                                            selected = 1)
                              ),
                              
                              # Display an interactive map using bubbleplot to show variables
                              mainPanel(
                                           leafletOutput(outputId = "Map")
                                           
                                  )
                                )
                              )
                   
####################### END UI #########################################################          
                 ))



####################### BEGIN SERVER #############################################################

# Define server logic
server <- function(input, output) {
   
######################### HAB ABUNDANCE CHART OUTPUT ##################################
  output$abunPlot <- renderPlot({
    
    # Filter by monitoring location only
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
    
    # Create column graph in ggplot plotting months (Jan-Dec) on the x-axis and variable on the y-axis
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
            axis.line = element_line(colour = "black"),
            axis.text.x = element_text(angle = 45, vjust = 0.6))
    
  })
   
###################### LINEAR REGRESSION MODEL OUTPUT ############################
   
  mydat <- reactive({
    
    # Filter by monitoring location and select columns needed
    HAB %>%
      filter(location == input$location) %>%
      dplyr::select("location",
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
  
  ## NOTE: didn't end up using below code for log y box:
  #output$logy <- reactive({
  #  log <- ln(input$var)
  #})
  
  # Show associated data values in table below linear regression model
  output$values <- renderTable({
    mydat()
  })
  
  # Create linear regression model
  lm1 <- reactive({
    lm(HAB[,names(HAB) %in% input$yvar] ~ HAB[,names(HAB) %in% input$xvar])
  })  
  
  output$scatter <- renderPlot({
    
    # Create scatterplot with dependent and independent variables 
    ggplot() +
      geom_point(data = mydat(), aes_string(x = input$xvar, y = input$yvar)) +
      geom_smooth(data = mydat(), aes_string(x = input$xvar, y = input$yvar), method = "lm", color = "seagreen3")+
      labs(title = paste("R2 = ",signif(summary(lm1())$r.squared, 5),
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
   
#################### INTERACTIVE MAP OUTPUT ##################################
  
  # Create new df "selected_var" for filtering input$year_map, input$month_map, and select required variables
  selected_var <- reactive({
    
    sites_hab %>%
      filter(year == input$year_map | month == input$month_map) %>%
      dplyr::select("location",
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
  
  
  output$Map <- renderLeaflet({
    
    # Create tm_map of the counties the monitoring locations are found in
    tm <-
      tm_shape(coast_counties) +
      tm_fill("COUNTY", 
              # pallette = "Set1",
              col = "peachpuff4",
              alpha = 0.35,
              # fill = "NA",
              # color = "gray30",
              # size = 0.1,
              legend.show = FALSE) +
      tm_borders() +
      tm_shape(selected_var()) +
      # Bubble plot the denisties of the HAB identifiers
      tm_bubbles(col = "location", size = input$variable_map, border.col = "black")+
      tm_view(basemaps = "Stamen.TerrainBackground") 

    tmap_mode("view")

    tmap_leaflet(tm) 
    
  })
  
########################## END SERVER #####################################################
   
}


############## RUN THE APPLICATION ############################################################# 

# Run the application 
shinyApp(ui = ui, server = server)


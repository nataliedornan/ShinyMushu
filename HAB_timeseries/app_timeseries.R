###### TIME SERIES GRAPH ########

library(tidyverse)
library(shiny)
library(shinythemes)
library(ggplot2)
library(tseries)
library(forecast)
library(scales)
library(lubridate)


HAB <- read.csv("clean_hab.csv", stringsAsFactors = F)




# Create 'ts' time series data
# HAB_ts <- ts(HAB_dates$chlorophyll, frequency = 12, start = c(2008,1))
# 
# HAB_ts
# 
# plot(HAB_ts)


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
                                            
                                            # # Create select widget for year
                                            # selectInput("selectyear_abun",
                                            #             label = h4("Year"),
                                            #             choices = c(2008:2018), 
                                            #             selected = 1),
                                            # 
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
                   
                   
                   # Create tab for Time Series Graph
                   tabPanel(title = "Time Series",
                            # Move sidebar containing widgets to right side of screen
                            sidebarLayout(
                                          sidebarPanel(
                                            # Create select widget for monitoring locations
                                            selectInput("selectlocation_time",
                                                        label = h4("Monitoring Location"),
                                                        choices = list("Cal Poly Pier" = "Cal Poly Pier", 
                                                                       "Goleta Pier" = "Goleta Pier", 
                                                                       "Stearns Wharf" = "Stearns Wharf", 
                                                                       "Santa Monica Pier" = "Santa Monica Pier", 
                                                                       "Newport Pier" = "Newport Pier", 
                                                                       "Scripps Pier" = "Scripps Pier"),
                                                        selected = 1),
                                            
                                            # # Create select widget for year
                                            # selectInput("selectyear_abun",
                                            #             label = h4("Year"),
                                            #             choices = c(2008:2018), 
                                            #             selected = 1),
                                            # 
                                            # Create select widget for variable
                                            selectInput("selectvar_time",
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
                                            plotOutput("timePlot"))
                                          
                            )
                            
                   ),
                   
                   
                   # Create tab for Interactive Map
                   tabPanel(title = "Interactive Map")
                   
                   
                   
                 ))

#
#
#
########### SERVER ############
# Define server logic required for each tab
server <- function(input, output) {
  
  
##################################### HAB ABUNDANCE PLOT #########################################
  
  # Send HAB abundance chart to the ui as "abunPlot"
  output$abunPlot <- renderPlot({
    
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
    
    
    ggplot(filtered, aes_string(x = "month", y = input$selectvar_abun)) +
      geom_col(fill = "seagreen", color = "seagreen") +
      scale_y_continuous(expand = c(0,0)) +
      scale_x_continuous(expand = c(0,0), limits = c(0,12.5), breaks = scales::pretty_breaks(n = 12)) +
      # facet_wrap(~year, scale = "free") +
      labs(x = "Month", y = "Variable") +
      theme_bw() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            panel.border = element_blank(),
            axis.line = element_line(colour = "black"))
    
  })
  
  
##################################### TIME SERIES #########################################
  
  # HAB_select <- reactive({
  #   
  #   # HAB_date_2 <- clean_hab %>%
  #   #   filter(location == input$selectlocation_time) %>%
  #   #   dplyr::select(year,
  #   #                 month,
  #   #                 akashiwo,
  #   #                 alexandrium,
  #   #                 ammonia,
  #   #                 chlorophyll,
  #   #                 domoic_acid,
  #   #                 n_n,
  #   #                 phosphate,
  #   #                 silicate,
  #   #                 water_temp)
  #   
  # })
  
  
  output$timePlot <- renderPlot({
    
    HAB_dates <- clean_hab %>%
      unite_("date", c("year", "month", "day"))
    
      HAB_dates$date <- ymd(HAB_dates$date)   
      
    
    HAB_date_2 <- HAB_dates %>%
      filter(location == input$selectlocation_time) %>%
      dplyr::select(date,
                    akashiwo,
                    alexandrium,
                    ammonia,
                    chlorophyll,
                    domoic_acid,
                    n_n,
                    phosphate,
                    silicate,
                    water_temp)
    
    ggplot(HAB_date_2, aes(date, input$selectvar_time)) +
      geom_line()
    
    # HAB_ts <- ts(HAB_select$input$selectvar_time, frequency = 12, start = c(2008), end = c(2017))
    # 
    # plot(HAB_ts)
    # 
    
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)


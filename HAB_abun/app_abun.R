###### ABUNDANCE CHART ########

library(tidyverse)
library(shiny)
library(shinythemes)

# load the data here
# as long as your selections have the exact same notation as the columns,
# if this is the same as you list in your widget lists
# in the widgets (in the ui), need to call them 
# in the server, you create a new reaction that will call "input$location" from the ui

# use read.csv?

HAB <- read.csv("clean_hab.csv", stringsAsFactors = F)



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
tabPanel(title = "Interactive Map")



))

#
#
#
########### SERVER ############
# Define server logic required for each tab
server <- function(input, output) {
  
  
  # Create reactive function to isolate reactions related to the HAB abundance chart
  abun_data <- reactive({
   
    
    # clean_hab %>%
     # filter(location == input$selectlocation_abun) %>%
    #  filter(year == input$selectyear_abun) %>%
     # select(input$selectvar_abun)
    # function(input$selectlocation_abun)
    # if(input$location == "Stearns Wharf")
    
    #vswitch(input$selectlocation_abun,
          # "Stearns Wharf")
    
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
     
     # abuncolor <- switch(input$selectvar_abun,
     #                    
     #                    "Akashiwo sp." = "red",
     #                    "Alexandrium spp." = "blue",
     #                    "Ammonia" = "purple",
     #                    "Chlorophyll" = "green",
     #                    "Domoic Acid" = "yellow",
     #                    "N+N" = "cyan",
     #                    "Phosphate" = "maroon",
     #                    "Pseudo Nitzschia spp." = "darkolivegreen",
     #                    "Silicate" = "darkseagreen",
     #                    "Water Temp" = "coral" )
     
     # maybe don't allow to select by year, still show month by variable but do facet_wrap and show all the months
     # compare seasonality of these species across all years
     # TIME SERIES GRAPH INSTEAD?? see assignment 3
     
     ggplot(filtered, aes_string(x = "month", y = input$selectvar_abun)) +
       geom_col(fill = "seagreen", color = "seagreen") +
       scale_y_continuous(expand = c(0,0)) +
       scale_x_continuous(expand = c(0,0),
                          breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
                          labels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"),
                          # breaks = scales::pretty_breaks(n = 12),
                          limits = c(0,12.5)) +
       labs(x = "Month", y = "Variable") +
       theme_bw() +
       theme(panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             panel.background = element_blank(),
             panel.border = element_blank(),
             axis.line = element_line(colour = "black"))
     
     # labels=c("1" = "January", 
     #          "2" = "February",
     #          "3" = "March",
     #          "4" = "April",
     #          "5" = "May",
     #          "6" = "June",
     #          "7" = "July",
     #          "8" = "August",
     #          "9" = "September",
     #          "10" = "October",
     #          "11" = "November",
     #          "12" = "December")
     
     # need location (selectlocation_abun), then year (selectyear_abun), then variable (selectvar_abun) (with all months in the year)
     
# ggplot() +
 #       geom_col(clean_hab, aes(x = input$year, y = input$selectvar_abun)fill = "seagreen3", color = "seagreen") +
#        labs(x = "Month", y = "Variable") +
 #       theme_bw() 
      
      

   })
}

# Run the application 
shinyApp(ui = ui, server = server)


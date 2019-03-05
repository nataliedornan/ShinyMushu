#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)

# Define UI for application that draws a histogram
ui <- navbarPage(theme = shinytheme("superhero"),
                 title = "Harmful Algal Blooms along the California Coast (2008-2018)",
                 
    tabsetPanel(
      tabPanel(title = "Summary",
               sidebarLayout(
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
                 
                 # Show a picture of a red tide
                 mainPanel(
                   img(src = "red_tide.jpg"),
                   p("Red algal bloom at Leigh, near Cape Rodney.",
                     em("Photo: SERC Carleton College."))
                 )
               )
               ),
      tabPanel(title = "HAB Abundance Chart",
               sidebarLayout(position = "right",
                             sidebarPanel(
                               selectInput("select",
                                           label = h4("Monitoring Locations"),
                                           choices = list("Cal Poly Pier" = 1, "Goleta Pier" = 2, "Stearns Wharf" = 3, "Santa Monica Pier" = 4, "Newport Pier" = 5, "Scripps Pier" = 6), selected = 1),
                               
                               selectInput("select",
                                           label = h4("Year"),
                                           choices = list("2008" = 1, "2009" = 2, "2010" = 3, "2011" = 4, "2012" = 5, "2013" = 6, "2014" = 7, "2015" = 8, "2016" = 9, "2017" = 10, "2018" = 11), selected = 1)
                               
                             ),
                             
                             # Show a plot of the generated distribution
                             mainPanel(
                               plotOutput("distPlot"))
    
      )
    
   ),

tabPanel(title = "Correlation Plot"),
tabPanel(title = "Interactive Map")

))

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      
      chlorophyll_col <- ggplot(clean_hab, aes(x = month, y = chlorophyll)) +
        geom_col(fill = "seagreen3", color = "seagreen") +
        labs(x = "Month", y = "Chlorophyll Concentration (mg/L)") +
        theme_bw() 
      
      chlorophyll_col
   })
}

# Run the application 
shinyApp(ui = ui, server = server)


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

# Define UI for application that displays a photo and summary text
ui <- fluidPage(theme = shinytheme("superhero"),
   
   # Application title
   titlePanel("Harmful Algal Blooms Along the California Coast (2008-2018)"),
   
   # Sidebar with summary text 
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
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
}

# Run the application 
shinyApp(ui = ui, server = server)


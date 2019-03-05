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
                     label = "Choose a HAB Variable",
                     choices = list("akashiwo" = 1,
                                 "alexandrium" = 2,
                                 "ammonia" = 3 ,
                                 "chlorophyll" = 4,
                                 "domoic acid" = 5,
                                 "nitrate" = 6,
                                 "phosphate" = 7,
                                 "pseudo nitzschia delicatissima" = 8 ,
                                 "pseudo nitzschia seriata" = 9 ,
                                 "silicate" = 10 ,
                                 "water temp" = 11),
                     selected = 1)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("HABMap")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  
  output$HABMap <- renderPlot({
    
    
    ggplot(coast_counties) +
      geom_sf(data = coast_counties, 
              fill = "NA",
              color = "gray30",
              size = 0.1) +
      coord_sf(xlim = c(-118, 125), ylim = c(31, 36)) +
      geom_point(data = clean_hab,
                 aes(x = longitude, y = latitude),
                 size = 3,
                 color = "gray10",
                 alpha = 0.5) +
      theme_minimal()+
      coord_sf(datum = NA) 
  })
  
  output$value <- renderPlot({ input$year })
  
  output$value <- renderPrint({ input$month })
  
  output$value <- renderPrint({ input$variable })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)


####### TIME SERIES GRAPH #############################################

library(tidyverse)
library(shiny)
library(shinythemes)


# Read in data using read.csv instead of read_csv to specify strings as factors and solve issue of R assigning integer classes
HAB <- read.csv("clean_hab.csv", stringsAsFactors = F)


########## ui ##########################################################

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)


############## SERVER #############################################################

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}



############## RUN THE APPLICATION ############################################################# 
shinyApp(ui = ui, server = server)


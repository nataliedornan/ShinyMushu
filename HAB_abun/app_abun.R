#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("HAB Abundance"),
   
   # Sidebar with a select input for monitoring locations 
   sidebarLayout(position = "right",
      sidebarPanel(
         selectInput("select",
                     label = h4("Monitoring Locations"),
                     choices = list("Cal Poly Pier" = 1, "Goleta Pier" = 2, "Stearns Wharf" = 3, "Santa Monica Pier" = 4, "Newport Pier" = 5, "Scripps Pier" = 6),
                     selected = 1),
         
         selectInput("select",
                     label = h4("Year"),
                     choices = list("2008" = 1, "2009" = 2, "2010" = 3, "2011" = 4, "2012" = 5, "2013" = 6, "2014" = 7, "2015" = 8, "2016" = 9, "2017" = 10, "2018" = 11),
                     selected = 1)
         
      ),
        
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

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


#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

## to make Input dropdown 



# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("HAB Correlation Plots"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        
        ##create select box with locations
        
        selectInput("select", label = h3("Select box"), 
                    choices = list("All" = 1, "Cal Poly Pier" = 2, "Goleta Pier" = 3, "Stearn's Wharf" = 4, "Santa Monica Pier" = 5, "Newport Pier" = 6, "Scripps Pier" = 7), 
                    selected = 1),
        
        hr(),
        fluidRow(column(7, verbatimTextOutput("Location"))),
      
        ##create group checkbox for x variables
      
      checkboxGroupInput("checkGroup", label = h3("x variables"), 
                         choices = list("Akashiwo sp." = 1, "Alexandrium spp." = 2, "Ammonia" = 3, "Chlorophyll" = 4, "Domoic Acid" = 5, "N+N" = 6, "Phosphate" = 7, "Silicate" = 8, "Water Temp" = 9),
                         selected = 1),
      
      hr(),
      fluidRow(column(9, verbatimTextOutput("Variable"))),
   
      ##create group checkbox for y variables     
      
      checkboxGroupInput("checkGroup", label = h3("y variables"), 
                         choices = list("Akashiwo sp." = 1, "Alexandrium spp." = 2, "Ammonia" = 3, "Chlorophyll" = 4, "Domoic Acid" = 5, "N+N" = 6, "Phosphate" = 7, "Silicate" = 8, "Water Temp" = 9),
                         selected = 1),
      
      hr(),
      fluidRow(column(9, verbatimTextOutput("Variable")))
         
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required for lm
server <- function(input, output) {
   
  output$plot <- renderPlot({

    g <- ggplot(hab_cor, aes(water_temp, chlorophyll)) + geom_point()
    
    g <- g + geom_smooth(method = "lm", col = "red") 
    
    g <- g + labs(title = paste("Adj R2 = ",signif(summary(temp_lm)$adj.r.squared, 5),
                         "Intercept =",signif(temp_lm$coef[[1]],5 ),
                         " Slope =",signif(temp_lm$coef[[2]], 5),
                         " P =",signif(summary(temp_lm)$coef[2,4], 5)))
      
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)



library(shiny)
library(tidyverse)

hab_new <- read_csv("hab_new.csv")

## to make Input dropdown 

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("HAB Correlation Plots"),
  
  # Sidebar 
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
                         choices = list("Akashiwo sp." = "Akashiwo sp.", 
                                        "Alexandrium spp." = "Alexandrium spp.", 
                                        "Ammonia" = "Ammonia", 
                                        "Chlorophyll" = "Chlorophyll", 
                                        "Domoic Acid" = "Domoic Acid", 
                                        "N+N" = "N+N", 
                                        "Phosphate" = "Phosphate", 
                                        "Silicate" = "Silicate", 
                                        "Water Temp" = "Water Temp"),
                         selected = "Akashiwo sp."),
      
      ##create group checkbox for y variables     
      
      radioButtons(inputId = "xvar", label = h3("Independent Variables"), 
                         choices = list("Akashiwo sp." = "Akashiwo sp.", 
                                        "Alexandrium spp." = "Alexandrium spp.", 
                                        "Ammonia" = "Ammonia", 
                                        "Chlorophyll" = "Chlorophyll", 
                                        "Domoic Acid" = "Domoic Acid", 
                                        "N+N" = "N+N", 
                                        "Phosphate" = "Phosphate", 
                                        "Silicate" = "Silicate", 
                                        "Water Temp" = "Water Temp"),
                         selected = "Akashiwo sp.")
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Application",     
                 plotOutput("scatter")#,
           #      tableOutput("lmStats"),
           #      tableOutput("lmResults"),
            #     tableOutput("values")
           ))
    )
  )
)

####################################################################################

server <- function(input, output) {
  mydat <- reactive({
  HAB %>%
      filter(location == input$location)  %>%
      select(input$xvar, input$xvar)
  })
      
output$scatter <- renderPlot({
ggplot() +
  geom_point(data = mydat(), aes(x = input$xvar, y = input$yvar))
})
}

# Run the application 
shinyApp(ui = ui, server = server)
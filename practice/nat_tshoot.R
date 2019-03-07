library(shiny)
library(tidyverse)

HAB <- read.csv("clean_hab.csv", stringsAsFactors = F)

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
                         choices = list("Akashiwo sp." = "akashiwo", 
                                        "Alexandrium spp." = "alexandrium", 
                                        "Ammonia" = "ammonia", 
                                        "Chlorophyll" = "chlorophyll", 
                                        "Domoic Acid" = "domoic_acid", 
                                        "N+N" = "n_n", 
                                        "Phosphate" = "phosphate", 
                                        "Silicate" = "silicate", 
                                        "Water Temp" = "water_temp"),
                         selected = "alexandrium"),
      
      ##create group checkbox for y variables     
      
      radioButtons(inputId = "xvar", label = h3("Independent Variables"), 
                         choices = list("Akashiwo sp." = "akashiwo", 
                                        "Alexandrium spp." = "alexandrium", 
                                        "Ammonia" = "ammonia", 
                                        "Chlorophyll" = "chlorophyll", 
                                        "Domoic Acid" = "domoic_acid", 
                                        "N+N" = "n_n", 
                                        "Phosphate" = "phosphate", 
                                        "Silicate" = "silicate", 
                                        "Water Temp" = "water_temp"),
                         selected = "akashiwo")
      
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
      filter(location == input$location)
  })
      
  output$scatter <- renderPlot({
    
    ggplot() +
      geom_point(data = mydat(), aes_string(x = input$xvar, y = input$yvar))
  })
}

##add checkbox input where it logs y axis

# Run the application 
shinyApp(ui = ui, server = server)
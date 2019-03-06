library(shiny)
library(tidyverse)

HAB <- read_csv("clean_hab.csv") %>%
  rename("Akashiwo sp." = akashiwo,
         "Alexandrium spp." = alexandrium ,
         "Ammonia" = ammonia,
         "Chlorophyll" = chlorophyll,
         "Domoic Acid" = domoic_acid,
         "Phosphate" = phosphate,
         "Pseudo Nitzschia spp." = pseudo_nitzschia_spp,
         "N+N" = n_n,
         "Silicate" = silicate,
         "Water Temp" = water_temp) # Rename variables

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
                  choices = list("Cal Poly Pier", 
                                 "Goleta Pier", 
                                 "Stearns Wharf", 
                                 "Santa Monica Pier", 
                                 "Newport Pier", 
                                 "Scripps Pier"), 
                  selected = 1),
      
      ##create group checkbox for x variables
 
           
      radioButtons(inputId = "DepVar", label = h3("Dependent Variables"), 
                         choices = list("Akashiwo sp.", 
                                        "Alexandrium spp.", 
                                        "Ammonia", 
                                        "Chlorophyll", 
                                        "Domoic Acid", 
                                        "N+N", 
                                        "Phosphate", 
                                        "Silicate", 
                                        "Water Temp"),
                         selected = 1),
      
      ##create group checkbox for y variables     
      
      radioButtons(inputId = "IndVar", label = h3("Independent Variables"), 
                         choices = list("Akashiwo sp.", 
                                        "Alexandrium spp.", 
                                        "Ammonia", 
                                        "Chlorophyll", 
                                        "Domoic Acid", 
                                        "N+N", 
                                        "Phosphate",
                                        "Silicate", 
                                        "Water Temp"),
                         selected = 1)
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Application",     
                 plotOutput("scatter"),
                 tableOutput("lmStats"),
                 tableOutput("lmResults"),
                 tableOutput("values")))
    )
  )
)

####################################################################################

server <- function(input, output) {
  mydat <- reactive({
    HAB %>%
      filter(location == input$location) %>%
      select(input$IndVar, input$DepVar)
  })

output$scatter <- renderPlot({
ggplot() +
  geom_point(data = mydata(), aes(x = input$IndVar, y = input$DepVar))
})
}

# Run the application 
shinyApp(ui = ui, server = server)
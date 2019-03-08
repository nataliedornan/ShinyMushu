library(shiny)
library(tidyverse)
library(data.table)
library(ggplot2)

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
                                        "Pseudo Nitzsia Spp." = "pseudo_nitzschia_spp",
                                        "Water Temp" = "water_temp"),
                         selected = "chlorophyll"),
  
##create group checkbox for y variables   
         
      checkboxInput("logy", "Log Y", TRUE),
      
      radioButtons(inputId = "xvar", label = h3("Independent Variables"), 
                         choices = list("Akashiwo sp." = "akashiwo", 
                                        "Alexandrium spp." = "alexandrium", 
                                        "Ammonia" = "ammonia", 
                                        "Chlorophyll" = "chlorophyll", 
                                        "Domoic Acid" = "domoic_acid", 
                                        "N+N" = "n_n", 
                                        "Phosphate" = "phosphate",
                                        "Pseudo Nitzsia Spp." = "pseudo_nitzschia_spp",
                                        "Silicate" = "silicate", 
                                        "Water Temp" = "water_temp"),
                         selected = "akashiwo")

    
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Application",  
                 plotOutput("scatter"),
                 tableOutput("values")
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
  
  #output$logy <- reactive({
  #  log <- ln(input$var)
  #})
  

  output$values <- renderTable({
    mydat()
  })
  
  lm1 <- reactive({
    lm(HAB[,names(HAB) %in% input$yvar] ~ HAB[,names(HAB) %in% input$xvar])
       })  
  
  output$scatter <- renderPlot({
    
  ggplot() +
    geom_point(data = mydat(), aes_string(x = input$xvar, y = input$yvar)) +
    geom_smooth(data = mydat(), aes_string(x = input$xvar, y = input$yvar), method = "lm", color = "seagreen3")+
           labs(title = paste("Adj R2 = ",signif(summary(lm1())$adj.r.squared, 5),
                              "Intercept =",signif(lm1()$coef[[1]],5 ),
                              " Slope =",signif(lm1()$coef[[2]], 5),
                              " P =",signif(summary(lm1())$coef[2,4], 5))) +
      theme_bw()
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
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
   titlePanel("HABs of Southern California"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("year",
                     "Year:",
                     min = 2008,
                     max = 2018,
                     value = 1),
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
                                 "pseudo_nitzschia_seriata" = 9 ,
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
    
    
    
    data <- switch(input$variable, 
                   "akashiwo" = sites_hab$akashiwo,
                   "alexandrium" = sites_hab$alexandrium,
                   "ammonia" = sites_hab$ammonia,
                   "chlorophyll" = sites_hab$chlorophyll,
                   "domoic acid" = sites_hab$domoic,
                   "nitrate" = sites_hab$nitrate,
                   "phosphate" = sites_hab$phosphate,
                   "pseudo nitzschia delicatissima" = sites_hab$pseudo_nitzschia_delicatissima,
                   "pseudo_nitzschia_seriata" = sites_hab$pseudo_nitzschia_seriata,
                   "silicate" = sites_hab$silicate,
                   "water temp" = sites_hab$water_temp)
    
    color <- switch(input$variable, 
                    "akashiwo" = "darkgreen",
                    "alexandrium" = "black",
                    "ammonia" = "darkorange",
                    "chlorophyll" = "darkviolet",
                    "domoic acid" = "lightpink",
                    "nitrate" = "lightcyan",
                    "phosphate" = "indiared1",
                    "pseudo nitzschia delicatissima" = "magenta",
                    "pseudo_nitzschia_seriata" = "powderblue",
                    "silicate" = "yellow",
                    "water temp" = "royalblue")
    
    percent_map(data, color, input$range[1], input$range[2])
    
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)


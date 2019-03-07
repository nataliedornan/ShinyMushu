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
                  selected = "Cal Poly Pier"
                  ),
      
      br(),
      uiOutput("choose_result"),
      
      uiOutput("choose_regressors"),
      br()#,
      
      ##create group raciobuttons for x variables
      
      
#      radioButtons(inputId = "xvar", label = h3("Dependent Variables"), 
#                   choices = list("Akashiwo sp.", 
#                                  "Alexandrium spp.", 
#                                  "Ammonia", 
#                                  "Chlorophyll", 
#                                  "Domoic Acid", 
#                                  "N+N", 
#                                  "Phosphate", 
#                                  "Silicate", 
#                                  "Water Temp"),
#                   selected = 1),
      
      ##create group radio buttons for y variables     
      
#      radioButtons(inputId = "yvar", label = h3("Independent Variables"), 
#                   choices = list("Akashiwo sp.", 
#                                  "Alexandrium spp.", 
#                                  "Ammonia", 
#                                  "Chlorophyll", 
#                                  "Domoic Acid", 
#                                  "N+N", 
#                                  "Phosphate",
#                                  "Silicate", 
#                                  "Water Temp"),
#                   selected = 1)
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Application",     
                 plotOutput("scatter")#,
            #     tableOutput("lmStats"),
            #     tableOutput("lmResults"),
            #     tableOutput("values")))
    )
  )
)
)) ##remove these when putting table outputs back in
####################################################################################

print(getwd())
# "print" comments were scaffolding for debugging the application
data.maker <- function(data, y, x) {
  #   print(y)
  if(is.null(y)) res1=data[, 1] else res1=data[, y]
  #   print(res1)
  if(is.null(x)) regr1=data[, 6] else regr1=data[, x]
  #   print(x)
  
  data.frame(x=regr1, y=res1)
}

server <- function(input, output) {
  mydata <- reactive({
    
    HAB %>%
    
    # Define the data set and its columns
    # If missing input, return to avoid error later in function
 #   if(is.null(input$location))
  #    return()
    
    dat <- get(input$location)
    
    columns <- colnames(dat)
    
    if (is.null(columns) || !(columns %in% names(dat)))
      return()
    

    res <<- input$yvar
    
    # Make sure yvar is correct for data set (when data set changes, the
    # yvar will initially be for the previous data set)
    if (is.null(input$xvar) || !(input$xvar %in% names(dat)))
      return()
    
    regr <<- input$xvar
    #     print(regr)
    
    data.maker(data=dat, y=res, x=regr)
  })
  
  # Pick the resulting variable
  output$choose_result <- renderUI({

    dat <<- get(input$location)

    # Make sure columns are correct for data set (when data set changes, the
    # columns will initially be for the previous data set)
    columns <<- colnames(dat)
    #     print(columns)
    if (is.null(columns) || !(columns %in% names(dat)))
      return()
    selectInput("result","Pick resulting variable",
                as.list(columns),
                selected=columns[1])
  })
  
  # Select the required regressors (Check boxes)
  output$choose_regressors <- renderUI({

    dat <<- get(input$location)

    columns <<- colnames(dat)
    # Make sure columns are correct for data set (when data set changes, the
    # columns will initially be for the previous data set)
    if (is.null(columns) || !(columns %in% names(dat)))
      return()

    # Create the checkboxes and select the default regressor
    radioButtons("regressors", "Choose regressors", 
                 choices  = columns,
                 selected = columns[6])
  })
 
## these would be a list of the output data  
   
  output$values <- renderTable({

    mydata()
  })
  
## This would be the table output of results  
  
  lmResults <- reactive({

    
    regress.exp <<- input$regression
    if (!input$constant) regress.exp <- paste(input$regression, "- 1")
    

    #     print(regress.exp)
    #    print(mydata())
    lm(regress.exp, data=mydata())
  })
  
## This would be the table output of the linear model statistics
    
  output$lmStats <- renderTable({
    
    # Make sure result is correct for data set (when data set changes, the
    # result will initially be for the previous data set)
    if (is.null(input$result) || !(input$result %in% names(dat)))
      return()
    
    results <<- summary(lmResults())
    data.frame(R2=results$r.squared,
               adj.R2=results$adj.r.squared,
               DOF.model=results$df[1],
               DOF.available=results$df[2],
               DOF.total=sum(results$df[1:2]),
               f.value=results$fstatistic[1],
               f.denom=results$fstatistic[2],
               f.numer=results$fstatistic[3],
               p=1-pf(results$fstatistic[1],
                      results$fstatistic[2],
                      results$fstatistic[3]))
  })
  
  
  # Show coefficients
  output$lmResults <- renderTable({
    
    # Make sure result is correct for data set (when data set changes, the
    # result will initially be for the previous data set)
    if (is.null(input$result) || !(input$result %in% names(dat)))
      return()
    
    summary(lmResults())
  })
  
  # Show plot of points, regression line, residuals
  output$scatter <- renderPlot({
    
    # print(mydata())     
    data1 <<- mydata()
    if(length(data1) > 0){

      #     print(data1$x)
      x <<- data1$x

      #     print(data1$y)
      y <<- data1$y

      xcon <- seq(min(x)-.1, max(x)+.1, .025)
#      x2 <<- xcon^2
#      x3 <<- xcon^3
#      sqrtx <<- sapply(xcon, function(x) {if(x > 0) sqrt(x) else 0})
#      logx <<- sapply(xcon, function(x) {if(x > 0) log(x) else -100})
#      expx <<- exp(xcon)
      
      predictor <<- data.frame(x=xcon
                               )#,x2=x2,x3=x3,sqrtx=sqrtx,logx=logx,expx=expx)
      yhat <<- predict(lmResults())
      yline <<- predict(lmResults(), predictor)
      plot(c(min(x),max(x))
           ,c(min(y,yline),max(y,yline)),
           type="n",
           xlab=as.character(input$regressors),
           ylab=as.character(input$result),
           main=paste0("Regression Model: ", input$regression))
      
      if (input$predict) lines(xcon, yline, lwd=15, col=grey(.9))
      if (input$resid) for (j in 1:length(x))
        lines(rep(x[j],2), c(yhat[j],y[j]), col="red")
      if (input$showdata) points(x,y)
      if (input$predict) lines(xcon, yline, lwd=2, col="blue")
    }
    
  })
}


# Run the application 
shinyApp(ui = ui, server = server)



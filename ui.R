# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(highcharter)
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Old Faithful Geyser Data"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  
                  
                  min = 13,
                  
                  max = 500,
                  value = 20)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      # plotOutput("distPlot"),
      highchartOutput("Macroplot2")
      ,dataTableOutput('table')
    )
  )
))
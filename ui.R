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
     
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
   
  # plotOutput("distPlot"),
      highchartOutput("HighchartPlot",height = "300px"),dataTableOutput('tableoutput')
    )
  )
))
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(data.table)
shinyServer(function(input, output) {
  
  output$distPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
  })
  
  output$Macroplot2 <- renderHighchart(
    #h
    hc<-highchart() %>% 
      hc_chart(animation = FALSE) %>% 
      hc_title(text = "Macro data") %>% 
      hc_xAxis(categories=as.character(1:5),#tickInterval=4,#type='datetime',
               labels=list(rotation=45),
               plotBands=list(list(label=list(text='Forecast period',align='center',verticalAlign='top'),color="#edf2f7",from=0,to=2),
                              list(label=list(text='Calibration period',align='center',verticalAlign='top'),color="#ffffff",from=3,to=4)
               )) %>% 
      hc_yAxis(
        list(
          title = list(text = "Loss"),
          align = "left",
          showFirstLabel = FALSE,
          showLastLabel = FALSE#,
          #labels = list(format = "{value} &#176;C", useHTML = TRUE)
        ),
        list(
          title = list(text = "one"),
          align = "right",
          showFirstLabel = FALSE,
          showLastLabel = FALSE,
          #labels = list(format = "{value} mm"),
          opposite = TRUE
        ),
        list(
          title = list(text = "two"),
          align = "right",
          showFirstLabel = FALSE,
          showLastLabel = FALSE,
          #labels = list(format = "{value} mm"),
          opposite = TRUE
        )
      ) %>% 
      hc_plotOptions(
        series = list(
          #stickyTracking = FALSE,
          point = list(
            events = list(
              drop = JS("function(){
                        console.log(this.series)
                        window.data = _.map(this.series.data, function(e) { return e.y })
                        if(this.series.name=='one'){Shiny.onInputChange('inputname', data)}
                        else {Shiny.onInputChange('inputname2', data)}
}"))
              ))) %>% 
      
      hc_add_series(name = "one", type = 'line', color = 'red',
                    #data = c(7.0, 6.9, 9.5, 14.5, 18.2, 21.5, 25.2, 26.5, 23.3, 18.3, 13.9, 9.6),
                    data=c(7.0, 6.9, 9.5, 14.5, 18.2),yAxis=1,
                    draggableY = TRUE
                    
      )%>% 
      hc_add_series(name = "two", type = 'line', color = 'blue',
                    #data = c(7.0, 6.9, 9.5, 14.5, 18.2, 21.5, 25.2, 26.5, 23.3, 18.3, 13.9, 9.6),
                    data=c(14.5, 18.2, 21.5, 25.2, 26.5),yAxis=2,
                    draggableY = TRUE
      )              
    %>% 
      hc_add_series(name =paste0('Loss'), type = 'line', color = 'black',
                    #data = c(49.9, 71.5, 106.4, 129.2, 144.0, 176.0, 135.6, 148.5, 216.4, 194.1, 95.6, 54.4))
                    data=c(1:5),
                    draggableY = FALSE)
    
    
    
    
    
              )
  
  output$table <-renderDataTable({
    if(is.null(input$inputname)) var <-   c(7.0, 6.9, 9.5, 14.5, 18.2) else var <- input$inputname
    if(is.null(input$inputname2)) var2 <- c(14.5, 18.2, 21.5, 25.2, 26.5) else var2 <- input$inputname2
    #var <- var[(length(var)-input$n.ahead+1):length(var)]
    #Output from graph
    data.table(var,var2)#Here I want to use the dragged data. something linke input$highchart$... should do the trick I guess...
  })
  
  })
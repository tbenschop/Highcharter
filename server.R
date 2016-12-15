# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(data.table)
library(stringi)
library(stringr)
cor_macro1<-.5
col1 <- "#000000"#cbbPalette[1]#hc_colors()[3]
col2 <- "#FFFFFF"#cbbPalette[6]#"#EEEEEE"#hc_get_colors()[2]


## find all 7th of the month between two dates, the last being a 7th.
st <- as.Date("1998-12-17")
en <- as.Date("2000-1-7")
ll <- seq(en, st, by = "-1 month")

plotdata<-data.table(DATE=ll,value1=c(1:13),value2=c(11:23),loss=abs(c(rnorm(n = 13,mean = 12,sd = 5))))
shinyServer(function(input, output) {
  
 
  output$HighchartPlot <- renderHighchart({

    
    hc<-highchart() %>% 
      hc_chart(animation = FALSE) %>% 
      hc_title(text = "Macro data") %>% 
      hc_xAxis(categories=as.character(plotdata$DATE),tickInterval=4,#type='datetime',
               labels=list(rotation=45),
               plotBands=list(list(label=list(text='Forecast period',align='center',verticalAlign='top'),color="#edf2f7",from=1,to=2),
                              list(label=list(text='Calibration period',align='center',verticalAlign='top'),color="#ffffff",from=4,to=6)
               )) %>% 
      hc_yAxis_multiples(
        list(
          title = list(text = "M1.test"),
          align = "left",
          showFirstLabel = FALSE,
          showLastLabel = FALSE#,
          #labels = list(format = "{value} &#176;C", useHTML = TRUE)
        ),
        list(
          title = list(text = 'M2.test'),
          align = "right",
          showFirstLabel = FALSE,
          showLastLabel = FALSE,
          #labels = list(format = "{value} mm"),
          opposite = TRUE
        )
      ) %>% 
      hc_plotOptions(
        series = list(
          marker=list(enabled=T,symbol='circle',radius=3)
          
          # stickyTracking = FALSE,
          ,point = list(
            events = list(
              drop = JS("function(){
                        window.data = _.map(this.series.data, function(e) { return e.y })
                        console.log(this.series.name+'tim')
                        if(this.series.name.substring(0,2)=='M1'){
                        console.log(this.series.name+'yes')
                        Shiny.onInputChange('changed_series_drag1', this.series.name)
                        Shiny.onInputChange('inputname', data)
                        }
                        if(this.series.name.substring(0,2)=='M2'){
                        Shiny.onInputChange('changed_series_drag2', this.series.name)
                        Shiny.onInputChange('inputname2', data)
                        }
                        
                        
                        
  }")
          )
              )
              )) %>%            
      
      hc_add_series(name = 'M1.test', type = 'line', #color = cbbPalette[2],
                    #data = c(7.0, 6.9, 9.5, 14.5, 18.2, 21.5, 25.2, 26.5, 23.3, 18.3, 13.9, 9.6),
                    data=plotdata$value1,yAxis=1,cursor = "ns-resize",
                    draggableY = TRUE
      )%>% 
      hc_add_series(name =paste0('Loss'), type = 'line', #color = cbbPalette[1],
                    #data = c(49.9, 71.5, 106.4, 129.2, 144.0, 176.0, 135.6, 148.5, 216.4, 194.1, 95.6, 54.4))
                    data=pnorm(plotdata$loss),
                    draggableY = FALSE
      )%>%
      
      hc_add_series(name = "Correlation", type = "pie",
                    data = list(list(y = abs(cor_macro1), name = "Correlation Macro1",
                                     sliced = TRUE, color = col1)
                                ,list(y =  abs(cor_macro1), name = "Correlation Macro1",
                                      color = col2,
                                      dataLabels = list(enabled = FALSE))
                    ),
                    center = c('0%', "%"),
                    size = 50)
    
              })
output$tableoutput<-renderDataTable({data.table(a=input$inputname)})
  
  })
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(data.table)
library(stringi)
library(stringr)
shinyServer(function(input, output) {
  
 
  output$Macroplot2 <- renderHighchart({
    Current1 <- 
    Current2 <- 
    x<-FALSE
    y<-FALSE
    x<-FALSE
    y<-FALSE
    if(!is.null(input$dropdown_change1)) x<-(input$dropdown_change1!=input$changed_series_drag1)   
    if(!is.null(input$dropdown_change2)) y<-(input$dropdown_change2!=input$changed_series_drag2)
    
    if(is.null(input$timeseries1)|x)var1<-c(7.0, 6.9, 9.5, 14.5, 18.2) else var1<-input$timeseries1
    if(is.null(input$timeseries2)|y)var2<-c(24.5, 18.2, 21.5, 25.2, 26.5)else var2<-input$timeseries2
    namee<-input$name1
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
          title = list(text = namee),
          align = "right",
          showFirstLabel = FALSE,
          showLastLabel = FALSE,
          #labels = list(format = "{value} mm"),
          opposite = TRUE
        ),
        list(
          title = list(text =input$name2),
          align = "right",
          showFirstLabel = FALSE,
          showLastLabel = FALSE,
          #labels = list(format = "{value} mm"),
          opposite = TRUE
        )
      ) %>% 
      hc_plotOptions(
        series = list(
          events=list(
            afterAnimate = JS("function(){

$(document).on('change', 'input.shiny-bound-input, select.shiny-bound-input', function(e){
	 $target = $(e.target)
                              
                              console.log($target.val());
             if($target.val().substring(0,1)=='1'){ 
Shiny.onInputChange('dropdown_change1', $target.val())
             }
             if($target.val().substring(0,1)=='2'){ 
Shiny.onInputChange('dropdown_change2', $target.val())
             }
})
console.log($target.val().substring(0,1)+'substring')
            }
            ")
          ),
          #stickyTracking = FALSE,
          point = list(
            events = list(
              drop = JS("
function(){
window.data = _.map(this.series.data, function(e) { return e.y })

             if(this.series.name.substring(0,1)=='2'){ 
Shiny.onInputChange('timeseries2', data)
}

             if(this.series.name.substring(0,1)=='1'){ 
Shiny.onInputChange('timeseries1', data)
}
             if(this.series.name.substring(0,1)=='1'){ 
Shiny.onInputChange('changed_series_drag1', this.series.name)
}
             if(this.series.name.substring(0,1)=='2'){ 
Shiny.onInputChange('changed_series_drag2', this.series.name)
  }
}


    "
                        )
             
              )))) %>% 
      
      hc_add_series(name = namee, type = 'line', color = 'red',
                    #data = c(7.0, 6.9, 9.5, 14.5, 18.2, 21.5, 25.2, 26.5, 23.3, 18.3, 13.9, 9.6),
                    data=var1,yAxis=1,
                    draggableY = TRUE
                    
      )%>% 
      hc_add_series(name =input$name2, type = 'line', color = 'blue',
                    #data = c(7.0, 6.9, 9.5, 14.5, 18.2, 21.5, 25.2, 26.5, 23.3, 18.3, 13.9, 9.6),
                    data=var2,yAxis=2,
                    draggableY = TRUE
      )  %>%             
    
      hc_add_series(name =paste0('Loss'), type = 'line', color = 'black',
                    #data = c(49.9, 71.5, 106.4, 129.2, 144.0, 176.0, 135.6, 148.5, 216.4, 194.1, 95.6, 54.4))
                    data=c(1:5),
                    draggableY = FALSE)
    
    print(paste0(input$changed_series_drag1,' - Drag1'))
    print(paste0(input$changed_series_drag2,' - Drag2'))
    print(paste0(input$dropdown_change1,' - Dropdown1'))
    print(paste0(input$dropdown_change2,' - Dropdown2'))
    #print(paste0(input$timeseries1,' - Timeseries1'))
    #print(paste0(input$timeseries2,' - Timeseries2'))
    hc
    
              })
  output$name1<-renderUI({      selectInput("name1",label = "name1",choices = c("A"="1A","B"="1B","C"="1C"),selected = "1A")})
  output$name2<-renderUI({      selectInput("name2",label = "name2",choices = c("A"="2A","B"="2B","C"="2C"),selected = "2A")})  
  output$table <-renderDataTable({
    
    x<-FALSE
    y<-FALSE
    if(!is.null(input$dropdown_change1)) x<-(input$dropdown_change1!=input$changed_series_drag1)   
    if(!is.null(input$dropdown_change2)) y<-(input$dropdown_change2!=input$changed_series_drag2)
 
    if(is.null(input$timeseries1)|x)var1<-c(7.0, 6.9, 9.5, 14.5, 18.2) else var1<-input$timeseries1
    if(is.null(input$timeseries2)|y)var2<-c(24.5, 18.2, 21.5, 25.2, 26.5)else var2<-input$timeseries2
    print(x)
    #print(input$timeseries1)
    #print(input$timeseries2)

    #if(!is.null(input$changed_series_drag)) variable <-str_sub(input$changed_series_drag,1,1)  else variable<-"exists_not"
    #if(variable=="1") var1 <- input$timeseries else if(variable=="2")var2<-input$timeseries
  #  if(str_sub(input$changed_series_drag,1,1)=="2") var2 <- input$timeseries
    #print(variable)
    #print(paste0('Dropdown-Change - ',input$dropdown_change)) #changed due to dropdown
    #print(paste0('Dragging-Change - ',input$changed_series_drag)) #changed by dragging
    #print(paste0('Reset 1 - ',x))
    #print(paste0('Reset 2 - ',y))
    #print(input$timeseries1) #data
    #print(input$timeseries2) #data
    #var <- var[(length(var)-input$n.ahead+1):length(var)]
    #Output from graph
    
  table <-  data.table(var1,var2)#Here I want to use the dragged data. something linke input$highchart$... should do the trick I guess...
  names(table)<-c(input$name1,input$name2)
  table
    })
  output$table2 <- renderTable({


    table('a')
  })
  
  })
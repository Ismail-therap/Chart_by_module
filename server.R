library(shiny)
library(xts)
library(dygraphs)
library(googleVis)


source("Service/google_vis_chart.R")


shinyServer(
  
  function(input, output) {
    
    chart_input1<-reactive({
      
      google_vis_line_chart(Table_name_from_db = input$data, Table_type=input$Time_zone_option, inidate = input$date)
      
    })
    
    chart_input2<-reactive({
      
      dychart_chart_by_date(Table_name_from_db = input$data, Table_type=input$Time_zone_option, inidate = input$date)
      
    })
    
    output$Linechart<- renderGvis({
      chart_input1()
    })
    
    output$Linechart_by_date<- renderDygraph({
      chart_input2()
    })
    
  })  


#runApp(host="192.168.5.16",port=5050)

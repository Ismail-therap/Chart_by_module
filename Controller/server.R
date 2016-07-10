library(shiny)
library(googleVis)
source("Service/google_vis_chart.R")


ispdata <- eastern_data_isp
billdata <- eastern_data_bill
mar <- eastern_data_mar
att <- eastern_data_att
ger <- eastern_data_ger
scMail <- eastern_data_smail


ispdataLocal <- local_data_isp
billdataLocal <- local_data_bill
marLocal <- local_data_mar
attLocal <- local_data_att
gerLocal <- local_data_ger
scMailLocal <- local_data_smail


Eastern_tz <- Combined_eastern
local_tz <- Combined_Local


shinyServer(
  
  function(input, output) {
    
    chart_input<-reactive({
      
      if(input$Time_zone_option == "Eastern"){
        inputdat = switch(input$data,
                          "ISP" = ispdata,
                          "Bill" = billdata,
                          "Mar" = mar,
                          "Attendance" = att,
                          "GER" = ger,
                          "scMail" = scMail, 
                          "Combined" = Eastern_tz)
    
        google_vis_line_chart(inputdat = inputdat,inputdate = input$date)
         
      }
        else if (input$Time_zone_option == "Local"){
          inputdat = switch(input$data,
                          "ISP" = ispdataLocal,
                          "Bill" = billdataLocal,
                          "Mar" = marLocal,
                          "Attendance" = attLocal,
                          "GER" = gerLocal,
                          "scMail" = scMailLocal,
                          "Combined" = local_tz)
          
          google_vis_line_chart(inputdat = inputdat,inputdate = input$date)
        }
          })
   
    output$Linechart<- renderGvis({
      chart_input()
      })
    
  })  

#runApp(host="192.168.5.16",port=5050)

#Helo

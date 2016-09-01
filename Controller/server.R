library(shiny)
library(xts)
library(dygraphs)
library(googleVis)


source("Service/google_vis_chart.R")


shinyServer(
  
  function(input, output) {
    
    
    output$ui <- renderUI({
      
       # if (is.null(input$input_type))
       #   return()
      
      # Depending on input$input_type, we'll generate a different
      # UI component and send it to the client.
      
      switch(input$input_type,
             "Desktop" =   radioButtons("data",label = h3("Choose the data"),
                                        choices = list("ISP_DATA_DUMP_COUNT",
                                                       "ATTENDANCE_DUMP_COUNT",
                                                       "GER_DUMP_COUNT",
                                                       "SC_MAILBOX_DUMP_COUNT",
                                                       "MAR_DUMP_COUNT",
                                                       "LB_LOG_BOOK_DUMP_COUNT",
                                                       "BILL_DATA_DUMP_COUNT"),
                                        selected = "MAR_DUMP_COUNT"),
            
             "Mobile" = radioButtons("data",label = h3("Choose the data"),
                                     choices = list("ISP_MOBILE_COUNT",
                                                    "ANDROID_APP_LOGIN_COUNT"),
                                     selected = "ISP_MOBILE_COUNT")
      )
    
      })
    

    
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


#runApp(host="192.168.5.16",port=5819)

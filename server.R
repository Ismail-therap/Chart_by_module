setwd("E:/Assignments/Chart_by_module_data_from_database_Calculation_in R")

library(shiny)
library(xts)
library(dygraphs)
library(googleVis)
library(plotly)


source("Service/google_vis_chart.R")
source("Service/comparison_bar_plot.R")
source("Service/comparison_line_chart1.R")
source("Service/comparison_line_chart2.R")
source("Service/creat_geo_chart.R")


data_ind = data.frame(
  Device = c(rep("Mobile",16),rep("Desktop",7)), 
  Data = c("ANDROID_APP_ISP_COUNT",
           "ANDROID_APP_LOGIN_COUNT",
           "ANDROID_APP_MAR_COUNT",
           "ANDROID_BROWSER_LOGIN_COUNT",
           "ANDROID_BROWSER_MAR_COUNT",
           "ANDROID_BROWSER_ISP_COUNT",
           "ANDROID_BROWSER_TLOG_COUNT",
           "ANDROID_BROWSER_SCOMM_COUNT",
           "ANDROID_BROWSER_BILL_COUNT",
           "ANDROID_BROWSER_ATTEND_COUNT",
           "IOS_APP_LOGIN_COUNT",
           "IOS_APP_TLOG_COUNT",
           "IOS_APP_PSWD_RESET_COUNT",
           "IOS_BROWSER_LOGIN_COUNT",
           "IOS_BROWSER_MAR_COUNT",
           "IOS_BROWSER_ISP_COUNT",
           "ISP_DATA_DUMP_COUNT",
           "ATTENDANCE_DUMP_COUNT",
           "GER_DUMP_COUNT",
           "SC_MAILBOX_DUMP_COUNT",
           "MAR_DUMP_COUNT",
           "LB_LOG_BOOK_DUMP_COUNT",
           "BILL_DATA_DUMP_COUNT"),
  stringsAsFactors = FALSE
)


shinyServer(
  
 
  
  function(input, output) {
    
    
    # data <- reactive({
    #   validate(
    #     need(input$date != "", "Please select suitable date!")
    #   )
    #   get(input$data, 'package:datasets')
    # })
    
    #output$Analysis = renderUI(selectInput("Analysis_type","select analysis",c("Explore the module","Compare between modules","pick one"),"pick one"))
    
    output$Device = renderUI(
      
      if (is.null(input$Analysis_type))
        {return()}
      else
        
      selectInput("device","Device",c(unique(data_ind$Device),"All"),unique(data_ind$Device)[1])
      )
    
    

  #### Data to compare the modle (Data A) ####
    
    
    output$DataA <- renderUI({
      
      if (is.null(input$device))
      {return()}
      
      else if (input$device=="All")
      {
       
               
        selectInput("dataA","Module",c(unique(data_ind$Data)),unique(data_ind$Data)[2])
               
        
        
      }
      else {

                selectInput("dataA", "Module", 
                            c(unique(data_ind$Data[which(data_ind$Device == input$device)])),
                            unique(data_ind$Data[which(data_ind$Device == input$device)])[2])

      }
      
      
    })

    
    
    #### Data to compare the modle (Data B) ####
    
    output$DataB <- renderUI({
      
       if (is.null(input$device))
       {return()}
      
      else if (input$device=="All")
      {
        switch(input$Analysis_type,
               
               "Compare between modules" =  selectInput("dataB", 
                                                        "Module", 
                                                        c(unique(data_ind$Data)),
                                                        unique(data_ind$Data)[3])
               
        ) 
        
      }
      else {
        
        switch(input$Analysis_type,
               
               "Compare between modules" =  selectInput("dataB", 
                                                        "Module", 
                                                        c(unique(data_ind$Data[which(data_ind$Device == input$device)])),
                                                        unique(data_ind$Data[which(data_ind$Device == input$device)])[3])
               
        )
        
      }
      

    })
    
    
output$text <- renderUI({
  
  switch(input$Analysis_type,
         
         "Compare between modules" =  h4("To")
  ) 
})
    
######## Generating the outputs ###########


    chart_input1<-reactive({
      
      if (is.null(input$dataA)||is.null(input$date))
        return()
      
      google_vis_line_chart(Table_name_from_db = input$dataA, Table_type=input$Time_zone_option, inidate = input$date, chart_for = input$Chart_for)
      
    })

    chart_input_state<-reactive({
  
     if (is.null(input$dataA)||is.null(input$date))
      return()
  
      geo_chart_by_state(Table_A = input$dataA, inidate = input$date, chart_for = input$Chart_for)
  
    })
    
    chart_input2<-reactive({
      
      if (is.null(input$dataA)||is.null(input$date))
        return()
      
      dychart_chart_by_date(Table_name_from_db = input$dataA, Table_type=input$Time_zone_option, inidate = input$date, chart_for = input$Chart_for)
      
    })
    
    chart_input3 <- reactive({
      
      if (is.null(input$dataA)||is.null(input$dataB)||is.null(input$date))
        return()
      
      bar_plot2(Table_A=input$dataA,Table_B=input$dataB,Table_type=input$Time_zone_option,inidate = input$date,chart_for = input$Chart_for) 
      
    })
    
    chart_input4 <- reactive({
      
      if (is.null(input$dataA)||is.null(input$dataB)||is.null(input$date))
        return()
      
      google_vis_line_chart_for_comparison(Table_A=input$dataA,Table_B=input$dataB,Table_type=input$Time_zone_option,inidate = input$date,chart_for = input$Chart_for) 
      
    })
    
    
    chart_input5 <- reactive({
      
      if (is.null(input$dataA)||is.null(input$dataB)||is.null(input$date))
        return()
      
      google_vis_line_chart_for_comparison_date(Table_A=input$dataA,Table_B=input$dataB,Table_type=input$Time_zone_option,inidate = input$date,chart_for = input$Chart_for) 
      
    })

  
        
        output$Linechart<- renderGvis({
         
                 "Explore the module" = chart_input1()
          
        })
        
        output$geo_chart<- renderGvis({
          
          "Explore the module" = chart_input_state()
          
        })
        
        output$Linechart_by_date<- renderDygraph({
          
                 "Explore the module" = chart_input2()
          
        }) 
        
        output$bar_chart<- renderPlotly({
          
                 "Compare between modules" = chart_input3()
          
        })
        
        output$line_chart_comp1<- renderGvis({
          
                 "Compare between modules" = chart_input4()
          
        })
        
        
        output$line_chart_comp2<- renderGvis({
          
                 "Compare between modules" = chart_input5()
          
        })

  
        
        # observe({
        #   # use tabsetPanel 'id' argument to change tabs
        #   compare_mod <- input$Analysis_type == "Compare between modules"
        # 
        #   if (compare_mod)
        #   {
        #     updateTabsetPanel(session, "tabs", selected = "panel2")
        #   }
        # 
        #   else {
        #     updateTabsetPanel(session, "tabs", selected = "panel1")
        #   }
        # })
        
        
        
          
  })  







#runApp(host="192.168.5.16",port=5819)



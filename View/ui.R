
shinyUI(
  
  
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        
        radioButtons("input_type", 
                     label = h3("Select Device"), 
                     choices = c("Desktop","Mobile"),
                     selected = "Desktop"),
        
        uiOutput("ui"),
        
        radioButtons("Time_zone_option", label = h3("Choose the time zone"),
                     choices = list("Eastern","Local"),
                     selected ="Eastern"),
        
        dateInput("date", label = h3("Select Date"),
                  value = "2015-01-01")
        

      ),
      
      
      mainPanel(
        fluidRow(
          column(12, htmlOutput("Linechart")),
          column(10, dygraphOutput("Linechart_by_date"))
        )
        
      )
    )
  )
  
)

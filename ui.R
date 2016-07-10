
shinyUI(fluidPage(
  sidebarLayout(
    sidebarPanel(
      
      radioButtons("data",label = h3("Choose the data"),
                   choices = list("ISP","Bill","Mar","Attendance","GER","scMail","Combined"),
                   selected ="Combined"),
      
      radioButtons("Time_zone_option", label = h3("Choose the time zone"),
                   choices = list("Eastern","Local"),
                   selected ="Eastern"),
      
      dateInput("date", label = h3("Select Date"),
                value = "2016-03-10")
      
      
  
      ),
    
    mainPanel(
      htmlOutput("Linechart")
      )
  )
)
)


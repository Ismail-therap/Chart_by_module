
shinyUI(fluidPage(
  sidebarLayout(
    sidebarPanel(
      
      radioButtons("data",label = h3("Choose the data"),
                   choices = list("ISP_DATA_DUMP_COUNT",
                                  "ATTENDANCE_DUMP_COUNT",
                                  "GER_DUMP_COUNT",
                                  "SC_MAILBOX_DUMP_COUNT",
                                  "MAR_DUMP_COUNT",
                                  "LB_LOG_BOOK_DUMP_COUNT",
                                  "BILL_DATA_DUMP_COUNT"),
                   selected ="MAR_DUMP_COUNT"),
      
       radioButtons("Time_zone_option", label = h3("Choose the time zone"),
                    choices = list("Eastern","Local"),
                    selected ="Eastern"),
      
      dateInput("date", label = h3("Select Date"),
                value = "2011-01-01")

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





shinyUI(
  
  
  fluidPage(
    #sidebarLayout(
      wellPanel(
        id = "leftPanel",
        tags$style(type="text/css", '#leftPanel { height:170px; float:middle;}'),
        

        fluidRow(
          column(2,radioButtons("Analysis_type", 
                                label = "Analysis types", 
                                choices = c("Explore the module","Compare between modules"), selected = "Explore the module")),
          column(
            1, uiOutput("Device")
          ),
          column(1,selectInput("Time_zone_option", label = "Time zone",
                               choices = list("Eastern","Local"),
                               selected ="Eastern")),
          column(1,dateInput("date", label = "Starting Date",
                             value = "2016-05-05")),
          column(2,selectInput("Chart_for", label = "Period",
                               choices = list("selected date","consecutive 7 days","consecutive 30 days","consecutive 365 days"),
                               selected ="selected date"))
        ),

        fluidRow(
          column(
            2, uiOutput("DataA")
          ),
         column(1,uiOutput("text")),
          column(
            2, uiOutput("DataB")
          )

        )
      ),
      

      # mainPanel(
      #   tabsetPanel(id="tabs",
      #     tabPanel(title="Explore the module",value="panel1",
      #              fluidRow(
      #                column(6, htmlOutput("Linechart")),
      #                column(6, dygraphOutput("Linechart_by_date"))
      #              )),
      #     
      #     
      #     tabPanel(title="Compare between modules",value="panel2",
      #              fluidRow(
      #                column(6, plotlyOutput("bar_chart")),
      #                column(6, htmlOutput("line_chart_comp1")),
      #                column(6, htmlOutput("line_chart_comp2"))
      #              ))
      #     
      #   )
      # )

      
      mainPanel(
        
          # wellPanel(
          #   id = "output_panel",
          #   tags$style(type="text/css", '#output_panel { width:1000px; height:500px; float:middle;}'),
          
          conditionalPanel(
            condition = "input.Analysis_type == 'Explore the module'",
            fluidRow(
              column(8, htmlOutput("Linechart")),
              column(4,h4("Counts by State"), htmlOutput("geo_chart")),
              column(6, dygraphOutput("Linechart_by_date")))
          ),
          
          conditionalPanel(
            condition = "input.Analysis_type == 'Compare between modules'",
            fluidRow(
              column(6, plotlyOutput("bar_chart")),
              column(6, htmlOutput("line_chart_comp1")),
              column(6, htmlOutput("line_chart_comp2")))
          )                 
        #)
      )
      

  )
  
)

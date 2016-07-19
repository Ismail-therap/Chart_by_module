# source("Model/Data_from_Database.R")
# source("Model/data_preperation_new.R")
source("Model/aggrigating_subsetted_data.R")

google_vis_line_chart <- function(Table_name_from_db,Table_type,inidate){
  
  library(googleVis)
  ####### Loading data from Model #############################################
  
  single_day_data <- aggrigated_usage_count_by_t_interval(Table_name_from_db,Table_type,inidate,period = 1)
  seven_day_data <- aggrigated_usage_count_by_t_interval(Table_name_from_db,Table_type,inidate,period = 2)
  a_month_data <- aggrigated_usage_count_by_t_interval(Table_name_from_db,Table_type,inidate,period = 3)
  a_year_data <- aggrigated_usage_count_by_t_interval(Table_name_from_db,Table_type,inidate,period = 4)
  

  
  
  
  ######## Chart options #####################################################
  isolate({
  vertical_axis = "[{viewWindowMode:'explicit',viewWindow:{min:0}}]"
  
  width = 600
  hgt = 300
  
  options_single_day = list(title = "For a single day", vAxes = vertical_axis, width = width, height = hgt)
  options_seven_day = list(title = "For seven days", vAxes = vertical_axis, width = width, height = hgt)
  options_month = list(title = "For a month", vAxes = vertical_axis, width = width, height = hgt)
  options_year = list(title = "For a year", vAxes = vertical_axis, width = width, height = hgt)
  })


  
  ###### Creating gvis objects ################################################
  single_day <- gvisLineChart(single_day_data, xvar = "Time", option = options_single_day)
  seven_day <- gvisLineChart(seven_day_data, xvar = "Time", option = options_seven_day)
  for_a_month <- gvisLineChart(a_month_data, xvar = "Time", option = options_month)
  for_a_year <- gvisLineChart(a_year_data, xvar = "Time", option = options_year)
  

  ##### Merging the charts ###################################################
  merged.output.chart1 <- gvisMerge(single_day, seven_day, horizontal = T)
  merged.output.chart2 <- gvisMerge(for_a_month,for_a_year, horizontal = T)
  merged.output.chart_by_t_interval <- gvisMerge(merged.output.chart1,merged.output.chart2, horizontal = FALSE)
  return(merged.output.chart_by_t_interval)
  
}




dychart_chart_by_date <- function(Table_name_from_db,Table_type,inidate){

  
  library(dygraphs)
  library(xts)
  ####### Loading data from Model #############################################
  a_year_data_by_date <- aggrigated_usage_count_by_date(Table_name_from_db,Table_type,inidate,period = 4)

  dat <- as.xts(
    a_year_data_by_date[-1]
    , order.by = as.Date(
      paste0(a_year_data_by_date$Date,format="%Y-%m-%d")
    )
  )
  
  plotting_object <- dygraph(dat,main = "Total count by date") %>% 
    dyAxis("y", label = "Total count") %>%
    dyAxis("x", label = "Date") %>%
    dyHighlight(highlightSeriesOpts = list(strokeWidth = 1))%>%
    dyOptions(fillGraph = F, drawGrid = F)%>%
    dyLegend(width = 600)%>%
    dyRangeSelector(height=20)
  
  return(plotting_object)

  }









#done

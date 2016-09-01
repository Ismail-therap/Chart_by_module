


source("Model/aggrigating_subsetted_data.R")



get_table_A_date <- function(Table_A,Table_type,inidate, chart_for ){
  
  if(chart_for == "selected date") {
    
    Table_name_from_db <- Table_A
    data_TableA <- aggrigated_usage_count_by_date(Table_name_from_db,Table_type,inidate,period = 1)
    
    return(data_TableA)
  }
  
  
  else if (chart_for == "consecutive 7 days") {
    Table_name_from_db <- Table_A
    data_TableA <- aggrigated_usage_count_by_date(Table_name_from_db,Table_type,inidate,period = 2)
    
    return(data_TableA)
  }
  else if (chart_for == "consecutive 30 days") {
    Table_name_from_db <- Table_A
    data_TableA <- aggrigated_usage_count_by_date(Table_name_from_db,Table_type,inidate,period = 3)
    
    return(data_TableA)
  }
  else if (chart_for == "consecutive 365 days") {
    Table_name_from_db <- Table_A
    data_TableA <- aggrigated_usage_count_by_date(Table_name_from_db,Table_type,inidate,period = 4)
    
    return(data_TableA)
  }
  
  
}



google_vis_line_chart_for_comparison_date <- function(Table_A,Table_B,Table_type,inidate, chart_for ){
  
  data_TableA <- get_table_A_date(Table_A,Table_type,inidate, chart_for)
  
  if(chart_for == "selected date") {
    
    
    
    Table_name_from_db <- Table_B
    data_TableB <- aggrigated_usage_count_by_date(Table_name_from_db,Table_type,inidate,period = 1)
    
    max_val <- c(max(data_TableA[,2:5]), max(data_TableB[,2:5]))
    
    isolate({
      ymax <- max(max_val)
      vertical_axis = paste("[{ viewWindowMode:'explicit', viewWindow:{min:0, max:",ymax,"}}]",sep="")
      options_TableA = list(title = Table_A, vAxes = vertical_axis, width = 600, height = 300)
      options_TableB = list(title = Table_B, vAxes = vertical_axis, width = 600, height = 300)
      
    })
    
    TableA <- gvisLineChart(data_TableA, xvar = "Date", option = options_TableA)
    TableB <- gvisLineChart(data_TableB, xvar = "Date", option = options_TableB)
    
    
    
    merged.output.chart <- gvisMerge(TableA,TableB, horizontal = T)
    return(merged.output.chart)
  }
  
  
  else if (chart_for == "consecutive 7 days") {
    
    
    Table_name_from_db <- Table_B
    data_TableB <- aggrigated_usage_count_by_date(Table_name_from_db,Table_type,inidate,period = 2)
    
    max_val <- c(max(data_TableA[,2:5]), max(data_TableB[,2:5]))
    
    isolate({
      ymax <- max(max_val)
      vertical_axis = paste("[{ viewWindowMode:'explicit', viewWindow:{min:0, max:",ymax,"}}]",sep="")
      options_TableA = list(title = Table_A, vAxes = vertical_axis, width = 600, height = 300)
      options_TableB = list(title = Table_B, vAxes = vertical_axis, width = 600, height = 300)
      
    })
    
    TableA <- gvisLineChart(data_TableA, xvar = "Date", option = options_TableA)
    TableB <- gvisLineChart(data_TableB, xvar = "Date", option = options_TableB)
    
    
    
    merged.output.chart <- gvisMerge(TableA,TableB, horizontal = T)
    return(merged.output.chart)
  }
  else if (chart_for == "consecutive 30 days") {
    
    
    Table_name_from_db <- Table_B
    data_TableB <- aggrigated_usage_count_by_date(Table_name_from_db,Table_type,inidate,period = 3)
    
    max_val <- c(max(data_TableA[,2:5]), max(data_TableB[,2:5]))
    
    isolate({
      ymax <- max(max_val)
      vertical_axis = paste("[{ viewWindowMode:'explicit', viewWindow:{min:0, max:",ymax,"}}]",sep="")
      options_TableA = list(title = Table_A, vAxes = vertical_axis, width = 600, height = 300)
      options_TableB = list(title = Table_B, vAxes = vertical_axis, width = 600, height = 300)
      
    })
    
    TableA <- gvisLineChart(data_TableA, xvar = "Date", option = options_TableA)
    TableB <- gvisLineChart(data_TableB, xvar = "Date", option = options_TableB)
    
    
    
    merged.output.chart <- gvisMerge(TableA,TableB, horizontal = T)
    return(merged.output.chart)
  }
  else if (chart_for == "consecutive 365 days") {
    
    
    Table_name_from_db <- Table_B
    data_TableB <- aggrigated_usage_count_by_date(Table_name_from_db,Table_type,inidate,period = 4)
    
    max_val <- c(max(data_TableA[,2:5]), max(data_TableB[,2:5]))
    
    isolate({
      ymax <- max(max_val)
      vertical_axis = paste("[{ viewWindowMode:'explicit', viewWindow:{min:0, max:",ymax,"}}]",sep="")
      options_TableA = list(title = Table_A, vAxes = vertical_axis, width = 600, height = 300)
      options_TableB = list(title = Table_B, vAxes = vertical_axis, width = 600, height = 300)
      
    })
    
    TableA <- gvisLineChart(data_TableA, xvar = "Date", option = options_TableA)
    TableB <- gvisLineChart(data_TableB, xvar = "Date", option = options_TableB)
    
    
    
    merged.output.chart <- gvisMerge(TableA,TableB, horizontal = T)
    return(merged.output.chart)
  }
  
  
}


# data <- aggrigated_usage_count_by_date("MAR_DUMP_COUNT","Eastern","2014-06-06",period = 1)
# head(data)
# 
# min(data[,2:5])
# 
# 
# a <- google_vis_line_chart_for_comparison_date(Table_A="ATTENDANCE_DUMP_COUNT",Table_B="MAR_DUMP_COUNT","Eastern","2014-06-06",chart_for = "consecutive 365 days")
# 
 #plot(a)

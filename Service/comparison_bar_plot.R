#install.packages("plotly")
library(plotly)
source("Model/aggrigating_subsetted_data.R")
bar_plot1 <- function(Table_A,Table_type,inidate,chart_for) {
  

  Table_name_from_db <- Table_A


  if(chart_for == "selected date") {
    a_single_data_by_date <- aggrigated_usage_count_by_date(Table_name_from_db,Table_type,inidate,period = 1)
    dat_A <- a_single_data_by_date
  }
  
  else if (chart_for == "consecutive 7 days") {
    a_seven_data_by_date <- aggrigated_usage_count_by_date(Table_name_from_db,Table_type,inidate,period = 2)
    dat_A <- a_seven_data_by_date
  }
  
  else if (chart_for == "consecutive 30 days") {
    a_month_data_by_date <- aggrigated_usage_count_by_date(Table_name_from_db,Table_type,inidate,period = 3)
    dat_A <- a_month_data_by_date
  }
  
  else if (chart_for == "consecutive 365 days") {
    
    a_year_data_by_date <- aggrigated_usage_count_by_date(Table_name_from_db,Table_type,inidate,period = 4)
    dat_A <- a_year_data_by_date
  }
  

  if(Table_type=="Eastern") {
    CENTRAL <- sum(dat_A[,2])
    MOUNTAIN <- sum(dat_A[,3])
    EASTERN <- sum(dat_A[,4])
    PACIFIC <- sum(dat_A[,5])
    
  }
  
  else {
    
    CENTRAL <- sum(dat_A[,3])
    MOUNTAIN <- sum(dat_A[,4])
    EASTERN <- sum(dat_A[,2])
    PACIFIC <- sum(dat_A[,5])
  }
  
  Time_zone = c("CENTRAL","MOUNTAIN","EASTERN","PACIFIC")
  Counts = c(CENTRAL,MOUNTAIN,EASTERN,PACIFIC)
  
  
  p <- plot_ly(
    x = Time_zone,
    y = Counts,
    name = Table_A,
    type = "bar")
  return(p)

}




bar_plot2 <- function(Table_A,Table_B,Table_type,inidate,chart_for) {
  
  p <- bar_plot1(Table_A,Table_type,inidate,chart_for)
  
   Table_name_from_db <- Table_B 

  if(chart_for == "selected date") {
    a_single_data_by_date <- aggrigated_usage_count_by_date(Table_name_from_db,Table_type,inidate,period = 1)
    dat_A <- a_single_data_by_date
  }
  
  else if (chart_for == "consecutive 7 days") {
    a_seven_data_by_date <- aggrigated_usage_count_by_date(Table_name_from_db,Table_type,inidate,period = 2)
    dat_A <- a_seven_data_by_date
  }
  
  else if (chart_for == "consecutive 30 days") {
    a_month_data_by_date <- aggrigated_usage_count_by_date(Table_name_from_db,Table_type,inidate,period = 3)
    dat_A <- a_month_data_by_date
  }
  
  else if (chart_for == "consecutive 365 days") {
    
    a_year_data_by_date <- aggrigated_usage_count_by_date(Table_name_from_db,Table_type,inidate,period = 4)
    dat_A <- a_year_data_by_date
  }

  
  if(Table_type=="Eastern") {
    CENTRAL <- sum(dat_A[,2])
    MOUNTAIN <- sum(dat_A[,3])
    EASTERN <- sum(dat_A[,4])
    PACIFIC <- sum(dat_A[,5])
    
  }
  
  else {
    
    CENTRAL <- sum(dat_A[,3])
    MOUNTAIN <- sum(dat_A[,4])
    EASTERN <- sum(dat_A[,2])
    PACIFIC <- sum(dat_A[,5])
  }
  
  Time_zone = c("CENTRAL","MOUNTAIN","EASTERN","PACIFIC")
  Counts = c(CENTRAL,MOUNTAIN,EASTERN,PACIFIC)

  p2 <- add_trace(
    p,
    x =Time_zone,
    y = Counts,
    name = Table_B,
    type = "bar")
  return(p2)
  
  
}




#bar_plot2(Table_A="MAR_DUMP_COUNT",Table_B="ATTENDANCE_DUMP_COUNT","Eastern","2014-06-06",chart_for = "consecutive 7 days")






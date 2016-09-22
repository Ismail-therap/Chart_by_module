
source("Model/Subsetted_data_from_database.R")


##### Shifting the row by time difference

row_shift <- function(x, time_in_min = -60) {
  
  shiftLen <- time_in_min/15
  r <- (1L + shiftLen):(length(x) + shiftLen)
  r[r<1] <- NA
  return(x[r])
}


##########

data_from_sql_data_base <- function(Table_name_from_db, Table_type, inidate, period) {
  
  dat <- Eastern_data_from_SQL_data_base(Table_name_from_db, inidate, period)
  
  validate(need(nrow(dat)!=0,"No data to show!")) # Handling the error output!!!
  
  
  dat$CENTRAL_LOCAL <- row_shift(dat$CENTRAL, -60)
  dat$MOUNTAIN_LOCAL <- row_shift(dat$MOUNTAIN, -120)
  dat$PACIFIC_LOCAL <- row_shift(dat$PACIFIC, -180)
  
  dat <- dat[-c(1:96),]

  if (Table_name_from_db == "COMBINED_MODULE") {
    
    Table_name_from_db = "COMBINED_MODULE"
    
    if (Table_type == "Eastern") {
      
      Table_type == "Eastern"
      
      dat <- dat[,1:6]
      
      dat$COMBINED_ALL_TZ_EASTERN <- dat$CENTRAL + dat$MOUNTAIN + dat$PACIFIC + dat$EASTERN
      
      
      return(dat)
    }
    
    else {
      
      dat <- dat[,c(1,2,5,7:9)]
      colnames(dat)[2] <- "EASTERN_LOCAL" 
      
      dat$COMBINED_ALL_TZ_LOCAL <- dat$CENTRAL_LOCAL + dat$MOUNTAIN_LOCAL + dat$PACIFIC_LOCAL + dat$EASTERN_LOCAL

      return(dat)
      
    }
    
  }
  
  
  else {
    
    if (Table_type == "Eastern") {
      
      Table_type == "Eastern"
      
      dat <- dat[,1:6]
      
      return(dat)
    }
    
    else {
      
      dat <- dat[,c(1,2,5,7:9)]
      colnames(dat)[3] <- "EASTERN_LOCAL" 
      
      return(dat)
      
    }

  }

}



###################################################################################################################

aggrigated_usage_count_by_t_interval <- function(Table_name_from_db,Table_type,inidate,period){
  
  susetted_data <- data_from_sql_data_base(Table_name_from_db,Table_type,inidate,period)
  
  validate(need(nrow(susetted_data) !=0,"No data to show!")) # Handling the error output!!!
  
  if(ncol(susetted_data) > 6) {
    susetted_data_by_t_interval <- aggregate(susetted_data[,3:7],by = list(susetted_data$T_INTERVAL),FUN = sum)

  }
  
  else{
    susetted_data_by_t_interval <- aggregate(susetted_data[,3:6],by = list(susetted_data$T_INTERVAL),FUN = sum)

  }
  
  colnames(susetted_data_by_t_interval)[1] <- "Time"

  return(susetted_data_by_t_interval)

}




##################################################################################################################


aggrigated_usage_count_by_date <- function(Table_name_from_db,Table_type,inidate,period){
  
  susetted_data <- data_from_sql_data_base(Table_name_from_db,Table_type,inidate,period)
  validate(need(nrow(susetted_data) !=0,"No data to show!")) # Handling the error output!!!
  
  
  if(ncol(susetted_data) > 6) {
    susetted_data_by_date <- aggregate(susetted_data[,3:7],by = list(susetted_data$EVENT_DATE),FUN = sum)
    
  }
  
  else{
    susetted_data_by_date <- aggregate(susetted_data[,3:6],by = list(susetted_data$EVENT_DATE),FUN = sum)
    #susetted_data_by_date$Total <- susetted_data_by_date[,3]+susetted_data_by_date[,4]+susetted_data_by_date[,5]+susetted_data_by_date[,6]
    
    
  }
  
  colnames(susetted_data_by_date)[1] <- "Date"
  
  return(susetted_data_by_date)
  
}









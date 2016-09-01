#ROracle
library(DBI)
library(ROracle)
drv <- dbDriver("Oracle")

#host <- "192.168.49.112"
#host <- "192.168.49.107"
host <- "192.168.49.105"


port <- 1521

#sid <- "bidb"
sid <- "therap"

connect.string <- paste("(DESCRIPTION = ", 
                        "(ADDRESS = (PROTOCOL = tcp) (HOST = ", host, ") (PORT = ", port, "))",
                        "(CONNECT_DATA = (SID = ", sid, ")))",
                        sep = "")

#ROracle_con <- dbConnect(drv, username = "bidb",password = "bidb",dbname=connect.string)
#ROracle_con <- dbConnect(drv, username = "dload",password = "Dload",dbname=connect.string)
#ROracle_con <- dbConnect(drv, username = "wdump",password = "wdump",dbname=connect.string)

ROracle_con <- dbConnect(drv, username = "BI_STAT_STORE",password = "bistatstore",dbname=connect.string)


Sys.setenv(TZ = "GMT")



######### Build query to extract data from database


build_query_for_selected_date <- function(Table_name_from_db, inidate,n_day) {
  

  selected_columns <- paste("Select DATESTAMP,STATE,
                    COUNT from")
    
  
  
  dates <- seq(as.Date(inidate),by = "days",length.out = n_day)
  Dates_in_db_format<-format(dates,format="%d-%b-%y")
  
  D1<-Dates_in_db_format[1]
  D2<-Dates_in_db_format[n_day]
  data <- paste(Table_name_from_db,"_state",sep="")
  est_quer <- paste(selected_columns,data, "WHERE (DATESTAMP BETWEEN '",D1,"' AND '",D2,"') ORDER BY DATESTAMP", sep=" ")
  
  return(est_quer)
}

#build_query_for_selected_date("ABC","2016-01-01",2)

##########################################################################################################################

finalize_sql_query_desired_period <- function(Table_name_from_db, inidate, period){
  
  inidate_main <-as.Date(inidate,"%Y-%m-%d")
  
  #inidate_dummy <- seq(as.Date(inidate_main),by = "-1 days",length.out =2)
  inidate <- inidate_main
  
  if(period == 1){
    num_day = 2
    
    est_quer <- build_query_for_selected_date(Table_name_from_db,inidate,n_day=num_day)
    
  }
  
  else if(period == 2){
    
    num_day = 8
    
    est_quer <- build_query_for_selected_date(Table_name_from_db,inidate,n_day=num_day)
  }
  
  else if(period == 3){
    library(Hmisc)
    num_day = monthDays(inidate)
    
    est_quer <- build_query_for_selected_date(Table_name_from_db,inidate,n_day=num_day)
    
  }
  
  
  else if(period == 4){
    library(Hmisc)
    num_day = yearDays(inidate)
    
    est_quer <- build_query_for_selected_date(Table_name_from_db,inidate,n_day=num_day)
    
  }
  
  
  return(est_quer)
}


##### Date extraction from database 
Data_from_SQL_data_base <- function(Table_name_from_db, inidate, period) {
  query <- finalize_sql_query_desired_period(Table_name_from_db, inidate, period) 
  dat <- dbGetQuery(ROracle_con, query)
  dat <- na.omit(dat)
  return(dat)
}

########################

#dat <- Data_from_SQL_data_base(Table_name_from_db="IOS_APP_LOGIN_COUNT_STATE", inidate="2016-04-04", period=3)
########################


state_count_by_date <- function(Table_name_from_db,inidate,period){
  
  susetted_data <- Data_from_SQL_data_base(Table_name_from_db,inidate,period)
  
  
    susetted_data1 <- aggregate(susetted_data[,3],by = list(susetted_data$DATESTAMP,susetted_data$STATE),FUN = sum)
    
    susetted_data_by_state <- aggregate(susetted_data1[,3],by = list(susetted_data1$Group.2),FUN = sum)
    
    colnames(susetted_data_by_state) <- c("State","Count")
  
 
  
  return(susetted_data_by_state)
  
}

##################################################

#data <- state_count_by_date(Table_name_from_db="IOS_APP_LOGIN_COUNT_STATE", inidate="2016-04-04", period=2)
#########


################################################


geo_chart_by_state <- function(Table_A,inidate, chart_for ){
  
  if(chart_for == "selected date") {
    
    Table_name_from_db <- Table_A
    data_TableA <- state_count_by_date(Table_name_from_db,inidate,period = 1)
    
    options_TableA = list(title = Table_A ,region="US",displayMode="regions",resolution="provinces",width=600, height=400)
    
    TableA <- gvisGeoChart(data_TableA, "State", "Count",
                           options=options_TableA)
    
    return(TableA)
  }
  
  
  else if (chart_for == "consecutive 7 days") {
    Table_name_from_db <- Table_A
    data_TableA <- state_count_by_date(Table_name_from_db,inidate,period = 2)
    
    options_TableA = list(title = Table_A ,region="US",displayMode="regions",resolution="provinces",width=600, height=400)
    
    TableA <- gvisGeoChart(data_TableA, "State", "Count",
                           options=options_TableA)
    
    return(TableA)
  }
  else if (chart_for == "consecutive 30 days") {
    Table_name_from_db <- Table_A
    data_TableA <- state_count_by_date(Table_name_from_db,inidate,period = 3)
    
    options_TableA = list(title = Table_A ,region="US",displayMode="regions",resolution="provinces",width=600, height=400)
    
    TableA <- gvisGeoChart(data_TableA, "State", "Count",
                           options=options_TableA)
    
    return(TableA)
  }
  else if (chart_for == "consecutive 365 days") {
    Table_name_from_db <- Table_A
    data_TableA <- state_count_by_date(Table_name_from_db,inidate,period = 4)
    
    options_TableA = list(title = Table_A ,region="US",displayMode="regions",resolution="provinces",width=600, height=400)
    
    TableA <- gvisGeoChart(data_TableA, "State", "Count",
                           options=options_TableA)
    
    return(TableA)
  }
  
  
}

#ot <- geo_chart_by_state(Table_A="ANDROID_APP_LOGIN_COUNT_STATE",inidate="2016-04-04", chart_for="selected date")



# D <- google_vis_line_chart_for_comparison_state(Table_A="ANDROID_APP_LOGIN_COUNT_STATE",Table_B="IOS_APP_LOGIN_COUNT_STATE",
#                                            inidate="2016-04-04", chart_for="selected date")
# plot(D)
# 
# 
# google_vis_line_chart_for_comparison_state <- function(Table_A,Table_B,inidate, chart_for ){
#   
#   data_TableA <- get_table_A_state(Table_A,inidate, chart_for)
#   
#   if(chart_for == "selected date") {
#     
#     
#     
#     Table_name_from_db <- Table_B
#     data_TableB <- state_count_by_date(Table_name_from_db,inidate,period = 1)
#     
#     # max_val <- c(max(data_TableA[,2]), max(data_TableB[,2]))
#     # 
#     # isolate({
#     #   ymax <- max(max_val)
#     #   vertical_axis = paste("[{ viewWindowMode:'explicit', viewWindow:{min:0, max:",ymax,"}}]",sep="")
#     #   options_TableA = list(title = Table_A, vAxes = vertical_axis, width = 600, height = 300)
#     #   options_TableB = list(title = Table_B, vAxes = vertical_axis, width = 600, height = 300)
#     #   
#     # })
#     
#     # TableA <- gvisLineChart(data_TableA, xvar = "Date", option = options_TableA)
#     # TableB <- gvisLineChart(data_TableB, xvar = "Date", option = options_TableB)
#     
#     isolate({
#       options_TableA = list(title = Table_A ,region="US",displayMode="regions",resolution="provinces",width=600, height=400)
#       options_TableB = list(title = Table_B ,region="US",displayMode="regions",resolution="provinces",width=600, height=400)
#       
#     })
#        
#     
#     options_TableA = list(title = Table_A ,region="US",displayMode="regions",resolution="provinces",width=600, height=400)
#     
#     TableA <- gvisGeoChart(data_TableA, "State", "Count",
#                               options=options_TableA)
# 
#     merged.output.chart <- gvisMerge(TableA,TableB, horizontal = T)
#     return(merged.output.chart)
#   }
#   
#   
#   else if (chart_for == "consecutive 7 days") {
#     
#     
#     Table_name_from_db <- Table_B
#     data_TableB <- state_count_by_date(Table_name_from_db,inidate,period = 2)
#     
#     # max_val <- c(max(data_TableA[,2:5]), max(data_TableB[,2:5]))
#     # 
#     # isolate({
#     #   ymax <- max(max_val)
#     #   vertical_axis = paste("[{ viewWindowMode:'explicit', viewWindow:{min:0, max:",ymax,"}}]",sep="")
#     #   options_TableA = list(title = Table_A, vAxes = vertical_axis, width = 600, height = 300)
#     #   options_TableB = list(title = Table_B, vAxes = vertical_axis, width = 600, height = 300)
#     #   
#     # })
#     
#     isolate({
#       options_TableA = list(title = Table_A ,region="US",displayMode="regions",resolution="provinces",width=600, height=400)
#       options_TableB = list(title = Table_B ,region="US",displayMode="regions",resolution="provinces",width=600, height=400)
#       
#     })    
#     
#     TableA <- gvisGeoChart(data_TableA, "State", "Count",
#                            options=options_TableA)
#     
#     TableB <- gvisGeoChart(data_TableB, "State", "Count",
#                            options=options_TableB)
#     
#     
#     
#     merged.output.chart <- gvisMerge(TableA,TableB, horizontal = T)
#     return(merged.output.chart)
#   }
#   else if (chart_for == "consecutive 30 days") {
#     
#     
#     Table_name_from_db <- Table_B
#     data_TableB <- state_count_by_date(Table_name_from_db,inidate,period = 3)
#     
#     # max_val <- c(max(data_TableA[,2:5]), max(data_TableB[,2:5]))
#     # 
#     # isolate({
#     #   ymax <- max(max_val)
#     #   vertical_axis = paste("[{ viewWindowMode:'explicit', viewWindow:{min:0, max:",ymax,"}}]",sep="")
#     #   options_TableA = list(title = Table_A, vAxes = vertical_axis, width = 600, height = 300)
#     #   options_TableB = list(title = Table_B, vAxes = vertical_axis, width = 600, height = 300)
#     #   
#     # })
#     # 
#     # TableA <- gvisLineChart(data_TableA, xvar = "Date", option = options_TableA)
#     # TableB <- gvisLineChart(data_TableB, xvar = "Date", option = options_TableB)
#     
#     
#     isolate({
#       options_TableA = list(title = Table_A ,region="US",displayMode="regions",resolution="provinces",width=600, height=400)
#       options_TableB = list(title = Table_B ,region="US",displayMode="regions",resolution="provinces",width=600, height=400)
#       
#     })
#     
#     TableA <- gvisGeoChart(data_TableA, "State", "Count",
#                            options=options_TableA)
#     
#     TableB <- gvisGeoChart(data_TableB, "State", "Count",
#                            options=options_TableB)
#     
#     
#     
#     merged.output.chart <- gvisMerge(TableA,TableB, horizontal = T)
#     return(merged.output.chart)
#   }
#   else if (chart_for == "consecutive 365 days") {
#     
#     
#     Table_name_from_db <- Table_B
#     data_TableB <- state_count_by_date(Table_name_from_db,inidate,period = 4)
#     
#     # max_val <- c(max(data_TableA[,2:5]), max(data_TableB[,2:5]))
#     # 
#     # isolate({
#     #   ymax <- max(max_val)
#     #   vertical_axis = paste("[{ viewWindowMode:'explicit', viewWindow:{min:0, max:",ymax,"}}]",sep="")
#     #   options_TableA = list(title = Table_A, vAxes = vertical_axis, width = 600, height = 300)
#     #   options_TableB = list(title = Table_B, vAxes = vertical_axis, width = 600, height = 300)
#     #   
#     # })
#     # 
#     # TableA <- gvisLineChart(data_TableA, xvar = "Date", option = options_TableA)
#     # TableB <- gvisLineChart(data_TableB, xvar = "Date", option = options_TableB)
#     
#     isolate({
#       options_TableA = list(title = Table_A ,region="US",displayMode="regions",resolution="provinces",width=600, height=400)
#       options_TableB = list(title = Table_B ,region="US",displayMode="regions",resolution="provinces",width=600, height=400)
#     })
#     
#     TableA <- gvisGeoChart(data_TableA, "State", "Count",
#                            options=options_TableA)
#     
#     TableB <- gvisGeoChart(data_TableB, "State", "Count",
#                            options=options_TableB)
#     
#     
#     merged.output.chart <- gvisMerge(TableA,TableB, horizontal = T)
#     return(merged.output.chart)
#   }
#   
#   
# }

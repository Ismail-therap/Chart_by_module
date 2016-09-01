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



# ######### Build query to extract data from database
# 
# 
# build_query_for_selected_date <- function(Table_name_from_db, inidate,n_day) {
#   
#   
#   selected_columns <- paste("Select DATESTAMP,STATE,
#                             COUNT from")
#   
#   
#   
#   dates <- seq(as.Date(inidate),by = "days",length.out = n_day)
#   Dates_in_db_format<-format(dates,format="%d-%b-%y")
#   
#   D1<-Dates_in_db_format[1]
#   D2<-Dates_in_db_format[n_day]
#   
#   data <- paste(Table_name_from_db,"_state",sep="")
#   est_quer <- paste(selected_columns,data, "WHERE (DATESTAMP BETWEEN '",D1,"' AND '",D2,"') ORDER BY DATESTAMP", sep=" ")
#   return(est_quer)
# }
# 
# ##########################################################################################################################
# 
# finalize_sql_query_desired_period <- function(Table_name_from_db, inidate, period){
#   
#   inidate_main <-as.Date(inidate,"%Y-%m-%d")
#   
#   #inidate_dummy <- seq(as.Date(inidate_main),by = "-1 days",length.out =2)
#   inidate <- inidate_main
#   
#   if(period == 1){
#     num_day = 2
#     
#     est_quer <- build_query_for_selected_date(Table_name_from_db,inidate,n_day=num_day)
#     
#   }
#   
#   else if(period == 2){
#     
#     num_day = 8
#     
#     est_quer <- build_query_for_selected_date(Table_name_from_db,inidate,n_day=num_day)
#   }
#   
#   else if(period == 3){
#     library(Hmisc)
#     num_day = monthDays(inidate)
#     
#     est_quer <- build_query_for_selected_date(Table_name_from_db,inidate,n_day=num_day)
#     
#   }
#   
#   
#   else if(period == 4){
#     library(Hmisc)
#     num_day = yearDays(inidate)
#     
#     est_quer <- build_query_for_selected_date(Table_name_from_db,inidate,n_day=num_day)
#     
#   }
#   
#   
#   return(est_quer)
# }
# 
# 
# ##### Date extraction from database 
# Data_from_SQL_data_base <- function(Table_name_from_db, inidate, period) {
#   query <- finalize_sql_query_desired_period(Table_name_from_db, inidate, period) 
#   dat <- dbGetQuery(ROracle_con, query)
#   dat <- na.omit(dat)
#   return(dat)
# }
# 
# ########################
# 
# #dat <- Data_from_SQL_data_base(Table_name_from_db="IOS_APP_LOGIN_COUNT_STATE", inidate="2016-04-04", period=3)
# ########################
# 
# 
# state_count_by_date <- function(Table_name_from_db,inidate,period){
#   
#   susetted_data <- Data_from_SQL_data_base(Table_name_from_db,inidate,period)
#   
#   
#   susetted_data1 <- aggregate(susetted_data[,3],by = list(susetted_data$DATESTAMP,susetted_data$STATE),FUN = sum)
#   
#   susetted_data_by_state <- aggregate(susetted_data1[,3],by = list(susetted_data1$Group.2),FUN = sum)
#   
#   colnames(susetted_data_by_state) <- c("State","Count")
#   
#   
#   
#   return(susetted_data_by_state)
#   
# }
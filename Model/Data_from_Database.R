#Database connection establishment


#ROracle
library(DBI)
library(ROracle)
drv <- dbDriver("Oracle")
host <- "192.168.49.112"
port <- 1521
sid <- "bidb"
connect.string <- paste("(DESCRIPTION = ", 
                        "(ADDRESS = (PROTOCOL = tcp) (HOST = ", host, ") (PORT = ", port, "))",
                        "(CONNECT_DATA = (SID = ", sid, ")))",
                        sep = "")

ROracle_con <- dbConnect(drv, username = "bidb",password = "bidb",dbname=connect.string)
Sys.setenv(TZ = "GMT")
######################################################################################################


data_from_database <- function(quary,connection){
  
  data <- dbGetQuery(connection,quary)
  names(data) <- c("Time","Central","Mountain","Eastern","Pasific","Date")
  return(data)
}

###################################################################################################
quary_es_mar <- "Select T_INTERVAL,CENTRAL,MOUNTAIN,EASTERN,PACIFIC,EVENT_DATE from MAR_DUMP_COUNT"
eastern_data_mar <- data_from_database(connection = ROracle_con, quary = quary_es_mar)


quary_es_isp <- "Select T_INTERVAL,CENTRAL,MOUNTAIN,EASTERN,PACIFIC,EVENT_DATE from ISP_DUMP_COUNT"
eastern_data_isp <- data_from_database(connection = ROracle_con, quary = quary_es_isp)

quary_es_att <- "Select T_INTERVAL,CENTRAL,MOUNTAIN,EASTERN,PACIFIC,EVENT_DATE from ATTENDANCE_DUMP_COUNT"
eastern_data_att <- data_from_database(connection = ROracle_con, quary = quary_es_att)

quary_es_ger <- "Select T_INTERVAL,CENTRAL,MOUNTAIN,EASTERN,PACIFIC,EVENT_DATE from GER_DUMP_COUNT"
eastern_data_ger <- data_from_database(connection = ROracle_con, quary = quary_es_ger)

quary_es_bill <- "Select T_INTERVAL,CENTRAL,MOUNTAIN,EASTERN,PACIFIC,EVENT_DATE from BILL_DATA_DUMP_COUNT"
eastern_data_bill <- data_from_database(connection = ROracle_con, quary = quary_es_bill)

quary_es_smail <- "Select T_INTERVAL,CENTRAL,MOUNTAIN,EASTERN,PACIFIC,EVENT_DATE from SMAIL_DUMP_COUNT"
eastern_data_smail <- data_from_database(connection = ROracle_con, quary = quary_es_smail)


##

quary_loc_mar <- "Select T_INTERVAL,CENTRAL_LOCAL,MOUNTAIN_LOCAL,EASTERN_LOCAL,PACIFIC_LOCAL,EVENT_DATE from MAR_DUMP_COUNT"
local_data_mar <- data_from_database(connection = ROracle_con, quary = quary_loc_mar)

quary_loc_isp <- "Select T_INTERVAL,CENTRAL_LOCAL,MOUNTAIN_LOCAL,EASTERN_LOCAL,PACIFIC_LOCAL,EVENT_DATE from ISP_DUMP_COUNT"
local_data_isp <- data_from_database(connection = ROracle_con, quary = quary_loc_isp)

quary_loc_att <- "Select T_INTERVAL,CENTRAL_LOCAL,MOUNTAIN_LOCAL,EASTERN_LOCAL,PACIFIC_LOCAL,EVENT_DATE from ATTENDANCE_DUMP_COUNT"
local_data_att <- data_from_database(connection = ROracle_con, quary = quary_loc_att)

quary_loc_ger <- "Select T_INTERVAL,CENTRAL_LOCAL,MOUNTAIN_LOCAL,EASTERN_LOCAL,PACIFIC_LOCAL,EVENT_DATE from GER_DUMP_COUNT"
local_data_ger <- data_from_database(connection = ROracle_con, quary = quary_loc_ger)

quary_loc_bill <- "Select T_INTERVAL,CENTRAL_LOCAL,MOUNTAIN_LOCAL,EASTERN_LOCAL,PACIFIC_LOCAL,EVENT_DATE from BILL_DATA_DUMP_COUNT"
local_data_bill <- data_from_database(connection = ROracle_con, quary = quary_loc_bill)

quary_loc_smail <- "Select T_INTERVAL,CENTRAL_LOCAL,MOUNTAIN_LOCAL,EASTERN_LOCAL,PACIFIC_LOCAL,EVENT_DATE from SMAIL_DUMP_COUNT"
local_data_smail <- data_from_database(connection = ROracle_con, quary = quary_loc_smail)


############################################################################
require(plyr)

combining_module <- function(filenames){
  df <- join_all(filenames, by = c("Date","Time"), type = 'inner')
  df <- df[,c(1,6,2:5,7:26)]
  
  seq_central <- seq(3, 23, by = 4)
  seq_Mountain <- seq(4, 24, by = 4)
  seq_Eastern <- seq(5, 25, by = 4)
  seq_Pasific <- seq(6, 26, by = 4)
  
  df$Central_comb <- apply(df[,seq_central],1, sum)
  df$Mountain_comb <- apply(df[,seq_Mountain],1, sum)
  df$Eastern_comb <- apply(df[,seq_Eastern],1, sum)
  df$Pasific_comb <- apply(df[,seq_Pasific],1, sum)
  df$Combined_all <- apply(df[,27:30],1, sum)
  df <- df[,c(1,27:31,2)]
  names(df)[2:5] <- c("Central","Mountain","Eastern","Pasific")
  
  return(df)
  
}


filenames_eastern <- list(eastern_data_isp,
                        eastern_data_mar,
                        eastern_data_att,
                        eastern_data_ger,
                        eastern_data_bill,
                        eastern_data_smail)

Combined_eastern <- combining_module(filenames_eastern)


filenames_local <- list(local_data_isp,
                      local_data_mar,
                      local_data_att,
                      local_data_ger,
                      local_data_bill,
                      local_data_smail)

Combined_Local <- combining_module(filenames_local)







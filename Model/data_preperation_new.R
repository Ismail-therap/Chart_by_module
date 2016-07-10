


###Read the data
get_data_frame <- function(file_name){
  
  if(class(file_name) == "data.frame"){
    dat <- file_name
  }
  else{
    dat <- read.csv(file_name)
    dat <- dat[2:7] #Getting the input file
  }
  
  dat$Date <- as.Date(dat$Date,"%Y-%m-%d")
  return (dat)
}


#####Calculating the total count for 
calculate_usage_count <- function(dat, inidate, period){
  if(period == 1){
    n_day = 1
  }
  else if(period == 2){
    n_day = 7
  }
  else if(period == 3){
    library(Hmisc)
    inidate <- as.Date(inidate,"%Y-%m-%d")
    n_day = monthDays(inidate)
  }
  
  days <- seq(as.Date(inidate),by = "days",length.out = n_day) #No of day selection
  
  ########### Subsetting and calculating the percentage 
  susetted_data <- subset(dat,format(dat$Date,'%Y-%m-%d') == days[1])
  
  if(period != 1){
    for (i in 2:length(days) ){
      data_dayn <- subset(dat,format(dat$Date,'%Y-%m-%d') == days[i])
      susetted_data <- rbind(susetted_data,data_dayn)
    }
  }
  
  #return(susetted_data)
  
  #Checking combined data or not combined
  
  if(ncol(susetted_data) > 6) {
    susetted_data <- aggregate(susetted_data[,2:6],by = list(susetted_data$Time),FUN = sum)
    }
  
  else{
    susetted_data <- aggregate(susetted_data[,2:5],by = list(susetted_data$Time),FUN = sum)
  }
  #return(susetted_data)
  order_the_data_set_by_minutes(susetted_data)
  
}


####Ordering the time interval by minutes.

order_the_data_set_by_minutes <- function(agdat){
  
  colnames(agdat)[1] <- "Time"
  hour <- gsub(":.*","",agdat$Time)
  minutes <- gsub(".*:","",agdat$Time)
  agdat$ord <- as.numeric(hour)*60+as.numeric(minutes)
  agdat <- agdat[order(agdat$ord,decreasing = F),]
  
  #Checking combined or not combined
  
  if(ncol(agdat) > 6){
    return(agdat[,1:6])
  }
  
  else{
    return(agdat[,1:5])
    
  }
  
}


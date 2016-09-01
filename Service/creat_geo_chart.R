source("Model/Subsetted_data_from_database.R")



geo_chart_by_state <- function(Table_A,inidate, chart_for ){
  
  if(chart_for == "selected date") {
    
    Table_name_from_db <- Table_A
    data_TableA <- state_count_by_date(Table_name_from_db,inidate,period = 1)
    
    options_TableA = list(title = Table_A ,region="US",displayMode="regions",resolution="provinces",width=500, height=350)
    
    TableA <- gvisGeoChart(data_TableA, "State", "Count",
                           options=options_TableA)
    
    return(TableA)
  }
  
  
  else if (chart_for == "consecutive 7 days") {
    Table_name_from_db <- Table_A
    data_TableA <- state_count_by_date(Table_name_from_db,inidate,period = 2)
    
    options_TableA = list(title = Table_A ,region="US",displayMode="regions",resolution="provinces",width=500, height=350)
    
    TableA <- gvisGeoChart(data_TableA, "State", "Count",
                           options=options_TableA)
    
    return(TableA)
  }
  else if (chart_for == "consecutive 30 days") {
    Table_name_from_db <- Table_A
    data_TableA <- state_count_by_date(Table_name_from_db,inidate,period = 3)
    
    options_TableA = list(title = Table_A ,region="US",displayMode="regions",resolution="provinces",width=500, height=350)
    
    TableA <- gvisGeoChart(data_TableA, "State", "Count",
                           options=options_TableA)
    
    return(TableA)
  }
  else if (chart_for == "consecutive 365 days") {
    Table_name_from_db <- Table_A
    data_TableA <- state_count_by_date(Table_name_from_db,inidate,period = 4)
    
    options_TableA = list(title = Table_A ,region="US",displayMode="regions",resolution="provinces",width=500, height=350)
    
    TableA <- gvisGeoChart(data_TableA, "State", "Count",
                           options=options_TableA)
    
    return(TableA)
  }
  
  
}

# 
#   t <- geo_chart_by_state(Table_A = "ANDROID_APP_LOGIN_COUNT", inidate = "2016-04-04", chart_for = "consecutive 30 days")
# # # 
 # plot(t)
# # 
# 
# df <- state_count_by_date(Table_name_from_db="ANDROID_APP_LOGIN_COUNT_STATE", inidate = "2016-04-04",period = 3)
# 
# df <- df[-1,]
# library(plotly)
# df <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/2011_us_ag_exports.csv")
# #df$hover <- with(df, paste(State, '<br>', "Total count", Count))
# # give state boundaries a white border
# l <- list(color = toRGB("white"), width = 2)
# # specify some map projection/options
# g <- list(
#   scope = 'usa',
#   projection = list(type = 'albers usa'),
#   showlakes = TRUE,
#   lakecolor = toRGB('white')
# )
# 
# plot_ly(df, z = Count, locations = State, type = 'choropleth',
#         locationmode = 'USA-states', color = Count, colors = 'Purples',
#         marker = list(line = l), colorbar = list(title = "Millions USD")) %>%
#   layout(title = 'My chart', geo = g)
# 
# 
# 

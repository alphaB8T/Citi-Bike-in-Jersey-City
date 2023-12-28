
library(Hmisc)
library(data.table)
library(DT)
library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggcorrplot)
library(geosphere)
library(skimr)
library(leaflet)

library(data.table)

#opts_knit$set(root.dir = "C:/xyz/CU/5902 Capstone - Data Science Consulting/Final Project/Scripts")

setwd("C:/xyz/CU/5902 Capstone - Data Science Consulting/Final Project/Scripts")

Sys.setlocale("LC_TIME", "en_US.UTF-8")


rides = readRDS("shiny_preprocessed_rides.rds")


### For Stations Tab
start_station = unique(rides[, 'start_station_name'])
end_station = unique(rides[, 'start_station_name'])
stations = merge(start_station, end_station, all = TRUE)

station_net_change <- function(bike_type = "All", data) {
  if (bike_type == "All") {
    # If not specified, use all data
    dat <- data
  } else {
    # If specified, filter data by bike_type
    dat <- data[data$rideable_type == bike_type, ]
  }
  
  #dat = data[rideable_type == bike_type,]
  bikes_out = dat[, .(bikes_out = .N, 
                      start_station_latitude = first(start_lat), 
                      start_station_longitude = first(start_lng)), 
                  by = .(station_name = start_station_name, date = ride_date)]
  
  bikes_in = dat[, .(bikes_in = .N, 
                     end_station_latitude = first(end_lat), 
                     end_station_longitude = first(end_lng)), 
                 by = .(station_name = end_station_name, date = ride_date)]
  
  bikes_change = merge(bikes_in, bikes_out, by = c("station_name", "date"), all = TRUE)
  
  bikes_change[is.na(bikes_change),] = 0
  bikes_change = bikes_change[, net_change := bikes_in - bikes_out,]
  setorderv(bikes_change, cols = c("station_name", "date"))
  bikes_change = bikes_change[, cumulative := cumsum(net_change), by = station_name]
  bikes_change = bikes_change[, .(station_name, date, bikes_in, bikes_out, net_change, cumulative, 
                                  latitude = coalesce(end_station_latitude, start_station_latitude), 
                                  longitude = coalesce(end_station_longitude,start_station_longitude),
                                  bike_type = bike_type
  )]
}

electric_change = station_net_change("electric_bike", rides)
classic_change = station_net_change("classic_bike", rides)
all_change = station_net_change("All", rides)
change = rbind(electric_change, classic_change, all_change)

saveRDS(change, "stations_tab.rds")


### ebike / classic bike tab
station_hour_weekday <- function(bike_type = "All", data) {
  if (bike_type == "All") {
    # If not specified, use all data
    dat <- data
  } else {
    # If specified, filter data by bike_type
    dat <- data[data$rideable_type == bike_type, ]
  }
  
  #dat = data[rideable_type == bike_type,]
  bikes_out = dat[, .(bikes_out = .N, 
                      start_station_latitude = first(start_lat), 
                      start_station_longitude = first(start_lng)), 
                  by = .(station_name = start_station_name, date = ride_date, week_day = ride_day, hour = hour(started_at))]
  
  bikes_in = dat[, .(bikes_in = .N, 
                     end_station_latitude = first(end_lat), 
                     end_station_longitude = first(end_lng)), 
                 by = .(station_name = end_station_name, date = ride_date, week_day = ride_day, hour = hour(started_at))]
  
  bikes_change = merge(bikes_in, bikes_out, by = c("station_name", "date", "week_day", "hour"), all = TRUE)
  
  bikes_change[is.na(bikes_change),] = 0
  #bikes_change = bikes_change[, net_change := bikes_in - bikes_out,]
  setorderv(bikes_change, cols = c("station_name", "date", "hour"))
  #bikes_change = bikes_change[, cumulative := cumsum(net_change), by = station_name]
  bikes_change = bikes_change[, .(station_name, date, week_day, hour, bikes_in, bikes_out,
                                  latitude = coalesce(end_station_latitude, start_station_latitude), 
                                  longitude = coalesce(end_station_longitude,start_station_longitude),
                                  bike_type = bike_type
  )]
}


library(jsonlite)
library(httr)

url_info = "https://gbfs.citibikenyc.com/gbfs/en/station_information.json"  # Replace with the actual URL
data_info = GET(url_info)
content_info = content(data_info, "text")
station_info_json = fromJSON(content_info)
station_info = station_info_json$data$stations

station_info_name = station_info$name

sum(station_info_name %in% rides$start_station_name)

# Get unique names from each column
unique_start_stations <- unique(rides$start_station_name)
unique_end_stations <- unique(rides$end_station_name)

# Combine and get unique names across both columns
all_ride_stations <- union(unique_start_stations, unique_end_stations)

station_info_jc = station_info[station_info$name %in% all_ride_stations,]
station_info_jc = as.data.table(station_info_jc)
station_lat_lng = station_info_jc[, .(station_name = name, latitude = lat, longitude = lon)]

saveRDS(station_lat_lng, "station_lat_lng.rds")

electric_change_hw = station_hour_weekday("electric_bike", rides)
classic_change_hw = station_hour_weekday("classic_bike", rides)
all_change_hw = station_hour_weekday("Both", rides)
#change_hw = rbind(electric_change_hw, classic_change_hw, all_change_hw)

all_change_hw_avg = all_change_hw[, .(avg_bikes_out = mean(bikes_out, na.rm = TRUE)), by = .(station_name, week_day, hour)][, group := "Both"][, bike_type := "Both"]

electric_change_hw_avg = electric_change_hw[, .(avg_bikes_out = mean(bikes_out, na.rm = TRUE)), by = .(station_name, week_day, hour)][, group := "electric vs classic"][, bike_type := "electric_bike"]

classic_change_hw_avg = classic_change_hw[, .(avg_bikes_out = mean(bikes_out, na.rm = TRUE)), by = .(station_name, week_day, hour)][, group := "electric vs classic"][, bike_type := "classic_bike"]

hw_avg = rbind(electric_change_hw_avg, classic_change_hw_avg, all_change_hw_avg)

saveRDS(hw_avg, "electric_vs_classic.rds")

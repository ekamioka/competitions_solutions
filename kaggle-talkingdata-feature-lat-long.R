options(scipen = 9999)
rm(list=ls());gc()
setwd('/home/dogz/workspace/kaggle/talking/')

# function to extract the mode, R doesn't have a built-in
Mode <- function(x) {
  ux <- unique(x)
  ux <- ux[ux > 0]
  ux[which.max(tabulate(match(x, ux)))]
}

# function to get the city, state and country
library(RJSONIO)
getCountry <- function(long, lat){
  url <- paste("http://maps.googleapis.com/maps/api/geocode/json?latlng=",lat,',',long,"&sensor=true", sep="")
  x <- fromJSON(url)
  if(length(x$results) == 0) {ret = 'notfound'}
  else if(length(x$results) == 1) {ret = 'Dead-Stop-B'}
  else {ret = x$results[[1]]$address_components[[5]]$long_name}
  return(ret)
}
getState <- function(long, lat){
  url <- paste("http://maps.googleapis.com/maps/api/geocode/json?latlng=", lat,',',long,"&sensor=true", sep="")
  x <- fromJSON(url)
  if(length(x$results) == 0) {ret = 'notfound'}
  else if(length(x$results) == 1) {ret = 'Dead-Stop-B'}
  else {ret = x$results[[1]]$address_components[[4]]$long_name}
  return(ret)
}

getCity <- function(long, lat){
  url <- paste("http://maps.googleapis.com/maps/api/geocode/json?latlng=", lat,',',long,"&sensor=true", sep="")
  x <- fromJSON(url)
  if(length(x$results) == 0) {ret = 'notfound'}
  else if(length(x$results) == 1) {ret = 'Dead-Stop-B'}
  else {ret = x$results[[1]]$address_components[[3]]$long_name}
  return(ret)
}

events <- read.csv("events.csv")

# replacing missing data by mode by device
library(reshape2)
melted <- melt(events[, c(1,4,5)], id.vars=c("event_id"))
rm(events);gc()
#melted$value = as.factor(melted$value)
library(dplyr)
grouped <- group_by(melted, event_id, variable)
rm(melted);gc()

x <- summarise(grouped, mode = Mode(value))
rm(grouped);gc()
lat = x[x$variable == 'latitude',]
long = x[x$variable == 'longitude',]
lat[is.na(lat)] = 82.192242 #-49.6861615
long[is.na(long)] = -39.742896 #178.7349963
lat = data.frame(lat)
lat$variable = NULL
long = data.frame(long)
long$variable = NULL
lat = setNames(lat, c('event_id', 'latitude'))
long = setNames(long, c('event_id', 'longitude'))

dt = merge(lat, long, by = 'event_id')
rm(x, lat, long);gc()

uni = dt[,c(2,3)]
uni = uni[!duplicated(uni), ]

for(i in 1:length(uni$latitude)){
  print(paste0(i, ' of ',length(uni$latitude)))
  uni$key[i] = paste0(uni$longitude[i], uni$latitude[i], sep='')
}

dt = read.csv('./talking/dt.csv')

for(i in 1:length(dt$latitude)){
  print(paste0(i, ' of ',length(dt$latitude)))
  dt$key[i] = paste0(dt$longitude[i], dt$latitude[i], sep='')
}

dt = dt[1:5200,]

u = merge(uni, dt[, c(5:7)], by = 'key', all.x = T)

u_na = u[is.na(u$city), ]
u_nna = u[!is.na(u$city), ]

u_nna$city = NULL
u_nna$country = NULL




limit = 24999 #length(dt$device_id)
for(i in 1:limit){
  print(paste0(i, ' of ',limit))
  uni$city[i] = getCity(uni$longitude[i], uni$latitude[i])
  print(uni$city[i])
  uni$country[i] = getCity(uni$longitude[i], uni$latitude[i])
}

limit = 26900
for(i in 1901:limit){
  print(paste0(i, ' of ',limit))
  uni$city[i] = getCity(uni$longitude[i], uni$latitude[i])
  uni$country[i] = getCity(uni$longitude[i], uni$latitude[i])
}

limit = 51900
for(i in 26901:limit){
  print(paste0(i, ' of ',limit))
  uni$city[i] = getCity(uni$longitude[i], uni$latitude[i])
  uni$country[i] = getCity(uni$longitude[i], uni$latitude[i])
}

limit = 67107
for(i in 26901:limit){
  print(paste0(i, ' of ',limit))
  uni$city[i] = getCity(uni$longitude[i], uni$latitude[i])
  uni$country[i] = getCity(uni$longitude[i], uni$latitude[i])
}


getCountry <- function(long, lat){
  url <- paste("http://nominatim.openstreetmap.org/reverse?format=json&lat=",lat,"&lon=",long,"&zoom=18&addressdetails=1", sep="")
  x <- fromJSON(url)
  #if(length(x) == 0) {ret = 'notfound'}
  if(length(x) == 1) {ret = 'Unable_to_geocode'}
  else if(x$address[['country']]) {ret = x$address[['country']]} 
  else ifx$address[['locality']] {ret = x$address[['locality']]}
  return(ret)
}

limit = length(dt$event_id)
for(i in 1:limit){
  print(paste0(i, ' of ',limit))
  dt$country[i] = getCountry(dt$longitude[i], dt$latitude[i])
  print(dt$country[i])
#  dt$country[i] = getCity(dt$longitude[i], dt$latitude[i])
}

write.csv(dt, 'talking/data/longlat.csv', quote = F, row.names = F)

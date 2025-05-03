options(stringsAsFactors = FALSE)

library(dplyr)
library(readr)
library(tidyr)
library(data.table)
library(pracma)
library(lubridate)
library(scales)
library(png)

temp <- tempfile()
download.file("https://s3.amazonaws.com/tripdata/JC-202201-citibike-tripdata.csv.zip", temp)
Jan <- read.csv(unz(temp, "JC-202201-citibike-tripdata.csv"))

download.file("https://s3.amazonaws.com/tripdata/JC-202202-citibike-tripdata.csv.zip", temp)
Feb <- read.csv(unz(temp, "JC-202202-citibike-tripdata.csv"))

download.file("https://s3.amazonaws.com/tripdata/JC-202203-citibike-tripdata.csv.zip", temp)
Mar <- read.csv(unz(temp, "JC-202203-citibike-tripdata.csv"))

download.file("https://s3.amazonaws.com/tripdata/JC-202204-citibike-tripdata.csv.zip", temp)
Apr <- read.csv(unz(temp, "JC-202204-citibike-tripdata.csv"))

download.file("https://s3.amazonaws.com/tripdata/JC-202205-citibike-tripdata.csv.zip", temp)
May <- read.csv(unz(temp, "JC-202205-citibike-tripdata.csv"))

download.file("https://s3.amazonaws.com/tripdata/JC-202206-citibike-tripdata.csv.zip", temp)
Jun <- read.csv(unz(temp, "JC-202206-citibike-tripdata.csv"))

download.file("https://s3.amazonaws.com/tripdata/JC-202207-citbike-tripdata.csv.zip", temp)
Jul <- read.csv(unz(temp, "JC-202207-citbike-tripdata.csv"))

download.file("https://s3.amazonaws.com/tripdata/JC-202208-citibike-tripdata.csv.zip", temp)
Aug <- read.csv(unz(temp, "JC-202208-citibike-tripdata.csv"))

download.file("https://s3.amazonaws.com/tripdata/JC-202209-citibike-tripdata.csv.zip", temp)
Sep <- read.csv(unz(temp, "JC-202209-citibike-tripdata.csv"))

download.file("https://s3.amazonaws.com/tripdata/JC-202210-citibike-tripdata.csv.zip", temp)
Oct <- read.csv(unz(temp, "JC-202210-citibike-tripdata.csv"))

download.file("https://s3.amazonaws.com/tripdata/JC-202211-citibike-tripdata.csv.zip", temp)
Nov <- read.csv(unz(temp, "JC-202211-citibike-tripdata.csv"))

download.file("https://s3.amazonaws.com/tripdata/JC-202212-citibike-tripdata.csv.zip", temp)
Dec <- read.csv(unz(temp, "JC-202212-citibike-tripdata.csv"))

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Przygotowujemy zlaczona ramke danych z 4 miesiecy. Aby nie zaburzac wynikow wybieramy tylko te wypozyczenia, ktore trwaly od minuty do 3 godzin.
# Zmieniamy informacje o czasie startu i konca na zmienne typu POSIXct co bedzie pozniej pomocne

Joined <- rbind(Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec)
Joined[Joined == "classic_bike"] <- "classic"
Joined[Joined == "electric_bike"] <- "electric"
Joined[Joined == "docked_bike"] <- "docked"
Joined[Joined == ""] <- NA
na.omit(Joined) %>% mutate(start_date = as.POSIXct(started_at, 
                                                   format = "%F %T", tz = "GMT",
                                                   origin = "1970-01-01"),
                           end_date = as.POSIXct(ended_at, 
                                                 format = "%F %T", tz = "GMT",
                                                 origin = "1970-01-01"),
                           .keep = "unused") %>% filter(start_date <= end_date - 60 & start_date >= end_date - 60*60*3) -> Joined

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Wyciagamy dane do wykresu dotyczace liczby wypozyczen w kazdym miesiacu dla roznych rodzajow rowerow


numb_of_trips <- function(Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec){
  df_list <- list(Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec)
  months <- c("January", "February", "March", "April", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  for (i in 1:12){
    df_list[[i]] <- na.omit(df_list[[i]]) %>%  select(rideable_type) %>% count(rideable_type, name = "number_of_trips") %>% mutate(month = months[i])
  }
  as.data.frame(rbindlist(df_list))[, c(3,1,2)]
}

Trips <- numb_of_trips(Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec)
Trips[Trips == "classic_bike"] <- "classic"
Trips[Trips == "electric_bike"] <- "electric"
Trips[Trips == "docked_bike"] <- "docked"

# Ramka TripsT bedzie wyswietlana pod wykresem, bedzie zawierac procentowy udzial roznych rodzajow we wszytskich wypozyczeniach w danym miesiacu

TripsT <- as.data.table(Trips)
setDT(TripsT)[, Fraction := percent(round(number_of_trips / sum(number_of_trips), 4), accuracy = 0.01), by = month]
TripsT$number_of_trips <- NULL

for (i in 1:12){
  m <- c("January", "February", "March", "April", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")[i]
  TripsT$v <- rep(as.list(t(TripsT[month == m, .(Fraction)])), times = 12)
  colnames(TripsT)[i + 3] <- m
}
TripsT$month <- NULL
TripsT$Fraction <- NULL

colnames(TripsT)[1] <- "Bike type"
TripsT %>% distinct %>% arrange(row_number() == 2) -> TripsT

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Haversine formula do obliczania dystansu w lini prostej pomiedzy dwoma punktami danymi koordynatami

hav_formula <- function(lat1, lat2, lng1, lng2){
  lat1 <- deg2rad(lat1)
  lat2 <- deg2rad(lat2)
  lng1 <- deg2rad(lng1)
  lng2 <- deg2rad(lng2)
  dlat <- lat2 - lat1
  dlng <- lng2 - lng1
  a <- (sin(dlat / 2) ** 2) + cos(lat1) * cos(lat2) * (sin(dlng / 2) ** 2)
  c <- 2 * asin(sqrt(a))
  dist <- c * 6371
  round(dist, 3)
}

# Tworzymy ramke danych Distance, ktora bedzie zawierala pokonany przyblizony dystans dla kazdego wypozyczenia

distance <- function(Joined){
  Joined %>% select(ride_id, rideable_type, start_date, end_date, start_station_name, end_station_name, start_lat, end_lat, start_lng, end_lng) -> Joined
  as.data.table(na.omit(Joined))[, .(ride_id, rideable_type, start_date, end_date, start_station_name, end_station_name,
                                     travelled_distance = hav_formula(start_lat, end_lat, start_lng, end_lng))] -> Joined
  as.data.frame(Joined[travelled_distance >= 0.1][order(start_station_name)])
}

Distance <- distance(Joined)

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Tworzy ramke danych Avgspeed, ktora bedzie zawierala przyblizone srednie predkosci dla kazdego wypozyczenia, filtrujemy ja 

avg_speed <- function(Joined, Distance){
  as.data.table(Joined)[, .(ride_id, rideable_type, trip_duration = difftime(end_date, start_date, units = "hours"))] -> Joined
  Joined %>% inner_join(Distance, by = "ride_id", suffix = c("", ".y")) %>% mutate(avg_speed = (travelled_distance / as.numeric(trip_duration))) -> Joined
  as.data.frame(Joined[, .(ride_id, rideable_type, start_date, end_date, start_station_name, end_station_name, 
                           avg_speed = round(avg_speed, 2))][avg_speed >= 0.1 & avg_speed <= 30][order(ride_id)])
}

AvgSpeed <- avg_speed(Joined, Distance)

# Tworzymy ramke danych ktora bedzie wyswiatlana pod wykresem, ma podsumowywac wykres

AvgSpeed %>% group_by(rideable_type) %>% mutate(mean_avg_speed = round(mean(avg_speed), 2),
                                                med = round(median(avg_speed), 2),
                                                sd_avg_speed = round(sd(avg_speed), 2),
                                                .keep = "unused") %>% select(rideable_type, mean_avg_speed, med, sd_avg_speed) %>% distinct() -> AvgSpeed_summary

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Tworzymy ramke danych Avgs2dis, bedzie ona zawierala dane potrzebne do wykresy sredniej predkosci w zaleznosci od pokonanego dystansu.
# Rozwazamy tyko te wypozyczenia, dla ktorych srednia predkosc wynosi co najmniej 5 km/h a dystans 7.5 km

avgs2dis <- function(Distance, AvgSpeed){
  Distance %>% filter(travelled_distance >= 1) -> Distance
  AvgSpeed %>% filter(avg_speed >= 5) -> AvgSpeed
  Distance %>% inner_join(AvgSpeed, by = "ride_id", suffix = c("", ".y")) %>% mutate(start_hour = strftime(start_date, "%T" , tz = "GMT"),
                                                                                     end_hour = strftime(end_date, "%T", tz = "GMT"),
                                                                                     date = as.POSIXct(strftime(start_date, "%F", tz = "GMT")),
                                                                                     .keep = "unused") %>% 
    select(ride_id, rideable_type, date, start_hour, end_hour, start_station_name, end_station_name, travelled_distance, avg_speed) %>% arrange(date)
}   

Avgs2dis <- avgs2dis(Distance, AvgSpeed)

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Przygotowujemy dane do wykresu intensywnosci wypozyczen w kazdym dniu tygodniu srednio z dokladnoscia do jednej minuty

hours <- function(Joined){
  weekday_names <- c("Sunday", "Monday", "Tuesday", "Wendesday", "Thursday", "Friday", "Saturday")
  as.data.table(Joined)[, .(start_date, weekday = weekday_names[wday(start_date)])] -> Joined
  
  get_minutes <- function(x, seconds = 60) { 
    as.POSIXct(floor(unclass(x) / seconds) * seconds, origin = "1970-01-01", tz = "GMT")
  }
  
  Joined[, .(weekday, fullminutes = as.POSIXct(ifelse(strftime(start_date, "%S") < 30, 
                                                      get_minutes(start_date),
                                                      get_minutes(start_date + 60)),
                                               origin = "1970-01-01", tz = "GMT"))] -> Joined
  Joined[, .(weekday, time_of_day = strftime(fullminutes, "%T", tz = "GMT"))] -> Joined
  Joined %>% add_count(time_of_day, name = "started_trips") -> Joined
  Joined %>% add_count(time_of_day, weekday) %>% distinct() %>% mutate(avg_trips_days = round(n / (365 / 7), 1),
                                                                       avg_overall = round(started_trips / 365, 1), .keep = "unused") -> Joined
  as.data.table(Joined)[order(weekday, time_of_day)] -> Joined
  Joined <- pivot_wider(Joined, names_from = weekday, values_from = avg_trips_days)
  Joined[is.na(Joined)] <- 0
  as.data.frame(Joined)
}

Hours <- hours(Joined)

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------
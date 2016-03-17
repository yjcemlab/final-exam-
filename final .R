  ##Final Exam                                                  Yan Jingchi 
library("knitr", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
library("nycflights13", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
library("dplyr", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
library("RSQLite", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
library("stargazer", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
library("tidyr", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
my_db <-nycflights13_sqlite()
weather<-tbl(my_db, "weather") %>% collect()
airports <- tbl(my_db, "airports") %>% collect()
planes <- tbl(my_db, "planes") %>% collect()
flights <- tbl(my_db, "flights") %>% 
  collect() %>%
  mutate(canceled = is.na(arr_time))
flights$canceled <- flights$canceled + 0
flights$date <- paste( flights$year, flights$month, flights$day, sep = ".")
weather$date <- paste(weather$year, weather$month, weather$day, sep = ".")
#creating a table with weather condition and departute delay and cancelations 
flights.delay <- flights %>%
  group_by(date) %>%
  summarise(
    delay <- mean(dep_delay, na.rm = T),
    cancel <- sum(canceled)
  )
weather1 <- weather %>%
  group_by(date) %>%
  summarise(
    temperature <- mean(temp, na.rm = T),
    dew <- mean(dewp, na.rm = T),
    humidity <- mean(humid, na.rm = T),
    wind_direction <- mean(wind_dir, na.rm = T),
    wind_mph <- mean(wind_speed, na.rm = T),
    wind_gusts <- mean(wind_gust, na.rm = T),
    rain <- mean(precip, na.rm = T),
    pressu <- mean(pressure, na.rm = T),
    visibility <- mean(visib, na.rm = T)
  )
data1 <- inner_join(flights.delay, weather1, by = c("date"))
#ceating a table with airport destination and departure delays and cancelations
flights.ad <- flights %>%
  group_by(date,dest) %>%
  summarise(
    delay <- mean(dep_delay, na.rm = T),
    cancel <- sum(canceled)
)
airports.ad <- airports[,1:2]
data2 <- inner_join(flights.ad, airports.ad, by = c("dest" = "faa"))
#creating a table with plane information and departure delays and cancelations
flights.plane <- flights %>%
  group_by(date, tailnum) %>%
  summarise(
    delay <- mean(dep_delay, na.rm = T),
    cancel <- sum(canceled)
  )
data3<- inner_join(flights.plane, planes, by = c("tailnum"))
#a
plot(data1$`delay <- mean(dep_delay, na.rm = T)`,col="red")
par(new=TRUE)
plot(data1$`temperature <- mean(temp, na.rm = T)`,col="blue")
plot(data1$`cancel <- sum(canceled)`,col="red")
par(new=TRUE)
plot(data1$`temperature <- mean(temp, na.rm = T)`,col="blue")
#b
flighttime <- flights%>%
  select(date,minute,contains("time"),canceled,dep_delay)
flighttime1 <- flighttime %>%
  separate(date,c("year", "month", "day","hour")) %>%
  group_by(canceled)%>%
  arrange(dep_time,dep_delay)%>%
  filter(!is.na(dep_delay))
reg1<-lm(dep_delay~dep_time+month+day+minute, flighttime1)
stargazer(reg1,type="text",title="time on dep_delay regression",out="final.doc")
reg2<-glm(canceled~dep_delay+dep_time+month+day+minute, flighttime1,family=binomial(link = "logit"))
stargazer(reg2,type="text",title="time on cancelation regression",out="final.doc")
#c
reg3 <- lm(data2$`delay <- mean(dep_delay, na.rm = T)`~data2$dest)
stargazer(reg3,type="text",title=" destination on dep_delay regression",out="final.doc")
reg4 <- lm(data2$`cancel <- sum(canceled)`~data2$dest)
stargazer(reg4,type="text",title="destination on cancelation regression",out="final.doc")
#d
reg5<- lm(data3$`delay <- mean(dep_delay, na.rm = T)`~data3$year+data3$type+data3$manufacturer)
stargazer(reg5,type="text",title="planes on dep_delay regression",out="final.doc")
reg6<- lm(data3$`cancel <- sum(canceled)`~data3$year+data3$type+data3$manufacturer)
stargazer(reg6,type="text",title="planes on cancelation regression",out="final.doc")

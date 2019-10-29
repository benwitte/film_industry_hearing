library(tidyverse)
library(chron)
library(ggplot2)
library(lubridate)

permits_raw <- read_csv('https://data.cityofnewyork.us/resource/tg4x-b46p.csv?$limit=9999999')


# YoY Analysis  --------------------------------------------------------------

permits_raw$count <- 1
permits_raw$year <- format(permits_raw$startdatetime, '%Y')

permits_raw_pre_19 <- filter(permits_raw, startdatetime < as.POSIXct('2019-01-01 00:00:00') & enddatetime < as.POSIXct('2019-01-01 00:00:00'))



yoy <- aggregate(permits_raw_pre_19$count,
                 by = list(year = permits_raw_pre_19$year),
                 function(x) {sum(x)}) %>% 
  rename(num_permits = x)


ggplot(data = yoy, aes(x=year, y=num_permits)) +
  geom_col(fill="tomato3") +
  xlab("Year") + ylab("Number of Permits") +
  ggtitle("Number of Permits per Year")


yoy_short <- yoy[yoy$year %in% c(2012, 2015, 2018),]

ggplot(data = yoy_short, aes(x=year, y=num_permits)) +
  geom_col(fill="tomato3") +
  xlab("Year") + ylab("Number of Permits") +
  ggtitle("Number of Permits per Year")




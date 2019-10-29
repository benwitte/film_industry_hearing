library(tidyverse)
library(janitor)
library(sf)
library(RCurl)


permits_raw <- read_csv('https://data.cityofnewyork.us/resource/tg4x-b46p.csv?$limit=9999999')

permits <- permits_raw %>% 
  separate_rows(parkingheld, sep = ', ') %>% 
  separate(., parkingheld, c('main','cross_st_1'), sep = ' between ') %>% 
  separate(., cross_st_1, c('cross_st_1', 'cross_st_2'), sep = ' and ') %>% 
  drop_na()

write_csv(permits, 'permits.csv')



# API function ------------------------------------------------------------
# see link in README to learn more and set up API access and receive id and key

Gcode<-function(eventid,main,cross_st_1,cross_st_2, borough){
  require(RCurl)
  u1=paste0("https://api.cityofnewyork.us/geoclient/v1/blockface.json?app_id=",app_id,"&app_key=",app_key,"&onStreet=")
  u2="&crossStreetOne="
  u3="&crossStreetTwo="
  u4= "&borough="
  
  
  p1=main
  p1=str_trim(p1)
  p1=gsub("\\s{1,}","+",p1)
  p2=cross_st_1
  p2=str_trim(p2)
  p2=gsub("\\s{1,}","+",p2)
  p3=cross_st_2
  p3=str_trim(p3)
  p3=gsub("\\s{1,}","+",p3)
  BR=borough
  BR=str_trim(BR)
  url=paste(c(u1,p1,u2,p2,u3,p3,u4,BR), sep = '', collapse = "")
  TMP=getURL(url)
  
  
  xf=parse_number(strsplit(TMP,"(longitudeOfFromIntersection)|(longitudeOfToIntersection)")[[1]][2])
  xt=parse_number(strsplit(TMP,"(longitudeOfToIntersection)|(lowCrossStreet)")[[1]][2])
  yf=parse_number(strsplit(TMP,"(latitudeOfFromIntersection)|(latitudeOfToIntersection)")[[1]][2])
  yt=parse_number(strsplit(TMP,"(latitudeOfToIntersection)|(leftSegment)")[[1]][2])
  
  #xf <- gsub("[^[:digit:]., ]", "", df$b)
  
  
  XY=c(eventid, main, cross_st_1, cross_st_2, xf,xt,yf,yt)
  names(XY)<-c("eventid", 'main', 'cross_st_1', 'cross_st_2', "long_from","long_to","lat_from", "lat_to")
  
  return(XY)
}



locations_raw_output <- apply(permits, MARGIN = 1, function(x) Gcode(x['eventid'],x['main'], x['cross_st_1'], x['cross_st_2'], x['borough']))

locations <- as_tibble(t(locations_raw_output))

locations$eventid <- as.numeric(locations$eventid)

# Missingness Rate
sum(!is.na(locations$long_from))/nrow(locations)



write_csv(locations, 'film_permit_lcoations.csv')


geopermits <- unique(left_join(permits, locations))


geopermits_2018 <- filter(permits, startdatetime >= as.POSIXct('2018-01-01 00:00:00') & startdatetime < as.POSIXct('2019-01-01 00:00:00'))

# Missingness Rate 18
sum(!is.na(geopermits_2018$long_from))/nrow(geopermits_2018)
# None missing from 2018


write_csv(geopermits, "geopermits.csv")

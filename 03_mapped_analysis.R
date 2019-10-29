library(tidyverse)
library(sf)
library(leaflet)
library(data.table)

geopermits <- read_csv('output_files/geopermits.csv') 

geopermits <- geopermits %>% drop_na(c('long_from', 'lat_from', 'long_to', 'lat_to'))

geopermits$clong <- (geopermits$long_from + geopermits$long_to)/2
geopermits$clat <- (geopermits$lat_from + geopermits$lat_to)/2

geopermits <- st_as_sf(geopermits, coords = c("clong", "clat"), crs = '+proj=longlat +datum=WGS84')


geopermits_2018 <- filter(geopermits, startdatetime < as.POSIXct('2019-01-01 00:00:00') & startdatetime >= as.POSIXct('2018-01-01 00:00:00')) %>% 
  st_set_crs('+proj=longlat +datum=WGS84')


# Map at CD Level ---------------------------------------------------------



cds <- read_sf('input_files/community_districts/geo_export_3b2cd0ff-eff3-46ff-adc6-59ddc6430073.shp') %>% 
  select(boro_cd, geometry) %>% 
  st_transform(st_crs(geopermits_2018))


cds$num_permits <- lengths(st_intersects(cds,geopermits_2018))


permit_pal <- colorBin(
  palette = 'YlGnBu',
  domain = cds$num_permits)

permit_pop  <- paste0("Community District: ", cds$boro_cd, '<br>',
                      "Number of Permits: ", cds$num_permits)


leaflet(cds) %>%
  addProviderTiles('CartoDB.Positron') %>%
  addPolygons(fillColor = ~permit_pal(cds$num_permits),
              fillOpacity = .9,
              weight = 3,
              popup = permit_pop) %>% 
  addLegend(position = "topleft",
            pal = permit_pal,
            values = cds$num_permits,
            title = "Number of Permits by Community Disctrict, 2018")





# Map at Zipcode Level ----------------------------------------------------


zips <- read_sf('input_files/zip_code/ZIP_CODE_040114.shp') %>% 
  janitor::clean_names() %>% 
  select(zipcode, geometry) %>% 
  st_transform('+proj=longlat +datum=WGS84')




zips$num_permits <- lengths(st_intersects(zips,geopermits_2018))


zpermit_pal <- colorBin(
  palette = 'Blues',
  domain = zips$num_permits)

zpermit_pop  <- paste0("Zipcode: ", zips$zipcode, '<br>',
                       "Number of Permits: ", zips$num_permits)


leaflet(zips) %>%
  setView(lng = -73.95, lat = 40.73, zoom = 11) %>%
  addProviderTiles('CartoDB.Positron') %>%
  addPolygons(fillColor = ~zpermit_pal(zips$num_permits),
              color = '#2F56A6',
              fillOpacity = .3,
              weight = 1,
              popup = zpermit_pop) %>% 
  addLegend(position = "topleft",
            pal = zpermit_pal,
            values = zips$num_permits,
            title = "Number of Permits by Zipcode, 2018")






# Adding 311 data ---------------------------------------------------------



three11 <- read_csv('input_files/311_Service_Request2018.csv') %>% 
  janitor::clean_names()

three11 <- three11[!is.na(three11$latitude),]
three11 <- three11[!is.na(three11$longitude),]

geo311 <- st_as_sf(three11, coords = c('longitude', 'latitude'), crs = "+proj=longlat +datum=WGS84")

permits_2018$count <- 1

top_streets <- aggregate(permits_2018$count,
                         by = list(main_street = permits_2018$main,
                                   cross_1 = permits_2018$cross_st_1,
                                   cross_2 = permits_2018$cross_st_2,
                                   borough = permits_2018$borough),
                         function(x) {sum(x)}) %>% 
  rename(num_permits = x) %>% 
  arrange(desc(num_permits)) %>% 
  mutate(street = paste(main_street, cross_1, cross_2, borough))

top_10 <- top_streets[1:10,]

# Make the buffered circle around geopermits_2018 permit locations

geopermits_2018 <- geopermits_2018 %>% 
  mutate(street = paste(main, cross_st_1, cross_st_2, borough))

geo18_10 <- filter(geopermits_2018, grepl(paste(top_10$street, collapse = "|"), street))

geo18_10$geometry <- geo18_10$geometry %>%
  st_transform(3488) %>%  # Convert to a projection that has a real unit (not degrees)
  # In this case it's Albers Equal Area (unit is meters)
  st_buffer(dist = units::as_units(.2, "mile")) %>%  # Buffer 1 mile (sf handles units)
  st_transform("+proj=longlat +datum=WGS84") # Unproject back to lat-lon




# Spacial join of top 10 permits and complaints

joined <- st_join(geo18_10,geo311)

drops <- c('eventagency', 'borough.y', 'communityboard_s', 'policeprecinct_s',
           'country', 'zipcode_s', "long_from", "long_to", "lat_from", "lat_to",
           "main", "cross_st_1", "cross_st_2", "location_type", "incident_zip",
           "incident_address", "street_name","cross_street_1", "cross_street_2",
           "intersection_street_1", "intersection_street_2", "address_type",
           "city", "landmark", "facility_type", "status", "due_date", 
           "resolution_action_updated_date", "community_board", "bbl",
           "x_coordinate_state_plane", "y_coordinate_state_plane", 
           "open_data_channel_type", "park_facility_name", "park_borough", 
           "vehicle_type", "taxi_company_borough", "taxi_pick_up_location", 
           "bridge_highway_name", "bridge_highway_direction", "road_ramp", 
           "bridge_highway_segment")


joined <- joined %>% 
  select(-one_of(drops))

write_csv(joined, 'outputfiles/joined_311_permits.csv')


# Confirming output has very few uniques, meaning there are so many
# because output is each combination possible
length(unique(west48$startdatetime))
length(unique(west48$unique_key))


west48 <- joined[joined$street %like% 'AVENUE 7 AVENUE Manhattan',]

write_csv(west48, 'output_files/west48.csv')

west48$created_date <- as.POSIXct(west48$created_date, format = "%m/%d/%Y %I:%M:%S %p")

west48_nonsf <- west48 %>% 
  select(eventid, startdatetime, enddatetime,created_date, unique_key) %>% 
  mutate(geometry = NULL)

west48_nonsf <- as.data.frame(west48_nonsf)

west48_nonsf <- west48_nonsf %>% 
  filter(as.POSIXct(created_date) >= startdatetime & created_date <= enddatetime) %>% 
  mutate(key_id = paste(unique_key, eventid))

west48 <- west48 %>% 
  mutate(key_id = paste(unique_key, eventid)) %>% 
  filter(key_id %in% west48_nonsf$key_id)

write_csv(west48, 'output_files/west48.csv')

'MONITOR STREET GREENPOINT AVENUE NORMAN AVENUE Brooklyn'

monitor_gp <- joined[joined$street %like% 'MONITOR STREET GREENPOINT AVENUE',]

write_csv(monitor_gp, 'output_files/monitor_gp.csv')

monitor_gp$created_date <- as.POSIXct(monitor_gp$created_date, format = "%m/%d/%Y %I:%M:%S %p")

sdt_mongp <- length(unique(monitor_gp$startdatetime))
ukey_mongp <- length(unique(monitor_gp$unique_key))

monitor_gp_nonsf <- monitor_gp %>% 
  select(eventid, startdatetime, enddatetime,created_date, unique_key) %>% 
  mutate(geometry = NULL)

monitor_gp_nonsf <- as.data.frame(monitor_gp_nonsf)

monitor_gp_nonsf <- monitor_gp_nonsf %>% 
  filter(as.POSIXct(created_date) >= startdatetime & created_date <= enddatetime) %>% 
  mutate(key_id = paste(unique_key, eventid))

monitor_gp <- monitor_gp %>% 
  mutate(key_id = paste(unique_key, eventid)) %>% 
  filter(key_id %in% monitor_gp_nonsf$key_id)

write_csv(monitor_gp, 'output_files/monitor_gp.csv')




# Council District Comparison ---------------------------------------------

coundis <- read_sf('input_files/council_districts/geo_export_8729b41f-14df-41c5-9542-2f46d543773b.shp')%>% 
  select(coun_dist, geometry) %>% 
  st_transform(st_crs(geopermits_2018)) %>% 
  arrange(coun_dist)


coundis$num_permits <- lengths(st_intersects(coundis,geopermits_2018))

councheck <- st_intersects(coundis,geopermits_2018)
cdholden <- unique(geopermits[councheck[[30]],]$eventid)







permit_pal <- colorBin(
  palette = 'YlGnBu',
  domain = cds$num_permits)

permit_pop  <- paste0("Community District: ", cds$boro_cd, '<br>',
                      "Number of Permits: ", cds$num_permits)


leaflet(cds) %>%
  addProviderTiles('CartoDB.Positron') %>%
  addPolygons(fillColor = ~permit_pal(cds$num_permits),
              fillOpacity = .9,
              weight = 3,
              popup = permit_pop) %>% 
  addLegend(position = "topleft",
            pal = permit_pal,
            values = cds$num_permits,
            title = "Number of Permits by Community Disctrict, 2018")




# Top 50 ------------------------------------------------------------------


top_50 <- 

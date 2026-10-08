library(rjson)
library(geojsonio)

ss_df = fromJSON(file = "../data/raw/ss-locations.json")$events

lat = c()
lon = c()
for (i in 1:length(ss_df)){
  sub_df = ss_df[[i]]
  lat = c(lat, as.numeric(sub_df$lat))
  lon = c(lon,as.numeric(sub_df$lon))
}

sensor_locations = tibble(lat = lat, lon = lon)
# write.csv(sensor_locations, '../gis/sensor_locations.csv', row.names = F)

# Some processing in QGIS...

sensor_sca_locations = geojson_read('../gis/ss_sca_locations.geojson', 
                                    parse = T)$features$properties
sensor_sca_locations = sensor_sca_locations %>% 
  filter(Precinct==9) %>% 
  group_by(Area) %>% 
  summarise(count = n())
treated_scas = c(sensor_sca_locations$Area, c(803,804, 807,808))

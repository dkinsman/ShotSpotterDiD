library(geojsonio)
library(dplyr)

intersections <- geojson_read('../gis/redline_intersections.geojson',
                              parse = T)
sca <- geojson_read('../gis/sca-areas.geojson',
                    parse = T)
intf <- as.data.frame(intersections$features)
intsca <- as.data.frame(sca$features)

df = intf %>% group_by(properties$Area) %>%
  summarise(total_intersection = sum(properties$red_int))
sca.area = intsca %>% group_by(properties$Area) %>%
  summarise(sca_area = sum(properties$sca_area))

empty_sca = setdiff(sca.area[,1], df[,1])
start = nrow(df) + 1
df[(start):(start + nrow(empty_sca)-1 ),1] = empty_sca
df[(start ):(start + nrow(empty_sca)-1),2] = rep(0, length(empty_sca))
# sca.area = sca.area[-131,]

redline = merge(df, sca.area, by = 'properties$Area')
redline['red_ratio'] = redline$total_intersection / redline$sca_area
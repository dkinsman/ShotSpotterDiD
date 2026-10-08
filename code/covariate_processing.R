library(geojsonio)
library(dplyr)
library(stringr)
library(MatchIt)
library(cobalt)
library(tidyr)
library(ggplot2)
library(scales)

treated_scas =  c(803, 804, 807, 808, 902, 903, 906, 907)

# Redlining Processing ----------------------------------------------------

intersections <-
  geojson_read('../gis/redline_intersections.geojson',
               parse = T)
sca <- geojson_read('../gis/sca-areas.geojson',
                    parse = T)
intf <- as.data.frame(intersections$features$properties)
# intf$Area = as.factor(intf$Area)
intsca <- as.data.frame(sca$features$properties)
# intsca$Area = as.factor(intsca$Area)

df = intf %>% group_by(Area) %>%
  summarise(total_intersection = sum(red_int))

sca.area = intsca %>% group_by(Area) %>%
  summarise(sca_area = sum(sca_area))

empty_sca = setdiff(sca.area[, 1], df[, 1])
start = nrow(df) + 1
df[(start):(start + nrow(empty_sca) - 1), 1] = empty_sca
df[(start):(start + nrow(empty_sca) - 1), 2] = rep(0, length(empty_sca))
# sca.area = sca.area[-131,]

redline = merge(df, sca.area, by = 'Area')
redline['red_ratio'] = redline$total_intersection / redline$sca_area


# Census Blocks Processing -------------------------------------------------------

census_blocks = geojson_read('../gis/sca_block.geojson', parse = T)
census_blocks_features = as.data.frame(census_blocks$features$properties)

total_sca_land = census_blocks_features %>%
  group_by(GEOID20) %>%
  summarise(block_area = sum(block_sca_area))

census_blocks_features = merge(census_blocks_features, total_sca_land,
                               by = 'GEOID20')
census_blocks_features['prop_block_in_sca'] = census_blocks_features$block_sca_area /
  census_blocks_features$block_area

census_blocks_features['weighted_pop'] = census_blocks_features$prop_block_in_sca *
  census_blocks_features$POP20
census_blocks_features['weighted_house'] = census_blocks_features$prop_block_in_sca *
  census_blocks_features$HOUSING20
census_blocks_features['weighted_land'] = census_blocks_features$prop_block_in_sca *
  census_blocks_features$ALAND20
# sca_unique_blocks =
# census_blocks_features %>%
#   group_by(GEOID20) %>%
#   summarise(count = n()) %>%
#   filter(count <= 1)
#
# sca_overlap_blocks = census_blocks_features %>%
#   group_by(GEOID20) %>%
#   summarise(count = n()) %>%
#   filter(count > 1)
#
# overlap_total_area = census_blocks_features %>%
#   filter(GEOID20 %in% sca_overlap_blocks$GEOID20) %>%
#   group_by(GEOID20) %>%
#   summarise(total_area = sum(block_sca_area))
#
# assigned_blocks = census_blocks_features %>%
#   filter((GEOID20 %in% sca_overlap_blocks$GEOID20)) %>%
#   group_by(GEOID20) %>%
#   filter(block_sca_area == max(block_sca_area))
# # assigned_blocks['total_area'] = mapvalues(assigned_blocks$GEOID20,
# #                                           overlap_total_area$GEOID20,
# #                                           overlap_total_area$total_area,
# #                                           warn_missing = F)
# # assigned_blocks$total_area = as.numeric(assigned_blocks$total_area)
# # assigned_blocks['block_sca_proportion'] = assigned_blocks$block_sca_area/assigned_blocks$total_area
# # assigned_blocks = assigned_blocks %>%
# #   filter(block_sca_proportion >= 1/3)
#
# census_blocks_features = rbind(census_blocks_features %>%
#                                  filter(GEOID20 %in% sca_unique_blocks$GEOID20),
#                                assigned_blocks)

p1_data = read.csv('../data/raw/DECENNIALPL2020.P1-Data.csv', header = T)
colnames(p1_data) = p1_data[1, ]
p1_data = p1_data[-1, ]
name_split = unlist(str_split(p1_data$Geography, pattern = 'US'), recursive = F)
name_split = name_split[!(name_split %in% c('Geography' , '1000000'))]
p1_data['GEOID20'] = name_split

census_block_columns = c(
  'weighted_house',
  'GEOID20',
  'Area',
  'weighted_land',
  'weighted_pop',
  'prop_block_in_sca'
)

temp_df = merge(census_blocks_features[, census_block_columns], p1_data,
                by = 'GEOID20')

census_df = sapply(temp_df[, -(1:9)], as.numeric) #/sapply(temp_df[,8], as.numeric)
census_df = cbind(temp_df[1:9], census_df)

pop_black_alone = ' !!Total:!!Population of one race:!!Black or African American alone'

census_df['adj_black'] = census_df[pop_black_alone] * census_df$prop_block_in_sca

# Blocks Comparison ----------------------------------------------------------

sca_comparison = census_df %>%
  group_by(Area) %>%
  summarise_if(is.numeric, sum, na.rm = T)

comparison_tab = as.data.frame(sca_comparison[c('Area',
                                                'weighted_pop',
                                                'weighted_house',
                                                'weighted_land',
                                                'adj_black')])
comparison_tab['percent_black_alone'] = sca_comparison$adj_black / sca_comparison$weighted_pop
comparison_tab['treated'] = ifelse(comparison_tab$Area %in% treated_scas, T, F)
# comparison_tab = comparison_tab %>%
#   group_by(treated) %>%
#   arrange(desc(percent_black_alone), .by_group = T) #%>%
#filter((percent_black_alone >=0.85))

#comparison_tab = comparison_tab %>% filter(POP20 >= 4500)


# Census Tract Processing -------------------------------------------------

census_tracts = geojson_read('../gis/sca_tract.geojson', parse = T)
census_tracts_features = as.data.frame(census_tracts$features$properties)

tract_sca_prop = census_tracts_features[, c('GEOID', 'Area')]
tract_sca_prop['prop'] = census_tracts_features$tract_sca_area / census_tracts_features$sca_area
tract_sca_prop = tract_sca_prop %>% filter(Area %in% comparison_tab$Area)

acs_data = read.csv('../data/raw/ACSST5Y2020.S1902-Data.csv', header = T)
colnames(acs_data) = acs_data[1, ]
acs_data = acs_data[-1, ]
name_split = unlist(str_split(acs_data$Geography, pattern = 'US'), recursive = F)
name_split = name_split[!(name_split %in% c('Geography' , '1400000'))]
acs_data['GEOID'] = name_split

cre_data = read.csv('../data/raw/CRE_21_Tract.csv')
cre_data = cre_data %>% filter((STATE == 26) & (COUNTY == 163))
name_split = unlist(str_split(cre_data$GEO_ID, pattern = 'US'), recursive = F)
name_split = name_split[!(name_split %in% c('Geography' , '1400000'))]
cre_data['GEOID'] = name_split
cre_data['PRED123_PE'] = cre_data$PRED12_PE + cre_data$PRED3_PE

cre_columns = c('GEOID', 'PRED123_PE')
census_tract_columns = c('GEOID', 'Area')
acs_columns = c(
  'Estimate!!Mean income (dollars)!!HOUSEHOLD INCOME!!All households',
  "Estimate!!Number!!HOUSEHOLD INCOME!!All households",
  "Estimate!!Number!!HOUSEHOLD INCOME!!All households!!With cash public assistance income or Food Stamps/SNAP",
  'GEOID'
)


tract_sca_prop = merge(tract_sca_prop, acs_data[, acs_columns], by = 'GEOID')
tract_sca_prop[, 4] = as.numeric(tract_sca_prop[, 4])
tract_sca_prop[, 5] = as.numeric(tract_sca_prop[, 5])
tract_sca_prop[, 6] = as.numeric(tract_sca_prop[, 6])
colnames(tract_sca_prop)[4] = 'mean_household_income'
colnames(tract_sca_prop)[5] = 'num_house'
colnames(tract_sca_prop)[6] = 'num_house_SNAP'
tract_sca_prop$num_house_SNAP = tract_sca_prop$num_house_SNAP/tract_sca_prop$num_house
tract_sca_prop = merge(tract_sca_prop, cre_data[, cre_columns], by = 'GEOID')

weighted_income = tract_sca_prop %>%
  drop_na() %>%
  group_by(Area) %>%
  summarise(
    weighted_income = weighted.mean(mean_household_income, prop),
    num_house_SNAP = weighted.mean(num_house_SNAP, prop),
    weighted_social_vulnerability = weighted.mean(PRED123_PE, prop)
  )

comparison_tab = merge(comparison_tab, weighted_income, by = 'Area')
#filter((weighted_income>=35000) & (weighted_income <=60000))

# min weighted income = 30000: 32 control SCA
comparison_tab['pop_land_ratio'] = comparison_tab$weighted_pop / comparison_tab$weighted_land
comparison_tab['housing_pop_ratio'] = comparison_tab$weighted_house / comparison_tab$weighted_pop

comparison_tab = merge(comparison_tab, redline[c('Area', 'red_ratio')], by = "Area")
comparison_tab = comparison_tab[comparison_tab$Area!=712,]

# comparison_tab = comparison_tab %>% filter((percent_black_alone >=0.85) & 
#                                              (weighted_social_vulnerability >=0.75) &
#                                              (weighted_income >=35000) &
#                                              (weighted_income <=60000) & 
#                                              (weighted_pop) >=4500)

old <- options(pillar.sigfig = 6)
mod_match <- matchit(treated ~ percent_black_alone + weighted_income+ weighted_pop+
                       num_house_SNAP + weighted_social_vulnerability + red_ratio,
                     method = "nearest", data = comparison_tab, ratio = 2)
summary(mod_match)
new_names = c(percent_black_alone = "Percent of residents that are Black",
              weighted_pop = "Population",
              weighted_income = "Mean Income",
              num_house_SNAP = "Percent of households on SNAP",
              weighted_social_vulnerability = "Percent of individuals with one or \nmore social vulnerability factors",
              red_ratio = "Redlining Ratio")
love_plot = love.plot(mod_match, binary = 'std', var.names = new_names,
                      line = T, abs = T, drop.distance = T, shapes = c("triangle", "circle"),
                      color = hue_pal()(2)) + theme(text = element_text(size = 16)) 
ggsave('output/covariate_balance.png', width = 1000, height = 1000,
       units = 'px')
ggsave('output/covariate_balance.eps')

comparison_tab = match.data(mod_match)

comparison_tab %>% group_by(treated) %>%summarise(count = n(),
                            mean_pop = mean(weighted_pop),
                            mean_percent_black = mean(percent_black_alone),
                            mean_SNAP_houses = mean(num_house_SNAP),
                            mean_social_vulnerability = mean(weighted_social_vulnerability),
                            mean_red = mean(red_ratio))
# 
comparison_tab %>% group_by(treated) %>%summarise(count = n(),
                            std_pop = sd(weighted_pop),
                            std_percent_black = sd(percent_black_alone),
                            std_SNAP_houses = sd(num_house_SNAP),
                            std_social_vulnerability = sd(weighted_social_vulnerability),
                            std_red = sd(red_ratio))
# 
comparison_tab %>% group_by(treated) %>%summarise(count = n(),
                            min_pop = min(weighted_pop),
                            min_percent_black = min(percent_black_alone),
                            min_SNAP_houses = min(num_house_SNAP),
                            min_social_vulnerability = min(weighted_social_vulnerability),
                            min_red = min(red_ratio))

comparison_tab %>% group_by(treated) %>%summarise(count = n(),
                            max_pop = max(weighted_pop),
                            max_percent_black = max(percent_black_alone),
                            max_SNAP_houses = max(num_house_SNAP),
                            max_social_vulnerability = max(weighted_social_vulnerability),
                            max_red = max(red_ratio))
# 

rm(list=setdiff(ls(), c("comparison_tab", 'treated_scas')))
library(fixest)
library(data.table)
library(plyr)
library(dplyr)
library(xtable)
library(ggplot2)
library(stringr)
library(tidyr)
library(did)
library(lmtest)
library(lubridate)

source('covariate_processing.R')
source('ss_locations.R')

# df = read.csv('../clean_updated_gunshots.csv')
df = read.csv('../data/clean_updated_RMS.csv') %>% 
  filter((precinct > 0) & 
           (scout_car_area > 0) & 
           (scout_car_area %% 100 != 0) & 
           (offense_description %in% c('NON-FATAL SHOOTING')))

df['scout_car_area'] = as.factor(df[,'scout_car_area'])
df['treated'] = ifelse(df$scout_car_area %in% treated_scas, T, F)

df
ggplot(df) + geom_bar(aes(x = scout_car_area, fill = treated)) +
  scale_x_discrete(guide = guide_axis(angle = 90))

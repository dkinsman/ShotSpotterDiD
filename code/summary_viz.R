library(dplyr)
library(ggplot2)
library(lubridate)
library(ggtext)

source('covariate_processing.R')

# 911 Calls ---------------------------------------------------------------

## Read in call data pulled via the api
df = read.csv('../data/clean_updated_gunshots.csv')

## Create Needed Variables 
treated_scas =  c(803,804, 807,808, 902,903, 906,907)
sca = unique(df[,'sca'])
df[,'day'] = as.Date(df[,'date'])

wtreat = week(as.Date('2021-03-16')) + (2021-2016)*52
wcap = week(as.Date('2022-09-30'))+(2022-2016)*52

## Filter out weeks & SCAs not in our groups or time range
df[, 'week'] = week(df[,'day']) + (year(df[,'day']) - 2016) * 52
df = df[df$week<wcap,]
df = df[df$sca %in% comparison_tab$Area,] %>% filter(week >=111)

## Function to aggregate data to weekly counts per SCA
df_to_count = function(dfw) {
  dfw_counts = dfw %>% dplyr::count(sca, sort=TRUE)
  dfw_counts = merge(dfw_counts, comparison_tab[c('treated', 'Area')], 
                     by.x = 'sca', by.y = 'Area') %>%
    arrange(-sca)
  
  return(dfw_counts)
}

df_to_week_mean = function(dfw){
  dfw_count = dplyr::count(dfw, sca, week)
  total_combs = expand.grid(week = unique(dfw$week),
                            sca = unique(dfw$sca))
  dfw_count = merge(dfw_count, total_combs, all = T)
  dfw_count['n'] = dfw_count$n %>% replace_na(0)
  
  dfw_count = dfw_count %>% group_by(sca) %>% 
    summarize(mean = mean(n), sd = sd(n)) 
  
  dfw_count = merge(dfw_count, comparison_tab[c('treated', 'Area')], 
                     by.x = 'sca', by.y = 'Area') %>%
    arrange(-sca)
  return(dfw_count)
}

# SS
dfw = df[((df$category %in% c("SHOTSPT ", 'SHOT SPT')) &
           (df$sca %in% treated_scas)),] ## for some untreated SCAs, bordering 
          ## treated SCAs, there are some alerts.
df_ss = df_to_count(dfw)
df_ss_means = df_to_week_mean(dfw)

# NON-SS
dfw = df[df$category %in% c('SHOTS IP', 'SHOTS JH'),]
df_911 = df_to_count(dfw)
df_911_means= df_to_week_mean(dfw)

## Calls visualization
df_ss$type = "ShotSpotter Alerts"
df_911$type = "911 Calls"
plot_calls_df = rbind(df_ss, df_911)

# Total Counts
ggplot(plot_calls_df, aes(x = as.factor(sca), y = n) ) +
  geom_col(aes(fill = treated)) + facet_wrap(~type, scale = "free_x")+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
        plot.title = element_text(hjust = 0.5))+
  xlab("SCA") + ylab("Count") +
  labs(title = "Total Number of Gunshot Incidents", fill = "Treated") +
  geom_text(aes(label = n), size = 2.5, vjust=-0.25)

ggsave('output/calls_summary_totals.png', width = 3000, 
       height = 2000, units = 'px')
ggsave('output/calls_summary_totals.eps')

# Average Counts
df_ss_means$type = "ShotSpotter Alerts"
df_911_means$type = "911 Calls"
plot_calls_df = rbind(df_ss_means, df_911_means)

ggplot(plot_calls_df, aes(x = as.factor(sca), y = mean) ) +
  geom_col(aes(fill = treated)) + facet_wrap(~type, scale = "free_x")+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
        plot.title = element_text(hjust = 0.5))+
  xlab("SCA") + ylab("Count / week") +
  labs(title = "Average Number of Gunshot Incidents per Week", fill = "Treated") +
  geom_pointrange(aes(ymin=mean-sd, ymax=mean+sd), color = "black") +
  geom_richtext(aes(label = round(mean,1)), size = 3,
                color = "black", label.padding = grid::unit(rep(1, 2), "pt"))

ggsave('output/calls_summary_means.png', width = 3000, 
       height = 2000, units = 'px')

# RMS Calls ---------------------------------------------------------------

df = read.csv('../data/clean_updated_RMS.csv') %>%
  filter((precinct > 0) &
           (scout_car_area > 0) &
           (scout_car_area %% 100 != 0))

# Data prepping
df[, 'sca'] = df$scout_car_area
df[, 'day'] = as.Date(df[, 'date'])
df[, 'year'] = year(df[, 'day'])
df[, 'month'] = month(df[, 'day'])

sca_map = df[, c('sca', 'precinct')] %>% distinct()

wtreat = week(as.Date('2021-03-16')) + (2021 - 2016) * 52
wcap = week(as.Date('2022-09-30')) + (2022 - 2016) * 52
df[, 'week'] = week(df[, 'day']) + (year(df[, 'day']) - 2016) * 52

mtreat = month(as.Date('2021-03-16')) + (2021 - 2016) * 12
mcap = month(as.Date('2022-09-30')) + (2022 - 2016) * 12
df[, 'month'] = month(df[, 'day']) + (year(df[, 'day']) - 2016) * 12

df_shootings = df %>% filter(offense_description %in% c('NON-FATAL SHOOTING'))
df_shootings_count = df_to_count(df_shootings)
df_shootings_count$type = "Non-fatal Shooting"

df_homicides = df %>% filter(offense_category %in% c('HOMICIDE'))
df_homicides_count = df_to_count(df_homicides)
df_homicides_count$type = "Homicide"
plot_calls_df = rbind(df_shootings_count, df_homicides_count)

ggplot(plot_calls_df, aes(x = as.factor(sca), y = n) ) +
  geom_col(aes(fill = treated)) + facet_wrap(~type, scale = "free_x")+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
        plot.title = element_text(hjust = 0.5))+
  xlab("SCA") + ylab("Count") +
  labs(title = "Total Number of RMS Arrests", fill = "Treated") +
  geom_text(aes(label = n), size = 3, vjust=-0.25)

ggsave('output/rms_summary_totals.png', width = 3000, 
       height = 2000, units = 'px')
ggsave('output/rms_summary_totals.eps')

df_to_month_mean = function(dfw){
  dfw_count = dplyr::count(dfw, sca, week)
  total_combs = expand.grid(week = unique(dfw$week),
                            sca = unique(dfw$sca))
  dfw_count = merge(dfw_count, total_combs, all = T)
  dfw_count['n'] = dfw_count$n %>% replace_na(0)
  
  dfw_count = dfw_count %>% group_by(sca) %>% 
    summarize(mean = mean(n), sd = sd(n)) 
  
  dfw_count = merge(dfw_count, comparison_tab[c('treated', 'Area')], 
                    by.x = 'sca', by.y = 'Area') %>%
    arrange(-sca)
  return(dfw_count)
}

# df_ss_means$type = "ShotSpotter Alerts"
# df_911_means$type = "911 Calls"
# plot_calls_df = rbind(df_ss_means, df_911_means)
# 
# ggplot(plot_calls_df, aes(x = as.factor(sca), y = mean) ) +
#   geom_col(aes(fill = treated)) + facet_wrap(~type, scale = "free_x")+
#   theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
#         plot.title = element_text(hjust = 0.5))+
#   xlab("SCA") + ylab("Count / week") +
#   labs(title = "Average Number of Gunshot Incidents per Week", fill = "Treated") +
#   geom_pointrange(aes(ymin=mean-sd, ymax=mean+sd), color = "black") +
#   geom_richtext(aes(label = round(mean,1)), size = 3,
#                 color = "black", label.padding = grid::unit(rep(1, 2), "pt"))
# 
# ggsave('output/calls_summary_means.png', width = 3000, 
#        height = 2000, units = 'px')
# ggsave('output/calls_summary_means.eps')
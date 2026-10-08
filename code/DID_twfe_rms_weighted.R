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

# df = read.csv('../clean_updated_gunshots.csv')
df = read.csv('../data/clean_updated_RMS.csv') %>% 
  filter((precinct > 0) & 
           (scout_car_area > 0) & 
           (scout_car_area %% 100 != 0))

# SCA ---------------------------------------------------------------------

df[,'sca'] = df$scout_car_area
df[,'day'] = as.Date(df[,'date'])
df[,'year'] = year(df[,'day'])
df[,'month'] = month(df[,'day'])

# View(df %>% filter(day >= '2018-01-01') %>% 
#   group_by(across(all_of(c('year', 'month')))) %>% 
#   summarise(count = n(), num_scas = length(unique(scout_car_area))))

sca_map = df[,c('sca', 'precinct')] %>% distinct()

wtreat = week(as.Date('2021-03-16')) + (2021-2016)*52
wcap = week(as.Date('2022-09-30'))+(2022-2016)*52
df[, 'week'] = week(df[,'day']) + (year(df[,'day']) - 2016) * 52

mtreat = month(as.Date('2021-03-16')) + (2021-2016)*12
mcap = month(as.Date('2022-09-30'))+(2022-2016)*12
df[, 'month'] = month(df[,'day']) + (year(df[,'day']) - 2016) * 12

df_to_count = function(df, covariates = comparison_tab) {
  dfw[, 'week'] = week(dfw[,'day']) + (year(dfw[,'day']) - 2016) * 52
  
  dfw = dfw[dfw$week<wcap,]
  dfw = dfw[dfw$sca %in% comparison_tab$Area,]
  dfw_count = count(dfw, sca, week)
  
  dfw_count = dfw_count %>% filter(week >=111)
  total_combs = expand.grid(week = unique(dfw_count$week),
                            sca = unique(dfw_count$sca))
  dfw_count = merge(dfw_count, total_combs, all = T)
  dfw_count['n'] = dfw_count$n %>% replace_na(0)
  
  dfw_count[,'precinct'] = ifelse(nchar(dfw_count[,'sca']) < 4,
                                  ifelse(substr(dfw_count[,'sca'], 1, 1) == 1, 
                                         substr(dfw_count[,'sca'], 1, 2),
                                         substr(dfw_count[,'sca'], 1, 1)),
                                  substr(dfw_count[,'sca'], 1, 2))
  dfw_count[,'treat'] = ifelse(dfw_count[,'sca'] %in% treated_scas, 1, 0)
  dfw_count[, 'time_to_treat'] = ifelse(dfw_count[, 'treat'] == 1,
                                        dfw_count[, 'week'] - wtreat, 0)
  
  dfw_count = merge(dfw_count, comparison_tab, by.x = 'sca', by.y = 'Area')
  return(dfw_count)
}


# Models --------------------------------------------------------------------

dfw = df %>% filter(offense_description %in% c('NON-FATAL SHOOTING'))
dfw_count = df_to_count(dfw)

mod_twfe = feols(n~ i(time_to_treat, treat, ref = -1) +
                   percent_black_alone + weighted_income + num_house_SNAP + 
                   weighted_social_vulnerability + red_ratio| 
                   precinct + week,                             
                 cluster = ~precinct,  
                 weights = ~weighted_pop,
                 data = dfw_count)

tab = mod_twfe$coeftable
summary(mod_twfe)
# mean(tab[134:nrow(tab),1])
print(xtable(tab, digits = 3))

setEPS()
postscript('output/non-fatal/TWFE_weeksNONSS.eps')
# png('output/non-fatal/TWFE_weeksNONSS.png', res = 300, width = 3000, height = 2000)
iplot(mod_twfe, 
      xlab = 'Time to treatment (Months)',
      main = 'TWFE Estimates of Non-fatal Shooting Arrests')
dev.off()

dfw_count[,'time'] = ifelse(dfw_count[,'week'] >=wtreat,
                            1, 0)
dfw_count[,'did'] = dfw_count$time * dfw_count$treat

model_did = feols(n ~ did + percent_black_alone + weighted_income + num_house_SNAP +
                    weighted_social_vulnerability + red_ratio|
                    time + treat,
                  cluster = ~precinct,
                  weights = ~weighted_pop,
                  dfw_count)
summary(model_did)
coefplot(model_did)
did.coef = model_did$coefficients[['did']]

df_date2016 = function(df){
  col_week = (unlist(df[,'week'])) %% 52
  year = ifelse((col_week %%52) < (unlist(df[,'week']) %% 52), 
                unlist(df[,'week']) %/% 52 + 2016 +1,
                unlist(df[,'week'])  %/% 52 + 2016)
  str.date = paste(year, col_week,7, sep = '-')
  df[,'date'] = as.Date(str.date, '%Y-%U-%u')
  return(df)
}
df_comp = df_date2016(df_comp)

df_comp %>% 
  ggplot(aes(x = date, y = mean)) +
  geom_point(aes(color = call), position = position_dodge(0.1)) +
  geom_line(aes(color = call), position = position_dodge(0.1)) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se, color = call), 
                alpha = 0.5, position = position_dodge(0.1))+
  # geom_vline(xintercept = 269, linetype = 'dashed', size = 1) +
  # geom_text(aes(x=264, y = 7, label="Treated Week"), angle = 90) +
  labs(title = "Trends for ShotSpotter Alerts and 911 Gunshot Related 911 Calls",
       x = "Week",
       y = "Mean Number of Calls") +
  scale_color_discrete(name = 'Type of Call',
                       labels = c('911 Gunshot Call', 'ShotSpotter Alert'))+
  scale_x_date(date_breaks = "3 months", date_minor_breaks = "1 week",
               date_labels = "%b %Y") +
  theme_minimal()
ggsave('output/non-fatal/mean_trend_SvG.eps', device = "eps")

cor(df_ss_mean$mean,df_911_mean$mean)
setEPS()
postscript('output/non-fatal/cross_corr.eps')
# png('output/non-fatal/cross_corr.png', res = 300, width = 3000, height = 2000)
ccf(df_ss_mean$mean,df_911_mean$mean,
    main = "Cross-correlation Estimates Between \nShotSpotter Alerts and 911 Gunshot Calls")
dev.off()
print(ccf(df_ss_mean$mean,df_911_mean$mean))

df_means = df_date2016(df_means)

df_means %>% 
  ggplot(aes(x = date, y = dif)) +
  geom_point() +
  geom_line() +
  labs(title = "Difference in Mean Number of ShotSpotter Alerts and 911 Gunshot Related Calls",
       x = "Week",
       y = "Mean Number of Calls") +
  scale_x_date(date_breaks = "3 months", date_minor_breaks = "1 week",
               date_labels = "%b %Y") +
  theme_minimal()
ggsave('output/non-fatal/diff_trend_SvG.eps', device = "eps")

week_means = dfw_count %>% group_by(treat, week) %>% summarise(mean = mean(n, na.rm = T),
                                                               se = sd(n, na.rm = T)/sqrt(length(n)),
                                                               sd = sd(n))
week_means$treat = as.factor(week_means$treat)
week_means = df_date2016(week_means)

setEPS()
postscript('output/non-fatal/mean_trends_weeksNONSS.eps')
# png('output/non-fatal/mean_trends_weeksNONSS.png', res = 300, width = 3000, height = 2000)
week_means %>% 
  ggplot(aes(x = date, y = mean)) +
  geom_point(aes(color = treat)) +
  geom_line(aes(color = treat)) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se, color = treat), 
                alpha = 0.5, position = position_dodge(0.1)) +
  # geom_ribbon(aes(ymin = mean - se, ymax = mean + se), alpha = 0.3)+
  labs(title = "Trends for Treated and Control Groups",
       x = "Week",
       y = "Mean Gunshot related 911 calls") +
  scale_color_discrete(name = 'Groups', 
                       labels = c('Control', 'Treated'))+
  scale_x_date(date_breaks = "6 months", date_minor_breaks = "1 week",
               date_labels = "%b %Y") +
  geom_vline(xintercept = as.Date('2021-03-16'), linetype = 'dashed', size = 1) +
  geom_text(aes(x=as.Date('2021-02-10'), y = .7, label="Treated Week"), angle = 90) +
  ylim (0,.8)+
  theme_minimal()
dev.off()


dfw_count %>%  group_by(treat, time) %>% 
  summarise(mean = mean(n), sd = sd(n),
            count = n(),
            min = min(n),
            max = max(n)) %>% mutate(delta = did.coef/mean)

overall_n = dfw_count %>% 
  group_by(treat, week) %>% 
  summarise(count = sum(n))

cor(overall_n[(overall_n$treat ==0) & (overall_n$week < wtreat),'count'],
    overall_n[(overall_n$treat ==1) & (overall_n$week < wtreat),'count'])

# Homicides ---------------------------------------------------------------

df_to_count = function(df, covariates = comparison_tab) {
  dfm = df
  dfm = dfm[dfm$month<mcap,]
  
  dfm_count = count(dfm, sca, month)
  
  dfm_count = dfm_count %>% filter(month >=26) #filter(month >=1)
  total_combs = expand.grid(month = unique(dfm_count$month),
                            sca = unique(dfm_count$sca))
  dfm_count = merge(dfm_count, total_combs, all = T)
  dfm_count['n'] = dfm_count$n %>% replace_na(0)
  
  dfm_count[,'treat'] = ifelse(dfm_count[,'sca'] %in% treated_scas, 1, 0)
  
  dfm_count[,'precinct'] = mapvalues(dfm_count$sca, sca_map$sca, sca_map$precinct,
                                     warn_missing = F)
  
  dfm_count[, 'time_to_treat'] = ifelse(dfm_count[, 'treat'] == 1,
                                        dfm_count[, 'month'] - mtreat, 0)
  
  dfm_count = merge(dfm_count, comparison_tab, by.x = 'sca', by.y = 'Area')
  return(dfm_count)
}


dfw = df %>% filter(offense_category %in% c('HOMICIDE'))
dfw_count = df_to_count(dfw)

mod_twfe = feols(n~ i(time_to_treat, treat, ref = -1) +
                   percent_black_alone + weighted_income + num_house_SNAP +
                   weighted_social_vulnerability + red_ratio| 
                   precinct + month,                             
                 cluster = ~precinct,  
                 weights = ~weighted_pop,
                 data = dfw_count)

tab = mod_twfe$coeftable
print(xtable(tab, digits = 3))

setEPS()
postscript('output/homicides/TWFE_weeksNONSS.eps')
# png('output/homicides/TWFE_weeksNONSS.png', res = 300, width = 3000, height = 2000)
iplot(mod_twfe, 
      xlab = 'Time to treatment (Months)',
      main = 'TWFE Estimates of Homicide Arrests')
dev.off()

dfw_count[,'time'] = ifelse(dfw_count[,'month'] >=mtreat,
                            1, 0)
dfw_count[,'did'] = dfw_count$time * dfw_count$treat
model_did = feols(n ~ did + percent_black_alone + weighted_income + num_house_SNAP + 
                    weighted_social_vulnerability + red_ratio|
                    time + treat,
                  cluster = ~precinct,
                  weights = ~weighted_pop,
                  dfw_count)
summary(model_did)
# coefplot(model_did)
did.coef = model_did$coefficients[['did']]

# Difference in means plot
# df_ss[,'call'] = 'ShotSpotter'
# df_911[,'call'] = '911 Call'
# df_comp = rbind(df_ss, df_911) %>% 
#   filter(week >=wtreat, precinct %in% c(8,9)) %>% 
#   group_by(call, week) %>% summarise(mean = mean(n), sd = sd(n), 
#                                      se = sd(n)/sqrt(length(n))) %>% 
#   mutate(lower = mean -se, upper = mean+se,
#          count = n())

# df_ss_mean = df_comp %>% filter(call == 'ShotSpotter')
# df_911_mean = df_comp %>%  filter(call== '911 Call')
# df_means = full_join(df_ss_mean, df_911_mean, by = c("month"), suffix =c('_ss', '_gun')) %>% 
# mutate(dif = mean_ss-mean_gun)

df_date2016 = function(df){
  col_week = (unlist(df[,'month'])) %% 12
  # year = ifelse((col_week %/%12) < (unlist(df[,'month']) %% 12), 
  #               unlist(df[,'month']) %/% 12 + 2016 +1,
  #               unlist(df[,'month'])  %/% 12 + 2016)
  col_week[col_week == 0] = 12
  year = df['month'] %/%12 +2016
  col_week = sprintf("%02d", col_week)
  # str.date = paste(year, col_week,7, sep = '-')
  date_col = cbind(year, col_week, df$month)
  date_col[date_col$col_week == '12', 'month'] = date_col[date_col$col_week == '12', 'month'] - 1
  date_col['date'] = paste(date_col$month, date_col$col_week, 1, sep = '-')
  df[,'date'] = as.Date(date_col$date, format = '%Y-%m-%d')
  return(df)
}
# df_comp = df_date2016(df_comp)

# df_comp %>% 
#   ggplot(aes(x = date, y = mean)) +
#   geom_point(aes(color = call), position = position_dodge(0.1)) +
#   geom_line(aes(color = call), position = position_dodge(0.1)) +
#   geom_errorbar(aes(ymin = mean - se, ymax = mean + se, color = call), 
#                 alpha = 0.5, position = position_dodge(0.1))+
#   # geom_vline(xintercept = 269, linetype = 'dashed', size = 1) +
#   # geom_text(aes(x=264, y = 7, label="Treated Week"), angle = 90) +
#   labs(title = "Trends for ShotSpotter Alerts and 911 Gunshot Related 911 Calls",
#        x = "Week",
#        y = "Mean Number of Calls") +
#   scale_color_discrete(name = 'Type of Call',
#                        labels = c('911 Gunshot Call', 'ShotSpotter Alert'))+
#   scale_x_date(date_breaks = "3 months", date_minor_breaks = "1 week",
#                date_labels = "%b %Y") +
#   theme_minimal()
# ggsave('output/homicides/mean_trend_SvG.png', width = 3000, height = 2000,
#        units = 'px')

# cor(df_ss_mean$mean,df_911_mean$mean)
# png('output/homicides/cross_corr.png', res = 300, width = 3000, height = 2000)
# ccf(df_ss_mean$mean,df_911_mean$mean,
#     main = "Cross-correlation Estimates Between \nShotSpotter Alerts and 911 Gunshot Calls")
# dev.off()
# print(ccf(df_ss_mean$mean,df_911_mean$mean))

# df_means = df_date2016(df_means)

# df_means %>% 
#   ggplot(aes(x = date, y = dif)) +
#   geom_point() +
#   geom_line() +
#   labs(title = "Difference in Mean Number of ShotSpotter Alerts and 911 Gunshot Related Calls",
#        x = "Week",
#        y = "Mean Number of Homicide Arrests") +
#   scale_x_date(date_breaks = "3 months", date_minor_breaks = "1 week",
#                date_labels = "%b %Y") +
#   theme_minimal()
# ggsave('output/homicides/diff_trend_SvG.png', width = 3000, height = 2000,
#        units = 'px')

week_means = dfw_count %>% group_by(treat, month) %>% summarise(mean = mean(n, na.rm = T),
                                                                se = sd(n, na.rm = T)/sqrt(length(n)),
                                                                sd = sd(n))
week_means$treat = as.factor(week_means$treat)
week_means = df_date2016(week_means)
# 
setEPS()
postscript("output/homicides/mean_trends_weeks.eps")
# png('output/homicides/mean_trends_weeks.png', res = 300, width = 3000, height = 2000)
week_means %>%
  ggplot(aes(x = date, y = mean)) +
  geom_point(aes(color = treat)) +
  geom_line(aes(color = treat)) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se, color = treat),
                alpha = 0.5, position = position_dodge(0.1)) +
  # geom_ribbon(aes(ymin = mean - se, ymax = mean + se), alpha = 0.3)+
  labs(title = "Trends for Treated and Control Groups",
       x = "Month",
       y = "Mean Number of Homicide Arrests") +
  scale_color_discrete(name = 'Groups',
                       labels = c('Control', 'Treated'))+
  scale_x_date(date_breaks = "6 months", date_minor_breaks = "1 month",
               date_labels = "%b %Y") +
  geom_vline(xintercept = as.Date('2021-03-16'), linetype = 'dashed', size = 1) +
  geom_text(aes(x=as.Date('2021-02-15'), y =0.8, label="Treated Week"), angle = 90) +
  ylim (0,.9) +
  theme_minimal()
dev.off()


dfw_count %>%  group_by(treat, time) %>% 
  summarise(mean = mean(n), sd = sd(n),
            count = n(),
            min = min(n),
            max = max(n)) %>% mutate(delta = did.coef/mean)

overall_n = dfw_count %>% 
  group_by(treat, month) %>% 
  summarise(count = sum(n))

cor(overall_n[(overall_n$treat ==0) & (overall_n$month < wtreat),'count'],
    overall_n[(overall_n$treat ==1) & (overall_n$month < wtreat),'count'])

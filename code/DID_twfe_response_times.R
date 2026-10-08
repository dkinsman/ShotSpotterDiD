### WEIGHTED DID FOR RESPONSE TIMES

library(fixest)
library(data.table)
library(plyr)
library(dplyr)
library(xtable)
library(ggplot2)
library(stringr)
library(tidyr)
library(did)
library(plm)
library(marginaleffects)

source('covariate_processing.R')
# source('ss_locations.R')

df = read.csv('../data/clean_updated_gunshots.csv')

# SCA ---------------------------------------------------------------------
treated_scas =  c(803,804, 807,808, 902,903, 906,907)


sca = unique(df[,'sca'])
df[,'day'] = as.Date(df[,'date'])

wtreat = week(as.Date('2021-03-16')) + (2021-2016)*52
# wcap = week(as.Date('2022-11-30'))+(2022-2016)*52
wcap = week(as.Date('2022-09-30'))+(2022-2016)*52

df_to_week_mean_response = function(dfw) {
  dfw[, 'week'] = week(dfw[,'day']) + (year(dfw[,'day']) - 2016) * 52

  dfw = dfw %>% filter(dfw$week<wcap, dfw$sca %in% comparison_tab$Area, 
                       totalresponsetime>0)
  dfw_count = dfw %>% group_by(sca, week) %>% summarise(mean_rt = mean(totalresponsetime,
                                                                       na.rm = T))

  dfw_count = dfw_count %>% filter(week >=111) # Minimum week
  total_combs = expand.grid(week = unique(dfw_count$week),
                            sca = unique(dfw_count$sca))
  dfw_count = merge(dfw_count, total_combs, all = T)
  # dfw_count['n'] = dfw_count$n %>% replace_na(0)
  
  dfw_count[,'precinct'] = ifelse(nchar(dfw_count[,'sca']) < 4,
                                  ifelse(substr(dfw_count[,'sca'], 1, 1) == 1,
                                         substr(dfw_count[,'sca'], 1, 2),
                                         substr(dfw_count[,'sca'], 1, 1)),
                                  substr(dfw_count[,'sca'], 1, 2))
  dfw_count[,'treat'] = ifelse(dfw_count[,'sca'] %in% treated_scas, 1, 0)
  dfw_count[, 'time_to_treat'] = ifelse(dfw_count[, 'treat'] == 1,
                                        dfw_count[, 'week'] - wtreat, 0)
  # NO NAs (besides council district) from above
  # I don't think this is a coding issue tho, some weeks might not have any calls,
  # thus NA as the mean
  
  dfw_count = merge(dfw_count, comparison_tab, by.x = 'sca', by.y = 'Area')
  return(dfw_count)
}

# SS ----------------------------------------------------------------------

dfw = df[df$category %in% c("SHOTSPT ", 'SHOT SPT'),]
df_ss = df_to_week_mean_response(dfw)

# NON-SS ----------------------------------------------------------------

dfw = df[df$category %in% c('SHOTS IP', 'SHOTS JH'),]
df_911 = df_to_week_mean_response(dfw)
dfw_count = df_911


# Models --------------------------------------------------------------------

### Dynamic Diff-in-diff
# mod_twfe = feols(mean_rt~ i(time_to_treat, treat, ref = -1) + 
#                    percent_black_alone + weighted_income + num_house_SNAP + 
#                    weighted_social_vulnerability + red_ratio| 
#                    precinct + week,                             
#                  cluster = ~precinct,
#                  weights = ~weighted_pop,
#                  data = dfw_count)
# 
# tab = mod_twfe$coeftable
# mean(tab[134:nrow(tab),1])
# print(xtable(tab, digits = 3))
# 
# png('output/calls/TWFE_weeksNONSS_response.png', res = 300, width = 3000, height = 2000)
# iplot(mod_twfe, 
#       xlab = 'Time to treatment (Weeks)',
#       main = 'TWFE Estimates of Gunshot Related 911 calls')
# dev.off()

### Diff-in-diff Weighted
dfw_count[,'time'] = ifelse(dfw_count[,'week'] >=wtreat,
                            1, 0)
dfw_count[,'did'] = dfw_count$time * dfw_count$treat

model_did = feols(mean_rt ~ did + percent_black_alone + weighted_income + num_house_SNAP +
                    weighted_social_vulnerability + red_ratio|
                    time + treat,
                  cluster = ~precinct,
                  weights = ~weighted_pop,
                  data = dfw_count)
summary(model_did)
coefplot(model_did)

### Diff-in-diff Unweighted
# dfw_count[,'time'] = ifelse(dfw_count[,'week'] >=wtreat,
#                             1, 0)
# dfw_count[,'did'] = dfw_count$time * dfw_count$treat

model_did = feols(mean_rt ~ did + percent_black_alone + weighted_income + num_house_SNAP +
                    weighted_social_vulnerability + red_ratio|
                    time + treat,
                  cluster = ~precinct,
                  # weights = ~weighted_pop,
                  data = dfw_count)
summary(model_did)
coefplot(model_did)

# did.coef = model_did$coefficients[['did']]
# 
# # Difference in means plot
# df_ss[,'call'] = 'ShotSpotter'
# df_911[,'call'] = '911 Call'
# df_comp = rbind(df_ss, df_911) %>% 
#   filter(week >=wtreat, sca %in% comparison_tab$Area) %>% 
#   group_by(call, week) %>% summarise(mean = mean(mean_rt, na.rm = T), sd = sd(mean_rt, na.rm = T), 
#                                      se = sd(mean_rt)/sqrt(length(mean_rt))) %>% 
#   mutate(lower = mean -se, upper = mean+se)
#          # count = n())
# 
# df_ss_mean = df_comp %>% filter(call == 'ShotSpotter')
# df_911_mean = df_comp %>%  filter(call== '911 Call')
# df_means = full_join(df_ss_mean, df_911_mean, by = "week", suffix =c('_ss', '_gun')) %>% 
#   mutate(dif = mean_ss-mean_gun)
# 
# df_date2016 = function(df){
#   col_week = (unlist(df[,'week'])) %% 52
#   year = ifelse((col_week %%52) < (unlist(df[,'week']) %% 52), 
#                 unlist(df[,'week']) %/% 52 + 2016 +1,
#                 unlist(df[,'week'])  %/% 52 + 2016)
#   str.date = paste(year, col_week,7, sep = '-')
#   df[,'date'] = as.Date(str.date, '%Y-%U-%u')
#   return(df)
# }
# df_comp = df_date2016(df_comp)
# 
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
# ggsave('output/calls/mean_trend_SvG_response.png', width = 3000, height = 2000,
#        units = 'px')
# 
# cor(df_ss_mean$mean,df_911_mean$mean)
# png('output/calls/cross_corr_response.png', res = 300, width = 3000, height = 2000)
# ccf(df_ss_mean$mean,df_911_mean$mean,
#     main = "Cross-correlation Estimates Between \nShotSpotter Alerts and 911 Gunshot Calls")
# dev.off()
# print(ccf(df_ss_mean$mean,df_911_mean$mean))
# 
# df_means = df_date2016(df_means)
# 
# df_means %>% 
#   ggplot(aes(x = date, y = dif)) +
#   geom_point() +
#   geom_line() +
#   labs(title = "Difference in Mean Number of ShotSpotter Alerts and 911 Gunshot Related Calls",
#        x = "Week",
#        y = "Mean Number of Calls") +
#   scale_x_date(date_breaks = "3 months", date_minor_breaks = "1 week",
#                date_labels = "%b %Y") +
#   theme_minimal()
# ggsave('output/calls/diff_trend_SvG_response.png', width = 3000, height = 2000,
#        units = 'px')
# 
# week_means = dfw_count %>% group_by(treat, week) %>% summarise(mean = mean(n, na.rm = T),
#                                                                se = sd(n, na.rm = T)/sqrt(length(n)),
#                                                                sd = sd(n))
# week_means$treat = as.factor(week_means$treat)
# week_means = df_date2016(week_means)
# 
# png('output/calls/mean_trends_weeksNONSS_response.png', res = 300, width = 3000, height = 2000)
# week_means %>% 
#   ggplot(aes(x = date, y = mean)) +
#   geom_point(aes(color = treat)) +
#   geom_line(aes(color = treat)) +
#   geom_errorbar(aes(ymin = mean - se, ymax = mean + se, color = treat), 
#                 alpha = 0.5, position = position_dodge(0.1)) +
#   # geom_ribbon(aes(ymin = mean - se, ymax = mean + se), alpha = 0.3)+
#   labs(title = "Trends for Treated and Control Groups",
#        x = "Week",
#        y = "Mean Gunshot related 911 calls") +
#   scale_color_discrete(name = 'Groups', 
#                        labels = c('Control', 'Treated'))+
#   scale_x_date(date_breaks = "6 months", date_minor_breaks = "1 week",
#                date_labels = "%b %Y") +
#   geom_vline(xintercept = as.Date('2021-03-16'), linetype = 'dashed', size = 1) +
#   geom_text(aes(x=as.Date('2021-02-10'), y = 4.5, label="Treated Week"), angle = 90) +
#   theme_minimal()
# dev.off()
# 
# 
dfw_count %>%  group_by(treat, time) %>%
  summarise(mean = mean(mean_rt), sd = sd(mean_rt),
            count = n(),
            se= sd/sqrt(count),
            min = min(mean_rt),
            max = max(mean_rt)) %>% mutate(delta = did.coef/mean)
# 
# overall_n = dfw_count %>% 
#   group_by(treat, week) %>% 
#   summarise(count = sum(n))
# 
# cor(overall_n[(overall_n$treat ==0) & (overall_n$week < wtreat),'count'],
#     overall_n[(overall_n$treat ==1) & (overall_n$week < wtreat),'count'])
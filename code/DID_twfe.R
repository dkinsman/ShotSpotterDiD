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
library(lmtest)

source('redline_ratio.R')

df = read.csv('../clean_updated_gunshots.csv')

# SCA ---------------------------------------------------------------------

sca = unique(df[,'sca'])
df[,'day'] = as.Date(df[,'date'])
# df[,'week'] = as.Date(df[,'week'], '%Y-%U')

wtreat = week(as.Date('2021-03-16')) + (2021-2016)*52
wcap = week(as.Date('2022-09-30'))+(2022-2016)*52

df_to_count = function(df) {
dfw[, 'week'] = week(dfw[,'day']) + (year(dfw[,'day']) - 2016) * 52

dfw = dfw[dfw$week<wcap,]
dfw_count = count(dfw, sca, week)

dfw_count = dfw_count %>% filter(week >=111)
total_combs = expand.grid(week = unique(dfw_count$week),
                        sca = unique(dfw_count$sca))
dfw_count = merge(dfw_count, total_combs, all = T)
dfw_count['n'] = dfw_count$n %>% replace_na(0)

dfw_count[,'precinct'] = ifelse(nchar(dfw_count[,'sca']) < 4,
                         ifelse(substr(dfw_count[,'sca'], 1, 1) == 1, substr(dfw_count[,'sca'], 1, 2),
                                substr(dfw_count[,'sca'], 1, 1)),substr(dfw_count[,'sca'], 1, 2))
dfw_count[,'treat'] = ifelse(dfw_count[,'precinct'] %in% c(8,9), 1, 0)
dfw_count[, 'time_to_treat'] = ifelse(dfw_count[, 'treat'] == 1,
                                      dfw_count[, 'week'] - wtreat, 0)
return(dfw_count)
}

# SS ----------------------------------------------------------------------

dfw = df[df$category %in% c("SHOTSPT ", 'SHOT SPT'),]
df_ss = df_to_count(dfw)

# NON-SS ----------------------------------------------------------------

dfw = df[df$category %in% c('SHOTS IP', 'SHOTS JH'),]
df_911 = df_to_count(dfw)
dfw_count = df_911

# Models --------------------------------------------------------------------

mod_twfe = feols(n~ i(time_to_treat, treat, ref = -1) | 
                   precinct + week,                             
                 cluster = ~precinct,                        
                 data = dfw_count)

tab = mod_twfe$coeftable
mean(tab[134:nrow(tab),1])
print(xtable(tab, digits = 3))

png('output/TWFE_weeksNONSS.png', res = 300, width = 3000, height = 2000)
iplot(mod_twfe, 
      xlab = 'Time to treatment (Weeks)',
      main = 'TWFE Estimates of Gunshot Related 911 calls')
dev.off()

dfw_count[,'time'] = ifelse(dfw_count[,'week'] >=wtreat,
                            1, 0)
dfw_count[,'did'] = dfw_count$time * dfw_count$treat
# feols(n ~ did + time + treat |
#         precinct + week,
#       cluster = ~precinct,
#       dfw_count)
model_did = feols(n ~ did + time + treat,
              cluster = ~precinct,
              dfw_count)
summary(model_did)
coefplot(model_did)
did.coef = model_did$coefficients[['did']]

panel <- pdata.frame(dfw_count, "sca")
did.reg <- lm(n ~ time*treat, 
               data = dfw_count)
# summary(did.reg)
# coeftest(did.reg, vcov = function(x) 
#   vcovHC(x, cluster = "group", type = "HC1"))

dfw_count[,'first.treat'] = ifelse(dfw_count[, 'treat'] == 1, 
                                   wtreat, 0)
agg = att_gt(yname = 'n', gname = 'first.treat', idname = 'sca', 
             tname = 'week', xformla = ~1, data = dfw_count, panel = T, 
             est_method = 'reg', clustervars = 'precinct')
summary(agg)
ggdid(agg)

agg.simple = aggte(agg, type = "simple")
summary(agg.simple)

agg.dynamic = aggte(agg, type = 'dynamic')
summary(agg.dynamic)
ggdid(agg.dynamic)

agg.group = aggte(agg, type = 'group')
ggdid(agg.group)
# Difference in means plot
df_ss[,'call'] = 'ShotSpotter'
df_911[,'call'] = '911 Call'
df_comp = rbind(df_ss, df_911) %>% 
  filter(week >=wtreat, precinct %in% c(8,9)) %>% 
  group_by(call, week) %>% summarise(mean = mean(n), sd = sd(n), 
                                     se = sd(n)/sqrt(length(n))) %>% 
  mutate(lower = mean -se, upper = mean+se,
         count = n())

df_ss_mean = df_comp %>% filter(call == 'ShotSpotter')
df_911_mean = df_comp %>%  filter(call== '911 Call')
df_means = full_join(df_ss_mean, df_911_mean, by = c("week"), suffix =c('_ss', '_gun')) %>% 
  mutate(dif = mean_ss-mean_gun)
# df_means[,'call'] = 'Difference'
# df_means[,'mean'] =  df_means[,'dif']
# df_comp = rbind(df_comp, df_means[,c('call', 'week', 'mean')])

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
ggsave('output/mean_trend_SvG.png', width = 3000, height = 2000,
       units = 'px')

  cor(df_ss_mean$mean,df_911_mean$mean)
png('output/cross_corr.png', res = 300, width = 3000, height = 2000)
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
ggsave('output/diff_trend_SvG.png', width = 3000, height = 2000,
       units = 'px')

week_means = dfw_count %>% group_by(treat, week) %>% summarise(mean = mean(n, na.rm = T),
                                                               se = sd(n, na.rm = T)/sqrt(length(n)),
                                                               sd = sd(n))
week_means$treat = as.factor(week_means$treat)
week_means = df_date2016(week_means)

png('output/mean_trends_weeksNONSS.png', res = 300, width = 3000, height = 2000)
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
  geom_text(aes(x=as.Date('2021-02-10'), y = 4.5, label="Treated Week"), angle = 90) +
  theme_minimal()
dev.off()
# 
# week_means_diff = full_join(week_means_tot, week_means, by = c("treat","week"),
#                             suffix = c('_total', '_gunshot'))
# week_means_diff %>% mutate(mean_diff = mean_total)


dfw_count %>%  group_by(treat, time) %>% 
  summarise(mean = mean(n), sd = sd(n),
            count = n(),
            min = min(n),
            max = max(n)) %>% mutate(delta = did.coef/mean)



# Redlining ---------------------------------------------------------------

# With the ratio of redlining as a control
dfw_count = merge(dfw_count, redline[c('properties$Area','red_ratio')], 
                  by.x = 'sca', by.y = 'properties$Area') 

redline[,'precinct'] = ifelse(nchar(redline[,1]) < 4,
                              ifelse(substr(redline[,1], 1, 1) == 1, substr(redline[,1], 1, 2),
                                     substr(redline[,1], 1, 1)),substr(redline[,1], 1, 2))
redline[,'treat'] = ifelse(redline[,'precinct'] %in% c(8,9), 1, 0)


dfw_count %>% distinct(sca, .keep_all = T) %>%
  group_by(treat) %>% 
  summarise(mean = mean(red_ratio),
            sd = sd(red_ratio),
            count = n(),
            min = min(red_ratio),
            max = max(red_ratio), count = n())


dfw_count$trt_time = dfw_count$treat * dfw_count$time
dfw_count$trt_red = dfw_count$treat * dfw_count$red_ratio
dfw_count$red_time = dfw_count$red_ratio * dfw_count$time
dfw_count$trt_time_red = dfw_count$treat * dfw_count$time * dfw_count$red_ratio

# Running the regression
model <- feols(n ~ treat + time + red_ratio + trt_time +  
                 trt_red + red_time + trt_time_red,
               cluster = ~precinct,
               dfw_count)

# Printing the model summary
summary(model)

png('output/ddd_coefNONSS.png', res = 300, width = 3000, height = 2000)
coefplot(model)
dev.off()

agg_red = att_gt(yname = 'n', gname = 'first.treat', idname = 'sca', 
             tname = 'week', xformla = ~red_ratio, data = dfw_count, panel = T, 
             est_method = 'reg', clustervars = 'precinct')
summary(agg_red)
ggdid(agg_red)

agg_red.simple = aggte(agg_red, type = "simple")
summary(agg_red.simple)

agg_red.dynamic = aggte(agg_red, type = 'dynamic')
summary(agg_red.dynamic)
ggdid(agg_red.dynamic) + ggtitle('DID + red_ratio')

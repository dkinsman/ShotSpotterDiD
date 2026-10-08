library(fixest)

non_ss = read.csv('./output/non-ss-counts.csv',)

wtreat = week(as.Date('2021-03-16')) + (2021-2016)*52

non_ss['policy'] = ifelse(non_ss$time_to_treat > 0, 1, 0)
non_ss['after'] = ifelse(non_ss$week >= wtreat, 1, 0)

classic = lm(n ~ treat*after, data = non_ss)
summary(classic)

muti_time_groups = lm(n ~ policy + as.factor(precinct) + as.factor(week), data = non_ss)
summary(muti_time_groups)

fix_model = feols(n ~ policy | precinct + week, data = non_ss)
summary(fix_model)

dynamic = feols(n ~ i(time_to_treat, treat,ref = -1) | precinct + week, data = non_ss)
summary(dynamic)
iplot(dynamic)

dynamic2 = feols(n ~ i( week, treat, ref = 111) | precinct + week, data = non_ss)
summary(dynamic2)
iplot(dynamic2)

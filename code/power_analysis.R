source('covariate_processing.R')
df = read.csv('../data/clean_updated_gunshots.csv')

treated_scas =  c(803,804, 807,808, 902,903, 906,907)
sca = unique(df[,'sca'])
df[,'day'] = as.Date(df[,'date'])

wtreat = week(as.Date('2021-03-16')) + (2021-2016)*52
# wcap = week(as.Date('2022-11-30'))+(2022-2016)*52
wcap = week(as.Date('2022-09-30'))+(2022-2016)*52

df_to_count = function(dfw) {
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
  
  # dfw_count = merge(dfw_count, comparison_tab, by.x = 'sca', by.y = 'Area')
  return(dfw_count)
}

df_count = df_to_count(df)
# df_ss = df_to_count(df[df$category %in% c("SHOTSPT ", 'SHOT SPT'),])
# df_911 = df_to_count(df[df$category %in% c('SHOTS IP', 'SHOTS JH'),])

n_treatment = 8
n_control = 16

df_count[,'time'] = ifelse(df_count[,'week'] >=wtreat,1,0)
df_summary = df_count %>%  group_by(treat, time) %>% 
  summarise(mean = mean(n), sd = sd(n),
            count = n(),
            se= sd/sqrt(count),
            min = min(n),
            max = max(n))


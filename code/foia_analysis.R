library(dplyr)
library(readxl)
library(stringr)

### GLOBAL VARIABLES
END_DATE = as.Date('2022-09-30')
by_month <- function(x,n=1){
  seq(min(x,na.rm=T),max(x,na.rm=T),by=paste0(n," months"))
}


df = read_xlsx('../data/raw/ShotSpotter CAD.xlsx')
unique(df$`Call Type`)
dim(df)


# ShotSpotter Calls -------------------------------------------------------

# Filter for SS calls and drop any duplicated rows
df_ss = df %>% filter(df$`Call Type` %in% c('SHOT SPOTTER', 'SHOTSPOTTER',
                                            'MULTI - SHOTSPOTTER',
                                            'SINGLE - SHOTSPOTTER')) %>% 
  distinct(.keep_all = TRUE)
dim(df_ss)
df_ss[c('date', 'time')] = str_split_fixed(df_ss$Calltime, ' ', 2)
df_ss$date = as.Date(df_ss$date, format = '%m/%d/%Y')
df_ss[duplicated(df_ss),,]

# Filter out entries after our end date
df_ss_lim = df_ss %>% filter(df_ss$date <= END_DATE)
dim(df_ss_lim)

# Summary of arrests & guns
print(paste0('Total Number of Arrests: ', sum(df_ss$Arrests)))
print(paste0('Total Number of Guns Found: ', sum(df_ss$Guns)))

print(paste0('Number of SS Alerts that resulted in an arrest: ', 
             dim(df_ss[df_ss$Arrests>0,,])[1]))
df_ss[df_ss$Arrests>0,,]

print(paste0('Number of SS Alerts that resulted in guns found: ', 
             dim(df_ss[df_ss$Guns>0,,])[1]))
# df_ss[df_ss$Guns>0,,]

ggplot(df_ss_lim, aes(date)) + geom_histogram(breaks = by_month(df_ss_lim$date)) +
  scale_x_date(labels = scales::date_format("%Y-%b"),
               breaks = by_month(df_ss_lim$date,2)) + 
  theme(axis.text.x = element_text(angle=90))

# T-test on the mean number of Arrests and Guns from each alert
t.test(df_ss_lim$Guns, mu = 0)
t.test(df_ss_lim$Arrests, mu = 0)

# 911 Gunshot Calls -------------------------------------------------------
# 
# Filter for SS calls and drop any duplicated rows
df_calls = df %>% filter(df$`Call Type` %in% c('SHOTS FIRED IP', 'SHOTS J/H')) %>%
  distinct(.keep_all = TRUE)
dim(df_ss)
df_calls[c('date', 'time')] = str_split_fixed(df_calls$Calltime, ' ', 2)
df_calls$date = as.Date(df_calls$date, format = '%m/%d/%Y')
df_calls[duplicated(df_calls),,]
# 
# Filter out entries bef our treatment date
df_calls_lim = df_calls %>% filter(df_calls$date <= END_DATE)
dim(df_calls_lim)

# Summary of arrests & guns
print(paste0('Total Number of Arrests: ', sum(df_calls$Arrests)))
print(paste0('Total Number of Guns Found: ', sum(df_calls$Guns)))

print(paste0('Number of Calls that resulted in an arrest: ',
             dim(df_calls[df_calls$Arrests>0,,])[1]))
df_calls[df_calls$Arrests>0,,]

print(paste0('Number of Calls that resulted in guns found: ',
             dim(df_calls[df_calls$Guns>0,,])[1]))
df_calls[df_calls$Guns>0,,]
# 
# ggplot(df_calls_lim, aes(date)) + geom_histogram(breaks = by_month(df_calls_lim$date)) +
#   scale_x_date(labels = scales::date_format("%Y-%b"),
#                breaks = by_month(df_calls_lim$date,2)) + 
#   theme(axis.text.x = element_text(angle=90))
# 

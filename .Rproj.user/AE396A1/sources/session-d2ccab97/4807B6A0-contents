library(tidyverse)
library(viridis) # just for the colors 
library(eurostat)
library(sf)
library('wcde')


# 1. data

demo_r_pjangroup <-  get_eurostat('demo_r_pjangroup')

demo_r_pjangroup <- 
  demo_r_pjangroup %>% 
  dplyr::rename('time' = 'TIME_PERIOD')


demo_r_pjangroup_ageGroups <- 
  demo_r_pjangroup %>%
  filter(age %in% c(
    "TOTAL","UNK","Y10-14","Y5-9","Y_LT5",
    "Y15-19","Y20-24",
    "Y25-29","Y30-34",
    "Y35-39","Y40-44"
    ,"Y45-49","Y50-54"
    ,"Y55-59","Y60-64"
    ,"Y65-69","Y70-74"
    ,"Y75-79","Y80-84"
    ,"Y_GE75")) %>% 
  spread(age, values) %>% 
  mutate(
    'under15' = `Y10-14`+`Y5-9`+`Y_LT5`,
    '15_24' = `Y15-19`+`Y20-24`,
    '25_44' = `Y25-29`+`Y30-34`+`Y35-39`+`Y40-44`,
    '45_64' = `Y45-49`+`Y50-54`+`Y55-59`+`Y60-64`,
    '65+' = `Y65-69`+`Y70-74`+`Y_GE75`,
  ) %>% 
  dplyr::select('sex', 'geo', 'time', 'TOTAL', 'UNK', 'under15', '15_24', '25_44', '45_64', '65+') 


# and now we fix the year groups 
time_intervals <- tibble(time = c(
  "1990-01-01","1991-01-01","1992-01-01","1993-01-01","1994-01-01",
  "1995-01-01","1996-01-01","1997-01-01","1998-01-01","1999-01-01",
  "2000-01-01","2001-01-01","2002-01-01","2003-01-01","2004-01-01",
  "2005-01-01","2006-01-01","2007-01-01","2008-01-01","2009-01-01",
  "2010-01-01","2011-01-01","2012-01-01","2013-01-01","2014-01-01",
  "2015-01-01","2016-01-01","2017-01-01","2018-01-01","2019-01-01",
  "2020-01-01","2021-01-01","2022-01-01","2023-01-01","2024-01-01"
),
interval = c(rep(c('1990-1994','1995-1999','2000-2004', '2005-2009', '2010-2014', '2015-2019'), each = 5),
             rep('2020-2024', 5)))




demo_r_pjangroup_ageGroups_timeIntervals_sex <- 
  demo_r_pjangroup_ageGroups %>% 
  mutate(time = as.character(time)) %>% 
  inner_join(time_intervals, by = 'time') %>% 
  gather(age.group, values, -c(sex,geo,time,interval)) %>% 
  group_by(sex,geo,interval,age.group) %>% 
  summarise(values = mean(values, na.rm=T)) %>% 
  ungroup()




demo_r_pjangroup_ageGroups_timeIntervals_sex %>% 
  pivot_wider(names_from = sex, values_from = values) %>% 
  dplyr::mutate(FtoM_ratio = `F`/`M`) %>% 
  dplyr::mutate(ctry = str_sub(geo, 1, 2)) %>%
  #dplyr::filter(age.group == 'TOTAL') %>% 
  ggplot(aes(x = interval, y = log(FtoM_ratio), group = interaction(geo))) +
  geom_line(aes(color = ifelse(str_length(geo) == 2, "Country", "Other"))) +
  facet_grid(age.group~ctry) +
  scale_color_manual(values = c("Country" = "red", "Other" = "black")) +
  theme_minimal() +
  labs(color = "Region Type")








































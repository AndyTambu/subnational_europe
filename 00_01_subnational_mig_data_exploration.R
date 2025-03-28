library(tidyverse)
library(eurostat)



# 
# Population by educational attainment level, sex, age, country of birth and NUTS 2 region (%)
# Online data code:
#   edat_lfs_9917

# Population by sex, age, country of birth, labour status and NUTS 2 region
lfst_r_lfsd2pwc <- eurostat::get_eurostat('lfst_r_lfsd2pwc')


# this is to see which countries delivered in whgich years
lfst_r_lfsd2pwc %>% 
  dplyr::filter(str_length(geo) == 2) %>% 
  dplyr::select( geo,TIME_PERIOD ) %>% 
  distinct_all() %>% 
  pivot_wider(names_from = 'TIME_PERIOD', values_from = 'TIME_PERIOD') 


lfst_r_lfsd2pwc %>% 
  distinct(age)
# it is not per se somethign which we can ignore the fact that the population under age 15 is not reported here. 
# Maybe we can have a look at the census data to see if we can get more information. 


# [EU27_2020_FOR]  EU27 countries (from 2020) except reporting country
# [NEU27_2020_FOR] Non-EU27 countries (from 2020) nor reporting country
# [FOR]            Foreign country
# [NAT]            Reporting country
# [TOTAL]          Total
# [NRP]            No response

# wstatus
# [POP] Population
# [ACT] Persons in the labour force
# [EMP] Employed persons
# [UNE] Unemployed persons
# [INAC] Persons outside the labour force
# [UNK] Unknown

# 
# lfst_r_lfsd2pwc %>% 
#   dplyr::select(-c(freq,unit)) %>% 
#   dplyr::filter(str_length(geo) == 4) %>% 
#   dplyr::filter(wstatus == "POP") %>% 
#   dplyr::filter(age == 'Y_GE15') %>% 
#   pivot_wider(names_from = "c_birth", values_from = 'values') %>% 
#   dplyr::mutate(check = TOTAL-(EU27_2020_FOR+NAT))
# 
# 
# [1] "wstatus"        "sex"            "age"            "geo"           
# [5] "TIME_PERIOD"    "EU27_2020_FOR"  "FOR"            "NAT"           
# [9] "NEU27_2020_FOR" "NRP"            "TOTAL" 



lfst_r_lfsd2pwc %>% 
  dplyr::select(-c(freq,unit)) %>% 
  dplyr::filter(str_length(geo) == 4) %>% 
  dplyr::mutate(ctry = str_sub(geo,1,2)) %>% distinct(ctry) %>% pull()
  dplyr::filter(wstatus == "POP") %>% 
  dplyr::filter(age == 'Y_GE15') %>% 
  pivot_wider(names_from = "c_birth", values_from = 'values') %>% 
  dplyr::mutate(ratio = FOR/TOTAL*100) %>% 
  dplyr::select("sex","geo","ctry","TIME_PERIOD","ratio"  ) %>% 
  pivot_wider(names_from = "sex", values_from = 'ratio') %>% 
  ggplot(aes(x = TIME_PERIOD, y = T))+
  geom_line(aes(group = interaction(geo)))+
  facet_wrap(~ctry)
  


  nuts2_capitals <- c(
    "AT13", # Vienna, Austria
    "BE10", # Brussels, Belgium
    "CH01", # Bern, Switzerland
    "CY00", # Nicosia, Cyprus
    "CZ01", # Prague, Czech Republic
    "DE30", # Berlin, Germany
    "DK01", # Copenhagen, Denmark
    "EE00", # Tallinn, Estonia
    "EL30", # Athens, Greece
    "ES30", # Madrid, Spain
    "FI1B", # Helsinki, Finland
    "FR10", # Paris, France
    "HR00", # Zagreb, Croatia
    "HU11", # Budapest, Hungary
    "IE06", # Dublin, Ireland
    "IS00", # Reykjavik, Iceland
    "ITI4", # Rome, Italy
    "LT00", # Vilnius, Lithuania
    "LU00", # Luxembourg, Luxembourg
    "LV00", # Riga, Latvia
    "ME00", # Podgorica, Montenegro
    "MK00", # Skopje, North Macedonia
    "MT00", # Valletta, Malta
    "NL33", # Amsterdam, Netherlands
    "NO08", # Oslo, Norway
    "PL91", # Warsaw, Poland
    "PT17", # Lisbon, Portugal
    "RO32", # Bucharest, Romania
    "RS00", # Belgrade, Serbia
    "SE11", # Stockholm, Sweden
    "SI04", # Ljubljana, Slovenia
    "SK01", # Bratislava, Slovakia
    "TR51", # Ankara, Turkey
    "UKI3", # London, United Kingdom
    "BG41"  # Sofia, Bulgaria
  )
  
  nuts2_capitals <- c(
    "AT13", "BE10", "CH01", "CY00", "CZ01", "DE30", "DK01", "EE00",
    "EL30", "ES30", "FI1B", "FR10", "HR00", "HU11", "IE06", "IS00",
    "ITI4", "LT00", "LU00", "LV00", "ME00", "MK00", "MT00", "NL33",
    "NO08", "PL91", "PT17", "RO32", "RS00", "SE11", "SI04", "SK01",
    "TR51", "UKI3", "BG41"
  )
  
  lfst_r_lfsd2pwc %>%
    dplyr::select(-c(freq, unit)) %>%
    dplyr::filter(str_length(geo) == 4) %>%
    dplyr::mutate(ctry = str_sub(geo, 1, 2)) %>%
    dplyr::filter(wstatus == "POP") %>%
    dplyr::filter(age == 'Y_GE15') %>%
    pivot_wider(names_from = "c_birth", values_from = 'values') %>%
    dplyr::mutate(ratio = FOR / TOTAL * 100) %>%
    dplyr::select("sex", "geo", "ctry", "TIME_PERIOD", "ratio") %>%
    pivot_wider(names_from = "sex", values_from = 'ratio') %>%
    ggplot(aes(x = TIME_PERIOD, y = T, group = interaction(geo))) +
    geom_line(aes(color = ifelse(geo %in% nuts2_capitals, "Capital", "Other")), linewidth = 1) +
    facet_wrap(~ctry) +
    scale_color_manual(values = c("Capital" = "red", "Other" = "black")) +
    theme_minimal() +
    labs(color = "Region Type")




















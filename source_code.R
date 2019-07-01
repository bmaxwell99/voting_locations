library(dplyr)
library(magrittr)
library(tidyr)
library(stringr)
library(ggplot2)

#sets WD to git repo folder, change to run on different computer
setwd("C:/Users/dark_/Documents/NDRN/voting_locations")

raw_data_18 <- 
#raw_data_16 <- read.csv('EAVS 2016 Final Data for Public Release v.4.csv', stringsAsFactors = FALSE)

df <- raw_data_18

#Connecticut collects by town, Louisiana collects by Parish, Main collects by something that has 500+, 
#Massachusetts collects by town, New Hampshite collects by town, Rhode Island by town, VT is towns? Wiscononsin is,
# towns concatenated with county, 
df_18 <- 
  read.csv('EAVS_2018_for_Public_Release_nolabel.CSV', stringsAsFactors = FALSE) %>% 
  select(FIPSCode, Jurisdiction_Name, State_Full, State_Abbr,D1a, D2a, D2b, D4a, D5a,) %>% 
  mutate(FIPSCode = trimws(as.character(FIPSCode))) %>% 
  mutate(FIPS_STATE = str_sub(FIPSCode, start = 1, end = 2)) %>% 
  mutate(FIPS_COUNTY = str_sub(FIPSCode, start = 3, end = 5)) %>% 
  mutate(D1a_NA = if_else(D1a == -99 | D1a == -88 | is.na(D1a), 1, 0)) %>% 
  mutate(D2a_NA = if_else(D2a == -99 | D2a == -88 | is.na(D2a), 1, 0)) %>% 
  mutate(D2b_NA = if_else(D2b == -99 | D2b == -88 | is.na(D2b), 1, 0)) %>% 
  mutate(D4a_NA = if_else(D4a == -99 | D4a == -88 | is.na(D4a), 1, 0)) %>% 
  mutate(D5a_NA = if_else(D5a == -99 | D5a == -88 | is.na(D5a), 1, 0)) %>% 
  mutate(D1a = replace(D1a, D1a < 0 | is.na(D1a), 0)) %>% 
  mutate(D2a = replace(D2a, D2a < 0 | is.na(D2a), 0)) %>% 
  mutate(D2b = replace(D2b, D2b < 0 | is.na(D2b), 0)) %>% 
  mutate(D4a = replace(D4a, D4a < 0 | is.na(D4a), 0)) %>% 
  mutate(D5a = replace(D5a, D5a < 0 | is.na(D5a), 0))
  

summary_18 <-
  df_18 %>% 
  group_by(State_Full) %>% 
  summarise(D1A_18 = sum(D1a),
            D2A_18 = sum(D2a),
            D2B_18 = sum(D2b),
            D4A_18 = sum(D4a),
            D5A_18 = sum(D5a),
            D1A_ratio = sum(D1a_NA) / n(),
            D
            
  )



df_18 %>% 
  filter(State_Abbr == 'AR') %>% 
  group_by(State_Full) %>% 
  summarise(

n
?summarise()


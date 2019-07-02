library(dplyr)
library(magrittr)
library(tidyr)
library(stringr)
library(ggplot2)

#sets WD to git repo folder, change to run on different computer
setwd("C:/Users/dark_/Documents/NDRN/voting_locations")


#Connecticut collects by town, Louisiana collects by Parish, Main collects by something that has 500+, 
#Massachusetts collects by town, New Hampshite collects by town, Rhode Island by town, VT is towns? Wiscononsin is,
# towns concatenated with county, 
df_18 <- 
  read.csv('EAVS_2018_for_Public_Release_nolabel.CSV', stringsAsFactors = FALSE) %>%  
  #select(State_Full, D4a, D5a) %>% 
  #mutate(FIPSCode = trimws(as.character(FIPSCode))) %>% 
  #mutate(FIPS_STATE = str_sub(FIPSCode, start = 1, end = 2)) %>% 
  #mutate(FIPS_COUNTY = str_sub(FIPSCode, start = 3, end = 5)) %>% 
  #mutate(total_votes_cast = if_else(D1a == -99 | D1a == -88 | is.na(D1a), 1, 0)) %>% 
  #mutate(votes_election_day = if_else(D2a == -99 | D2a == -88 | is.na(D2a), 1, 0)) %>% 
  #mutate(votes_early = if_else(D2b == -99 | D2b == -88 | is.na(D2b), 1, 0)) %>% 
  mutate(locations_election_day_na = if_else(D4a == -99 | D4a == -88 | is.na(D4a), 1, 0)) %>% 
  mutate(locations_early_na = if_else(D5a == -99 | D5a == -88 | is.na(D5a), 1, 0)) %>% 
  #mutate(total_votes_cast = replace(D1a, D1a < 0 | is.na(D1a), 0)) %>% 
  #mutate(votes_election_day = replace(D2a, D2a < 0 | is.na(D2a), 0)) %>% 
  #mutate(votes_early = replace(D2b, D2b < 0 | is.na(D2b), 0)) %>% 
  mutate(locations_election_day = replace(D4a, D4a < 0 | is.na(D4a), 0)) %>% 
  mutate(locations_early = replace(D5a, D5a < 0 | is.na(D5a), 0)) %>% 
  select(Jurisdiction_Name, State_Full, State_Abbr, locations_election_day, locations_early, 
         locations_election_day_na, locations_early_na) 

df_18 %>% filter(State_Full == 'CALIFORNIA')
#summarizes the data by State
summary_18 <-
  df_18 %>% 
  group_by(State_Full) %>% 
  summarise(#total_votes_cast_18 = sum(total_votes_cast),
            #votes_election_day_18 = sum(votes_election_day),
            #votes_early_18 = sum(votes),
            locations_election_day_18 = sum(locations_election_day),
            locations_early_18 = sum(locations_early),
            #r_total_votes_cast_18 = round(sum(D1a_NA) / n(), 4),
            #r_votes_election_day_18 = round(sum(D2a_NA) / n(), 4),
            #r_votes_early_18 = round(sum(D2b_NA) / n(), 4),
            r_locations_election_day_18 = round(sum(locations_election_day_na) / n(), 4),
            r_locations_early_18 = round(sum(locations_early_na) / n(), 4),
            n_area_18 = n()
            
  )

df_16 <- 
  read.csv('2016_eavs_section_D.csv', stringsAsFactors = FALSE) %>% 
  select( ï..FIPSCode, JurisdictionName, State ,D2a, D2b, D2c, D2d, D2e, D2f, D2g) %>% 
  mutate(D2a = as.numeric(D2a)) %>% 
  mutate(D2b = as.numeric(D2b)) %>% 
  mutate(D2c = as.numeric(D2c)) %>% 
  mutate(D2d = as.numeric(D2d)) %>% 
  mutate(D2e = as.numeric(D2e)) %>% 
  mutate(D2f = as.numeric(D2f)) %>% 
  mutate(D2g = as.numeric(D2g)) %>% 
  group_by(State) %>% 
  summarise(
    D2a = sum(D2a, na.rm = TRUE),
    D2b = sum(D2b, na.rm = TRUE),
    D2c = sum(D2c, na.rm = TRUE),
    D2d = sum(D2d, na.rm = TRUE),
    D2e = sum(D2e, na.rm = TRUE),
    D2f = sum(D2f, na.rm = TRUE),
    D2g = sum(D2g, na.rm = TRUE)
  ) %>% 
  mutate(diff = abs(D2a - D2b - D2c - D2d - D2e - D2f - D2g))

df_14 <- 
  read.csv('2014_EAVS_Section_D.csv', stringsAsFactors = FALSE) %>% 
  select(State, Jurisdiction, QD2a, QD2b, QD2c, QD2d, QD2e, QD2f, QD2g, QD2_Total )


colnames(df_14)

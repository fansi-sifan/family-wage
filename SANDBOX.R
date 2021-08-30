acs2019ipums <- data.table::fread("data-raw/usa_00019.csv")
# extracts from ipums
# group quarter = 1,2,5
# metro = T

# CLEAN ==========
library(tidyverse)
library(tidylog)

# which metros were not included?
cbsa_match <- acs2019ipums %>% 
  distinct(MET2013, MET2013ERR) %>% 
  mutate(cbsa_code = as.character(MET2013)) %>% 
  full_join(metro.data::cbsa_18, by = "cbsa_code")

cbsa_match %>% 
  filter(str_detect(cbsa_size, "large")) %>% 
  view()

# ACS editing -----
df <- acs2019ipums %>% 
  mutate(family = ifelse(FAMUNIT == 1, SUBFAM, FAMUNIT + 100), 
         adult = AGE >= 18, 
         senior = AGE >= 65) %>% 
  
  mutate(sex_cat = ifelse(SEX == 1, "Male", "Female")) %>% 
  
  mutate(race_cat = case_when(
    RACE == 1 & HISPAN == 0 ~ "white",
    RACE == 2 & HISPAN == 0 ~ "Black",
    RACE %in% c(4,5,6) & HISPAN == 0 ~ "Asian",
    RACE %in% c(3,7,8,9) & HISPAN == 0 ~ "Other",
    HISPAN != 0 ~ "Hispanic"
  )) %>% 
  
  mutate(edu_cat = case_when(
    EDUCD %in% c(63, 64) ~ "High shool",
    EDUCD %in% c(65, 71) ~ "Some college",
    EDUCD == 81 ~ "Associate", 
    EDUCD == 101 ~ "Bachelor", 
    EDUCD %in% c(114, 115, 116) ~ "Postgraduate", 
    T ~ "Below high school"
  )) %>% 
  
  mutate(across(INCTOT:INCOTHER, 
                ~ifelse(.x %in% c(99999999, 9999999, 999999, 999998, 99999), 0, .x))) 

# identify family head
acs_fam <- df %>% 
  mutate(id = str_c(SERIAL, PERNUM)) %>% 
  group_by(SERIAL, family) %>% 
  mutate(head = case_when(
    RELATE == 1 ~ T, 
    RELATE !=1 & INCTOT == max(INCTOT) ~ T, 
    T ~ F))

acs_fam %>% 
  ungroup() %>% 
  filter(head) %>% 
  count(adult, senior, LABFORCE, DIFFMOB, DIFFCARE, wt = PERWT) %>% 
  mutate(pct = n/sum(n)) %>% 
  view()

acs_fam %>% 
  filter(SERIAL == 2584) %>% view()

acs_fam %>% 
  ungroup() %>% 
  filter(head) %>% 
  filter(!adult) %>% 
  view()

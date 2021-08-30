library(tidyverse)
library(tidylog)
# Load raw PUMS files ---------

# household
h_data <- data.table::fread("../../_metro_data_warehouse/data_raw/2019 ACS/2019 ACS 1-year/csv_hus/psam_husa.csv")
hb_data <- data.table::fread("../../_metro_data_warehouse/data_raw/2019 ACS/2019 ACS 1-year/csv_hus/psam_husb.csv")

h <- bind_rows(h_data, hb_data)

save(h, file = "../../_metro_data_warehouse/data_raw/2019 ACS/2019 ACS 1-year/psam_hus.rda")

# person
p_data <- data.table::fread("../../_metro_data_warehouse/data_raw/2019 ACS/2019 ACS 1-year/csv_pus/psam_pusa.csv")
pb_data <- data.table::fread("../../_metro_data_warehouse/data_raw/2019 ACS/2019 ACS 1-year/csv_pus/psam_pusb.csv")

p <- rbind(p_data, pb_data)

# save(p, file = "../../_metro_data_warehouse/data_raw/2019 ACS/2019 ACS 1-year/psam_pus.rda")
# 
# 
# load("../../_metro_data_warehouse/data_raw/2018 ACS/2018 ACS 1-year/psam_hus.rda")
# load("../../_metro_data_warehouse/data_raw/2018 ACS/2018 ACS 1-year/psam_pus.rda")

# data dictionary
dic <- read_csv("https://www2.census.gov/programs-surveys/acs/tech_docs/pums/data_dict/PUMS_Data_Dictionary_2015-2019.csv?")

# merge person and households
data_st <- left_join(
  p %>%
    # filter(ST == as.numeric(state)) %>%
    select(SERIALNO, AGEP, RELP = RELSHIPP, ST, PUMA, PWGTP, ESR, PINCP, SEMP, WAGP, INTP, RETP, SSP, OIP, SSIP, PAP, COW, NWLK, NWAV, contains("HINS"), 
           RAC1P, HISP, SCH, SCHL, POVPIP, SEX, SFN,SPORDER, DDRS, DOUT, SCHG, MSP,MIL, PAOC, RC, OC, WKHP, NAICSP), 
  h %>%
    # filter(ST == as.numeric(state)) %>%
    select(SERIALNO, WGTP, TYPE, FINCP, HINCP, NP, WORKSTAT, HUPAOC, SRNT, SVAL,GRNTP, SMOCP, GRPIP, OCPIP, PARTNER), 
  by = "SERIALNO")

saveRDS(data_st, file = "data/acs2019puma.rds")

# ACS microdata editing procedure ------
df <- data_st %>% 
  mutate(GQ = str_detect(SERIALNO, "GQ")) %>% 
  filter(!GQ) %>% 
  mutate(SFN = ifelse(is.na(SFN),0,SFN)) %>%
  
  # demographics
  mutate(sex_cat = ifelse(SEX == 1, "male", "female")) %>%
  
  mutate(raced_cat = ifelse(HISP == 1, RAC1P, "Hispanic")) %>%
  mutate(race_cat = case_when(
    raced_cat == 1 ~ "white",
    raced_cat == 2 ~ "Black",
    raced_cat == 6 ~ "Asian",
    raced_cat %in% c(3,4,5,7,8,9) ~ "Other",
    raced_cat == "Hispanic" ~ "Hispanic"
  )) %>%
  mutate(minority_cat = case_when(
    race_cat == "Black" | race_cat == "Hispanic" ~ "Black or Hispanic", 
    T ~ "White, Asian, and all others"
  ))%>%
  
  mutate(edu_cat = case_when(
    SCHL < 16 ~ "no high school",
    SCHL == 16 | SCHL == 17 ~ "High school",
    SCHL == 18 | SCHL == 19 ~ "Some college",
    SCHL == 20 ~ "Associate",
    SCHL == 21 ~ "Bachelor",
    SCHL >= 22 ~ "Graduate degree"
  )) %>%
  
  # age
  mutate(age_cat = case_when(
    AGEP < 18 ~ "below 18", 
    between(AGEP, 18, 24) ~ "18 - 24", 
    between(AGEP, 25, 34) ~ "25 - 34",
    between(AGEP, 35, 44) ~ "35 - 44", 
    between(AGEP, 45, 54) ~ "45 - 54",
    between(AGEP, 55, 64) ~ "55 - 64", 
    AGEP > 64 ~ "above 64"
  )) %>%
  
  mutate(pop_cat = case_when(
    between(AGEP, 18, 24) & SCH %in% c(2,3) & RELP %in% c(2,4,7) & SFN == 0 ~ "child", # college kids 
    AGEP < 18 ~ "child",
    AGEP >=18 ~ "adult"
    # AGEP >= 65 ~ "senior",
    # between(AGEP, 18, 64) ~ "adult"
  )) %>%
  
  # unmarried couple
  mutate(partnered = (MSP == 1 | RELP == 13)) %>%
  
  # detect roommates  
  mutate(related = RELP %in% c(0:10,13,14)) %>% 
  
  # independent
  mutate(indep = case_when(
    # adult children not in school, living with parents are treated as separate subfamily
    RELP %in% c(2:4) & age_cat != "below 18" & SCH == 1 ~ 1,
    # non-related adult 
    !related & age_cat != "below 18" ~ 1,
    
    # exception: with own kids in family, kids, disability, have partner, hh head with kids
    pop_cat == "child" ~ 0,
    
    DDRS == 1 ~ 0,
    DOUT == 1 ~ 0,
    partnered ~ 0,
    
    # no income
    PINCP <= 0 ~ 0,
    
    # # sole income provider of the family
    # PINCP == HINCP ~ 0,
    # PINCP == FINCP ~ 0,
    
    # kids in HH
    RELP == 0 & PAOC %in% c(1:3) ~ 0,
    
    # all others
    T ~ 1
  )) %>% 
  
  # labor force
  # https://usa.ipums.org/usa-action/variables/EMPSTAT#editing_procedure_section
  mutate(emp_cat = case_when(
    ESR == 1 | ESR == 2 | ESR == 4 | ESR == 5 ~ "Employed",
    ESR == 3 ~ "Unemployed",
    ESR == 6 ~ "Not in labor force"
  )) %>%
  
select(SERIALNO, PUMA, ST, PWGTP, contains("_cat"), WAGP, SEMP, PINCP, everything())

# Create family types ===============================
create_family <- function(data){
  
  start.time <- Sys.time()
  df <- data %>% 
    
    group_by(SERIALNO,SFN,indep) %>% 
    count(pop_cat) %>%
    mutate(type = paste0(n," ",pop_cat)) %>% 
    
    group_by(SERIALNO,SFN,indep) %>% 
    arrange(pop_cat) %>% 
    summarise(cat = paste(type, collapse = ",")) %>% 
    
    # split family type by 1) have adult presence and b) independent living
    group_by(str_detect(cat,"adult"), indep) %>% 
    group_split()
  
  end.time <- Sys.time()
  cat(end.time - start.time)
  
  return(df)
  
}

fam <- create_family(df)

# Step 1: for subfamiles with only kids, match the kid to the adults in the subfamily, and change indep = 0
df <- fam[[3]] %>% 
  inner_join(fam[[1]], by = c("SERIALNO","SFN")) %>% 
  filter(str_detect(cat.x,"adult")) %>% 
  mutate(child_adult = 1) %>% 
  select(SERIALNO, SFN, child_adult) %>% 
  right_join(df, by = c("SERIALNO","SFN")) %>% 
  mutate(indep = ifelse(is.na(child_adult),indep,0))

fam <- create_family(df)

# Step 2: take independent person out of families, treat them as new subfamily
df <- fam[[3]] %>% 
  # mutate(loner = 1) %>% 
  select(SERIALNO, SFN) %>%
  left_join(df %>% filter(indep == 1), by = c("SERIALNO","SFN")) %>% 
  group_by(SERIALNO, SFN) %>% 
  mutate(SFN = 100 + row_number()) %>% 
  bind_rows(df %>% filter(indep == 0))

fam <- create_family(df)

# check family types
bind_rows(fam) %>% 
  count(cat, sort = T) %>%
  # filter(str_detect(cat,"adult")) %>% 
  mutate(pct = n/sum(n),
         cum = cumsum(pct)) 

# identify family head ===============
# family head characteristics ----------
fam_head <- df %>% 
  group_by(SERIALNO, SFN) %>% 
  mutate(
    R = prod(RELP),
    head = case_when(
      RELP == 0 ~ 1,
      
      # for subfamilies, assume hh is the one makes the highest income
      R!= 0 & PINCP == max(PINCP, na.rm = T) ~ 1,
      T ~ 0
    ))  %>% 
  filter(head == 1) %>% 
  select(SERIALNO, SFN, head, related)

# Calculate family income =====
fam_inc <- df %>% 
  # slice_head(5000) %>% 
  group_by(SERIALNO, SFN) %>% 
  summarise_at(vars(PINCP, INTP, RETP, WAGP, SEMP, SSP, SSIP, OIP, PAP), ~sum(., na.rm = T)) %>% 
  select(SERIALNO, SFN, contains("P")) 

# summary(fam_inc$PINCP)
# quantile(fam_inc$PINCP,c(0.01, 0.05,0.1,0.2,0.3))
# hist(fam_inc$PINCP,xlim = c(0,18700), breaks = 10000)


# STEP 3: if subfamily gross income is unusually low, and if subfamily HH is related to HH, assume it's depending on HH
p <- quantile(fam_inc$PINCP,0.25)

df <- fam_inc %>% 
  left_join(fam_head, by = c("SERIALNO", "SFN")) %>% 
  mutate(SFN_adj = ifelse(PINCP < p & related == 1, 0, SFN)) %>% 
  right_join(df, by = c("SERIALNO", "SFN"))

fam <- df %>%
  
  group_by(SERIALNO,SFN_adj) %>%
  count(pop_cat) %>%
  mutate(type = paste0(n," ",pop_cat)) %>%
  
  group_by(SERIALNO,SFN_adj) %>%
  arrange(pop_cat) %>%
  summarise(cat = paste(type, collapse = ","))

fam %>% 
  ungroup() %>% 
  count(cat, sort = T) %>% 
  mutate(pct = n/sum(n),
         cum = cumsum(pct)) 

# re-identify HH
final <- df %>% 
  group_by(SERIALNO, SFN_adj) %>% 
  mutate(
    R = prod(RELP),
    head = case_when(
      RELP == 0 ~ 1,
      
      # for subfamilies, hh makes the highest income
      R!= 0 & PINCP.y == max(PINCP.y, na.rm = T) ~ 1,
      T ~ 0
    ))  %>% 
  # filter(head == 1) %>% 
  left_join(fam, by = c("SERIALNO", "SFN_adj")) %>% 
  mutate(stpuma_code = paste0(str_pad(ST,2,"left","0"), str_pad(PUMA, 5,"left","0")))


# save ====

saveRDS(final, file = "data/acs2019final.rds")


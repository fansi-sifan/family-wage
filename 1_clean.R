library(tidyverse)
library(tidylog)
# devtools::install_github("BrookingsInstitution/metro-data-warehouse")
# get all metros code
all_cbsa <- metro.data::cbsa_18 %>%
  filter(cbsa_size != "micro") %>%
  select(cbsa_code) %>%
  pull()

# match to livnig wage data ---------
load("data-raw//county_fbc_data_2018.rda")

cat_fam <- tibble::tribble(
  ~family, ~cat,
  "1p0c", "1 adult",
  "2p0c", "2 adult",
  "1p1c", "1 adult,1 child",
  "2p2c", "2 adult,2 child",
  "2p1c", "2 adult,1 child",
  "1p2c", "1 adult,2 child",
  "1p3c", "1 adult,3 child",
  "1p4c", "1 adult,4 child",
  "2p3c", "2 adult,3 child",
  "2p4c", "2 adult,4 child"
)

# PUMA ------------
# GEOCORR 2018, weighted by 2010 household units
final <- readRDS(file = "data-raw//acs2019final.rds")

puma_county <- metro.data::puma2county %>%
  mutate(stpuma_code = paste0(st_code, puma_code)) %>% 
  select(stpuma_code, stco_code, afact1)

target_county <- puma_county %>% 
  left_join(final, by = "stpuma_code")

# match to living wage data =================
fbc_xwalk_county <- county_fbc_data_2018 %>% 
  unique() %>% 
  left_join(cat_fam) %>% 
  mutate_at(vars(contains("a_")), as.numeric) %>% 
  select(stco_code, family, cat, contains("a_"))

target_struggle <- target_county %>%
  # ACS household weight * PUMA2county crosswalk weight
  mutate(weight = PWGTP * afact1) %>% 
  
  # include only household heads who are working-age adults currently in labor force
  filter(head == 1) %>%
  filter(age_cat != "above 64", age_cat != "below 18") %>%
  filter(emp_cat %in% c("Employed", "Unemployed")) %>%
  
  # match to family budget data
  left_join(fbc_xwalk_county, by = c("stco_code", "cat")) %>%
  
  mutate(a_total = (a_total - a_taxes), 
         income = PINCP.x,
         struggle_alt = income < a_total) %>%

  left_join(metro.data::county_cbsa_st[c("stco_code", "cbsa_code", "cbsa_name", "cbsa_size")], by = "stco_code")

save(target_struggle, file = "result/stco_struggle19.rda")


# determine expected wage based on family types
load("result/stco_struggle19.rda")
get_wage <- function(df){
  df%>%
    group_by(cbsa_code) %>%
    arrange(expected_wage) %>%
    mutate(
      kids = as.numeric(str_sub(cat, 9, 9)),
      kids = ifelse(is.na(kids), 0, kids)
    ) %>%
    # mutate(black_kids = ifelse(race_cat  == "Black", kids, 0),
    #        latino_kids = ifelse(race_cat  == "Hispanic", kids, 0),
    #        minority_kids = ifelse(race_cat %in% c("Black", "Hispanic"), kids, 0)) %>%
    mutate(
      # black_kids_s = weight * black_kids / sum(weight * black_kids),
      # pct_black = cumsum(black_kids_s),
      # 
      # latino_kids_s = weight * latino_kids / sum(weight * latino_kids),
      # pct_latino = cumsum(latino_kids_s),
      # 
      # minority_kids_s = weight * minority_kids / sum(weight * minority_kids),
      # pct_minority = cumsum(minority_kids_s),
      
      # all kids
      kid_s = weight * kids / sum(weight * kids),
      pct_kids = cumsum(kid_s),
      
      # families with kids
      kids_fam = (weight * (kids != 0))/sum(weight * (kids !=0)), 
      pct_kids_fam = cumsum(kids_fam),
      
      # all families
      fam = weight / sum(weight),
      pct_all_fam = cumsum(fam)
    ) 
}


target_wages_semp <- target_struggle %>%
  filter(struggle_alt) %>% 
  mutate(expected_wage = case_when(
    str_detect(cat, "1 adult") ~ (a_total - (income - WAGP.x)) / 2080,
    # 2 adult, one with no income - childcare
    str_detect(cat, "2 adult") & PINCP.x == PINCP.y ~ (a_total - a_childcare - (income - WAGP.x)) / 2080,
    # 2 adults both working
    T ~ (a_total - (income - WAGP.x)) / 2 / 2080
  )) %>%
  get_wage()

# check 
target_wages_semp %>%
  # group_by(cbsa_code)%>% 
  filter(cbsa_code == "13820") %>% 
  summarise(
    kids = spatstat::weighted.quantile(expected_wage, kids , probs = 0.4, na.rm = T),
    kids_fam = spatstat::weighted.quantile(expected_wage, kids_fam * weight, probs = 0.4, na.rm = T),
    fam = spatstat::weighted.quantile(expected_wage, fam * weight, probs = 0.4, na.rm = T)
  ) 


save(target_wages_semp, file = "result/target_wage.rda")






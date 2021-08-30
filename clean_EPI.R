# Data Cleaning EPI Family Budget Calculator
# By: Gabe Morrison, Intern Metropolitan Policy Program at the Brookings Institution
# June 25, 2020

# Loading Packages:
library(tidyverse)
library(readxl)
# library(metro.data)
library(data.table)

# Reading each of the files already in the folder:
crosswalk_metro_county <- read_excel("data/metro_county.xlsx")
crosswalk_county_fips_fbc_guide <- read_excel("data/county_fips_fbc.xlsx")
county_fbc_data_2018 <- read_excel("data/fbc_data_2018.xlsx", sheet = 2)
metro_fbc_data_2018 <- read_excel("data/fbc_data_2018.xlsx", sheet = 3)


# Renaming County df: naming got messed up due to data using 2 rows for names in Excel
county_fbc_data_2018 <- county_fbc_data_2018 %>%
  rename(
    case_id = ...1,
    st_abb = ...2,
    is_metro = ...3,
    cbsa_name = ...4,
    stco_name = ...5,
    family = ...6,
    m_housing = Monthly,
    m_food = ...8,
    m_transportation = ...9,
    m_healthcare = ...10,
    m_other_necessities = ...11,
    m_childcare = ...12,
    m_taxes = ...13,
    m_total = ...14,
    a_housing = Annual,
    a_food = ...16,
    a_transportation = ...17,
    a_healthcare = ...18,
    a_other_necessities = ...19,
    a_childcare = ...20,
    a_taxes = ...21,
    a_total = ...22,
    median_family_income = Rankings,
    num_counties_in_st = ...24,
    st_cost_rank = ...25,
    st_med_aff_rank = ...26,
    st_income_rank = ...27
  )

# To further assess conversion, need to delete first row:
county_fbc_data_2018 <- county_fbc_data_2018[-1, ]

# Converting characters to numeric for the numeric categories
county_fbc_data_2018[7:27] <- sapply(county_fbc_data_2018[7:27], as.numeric)



# Rename metro file (exact same process as counties)
metro_fbc_data_2018 <- metro_fbc_data_2018 %>%
  rename(
    case_id = ...1,
    st_abb = ...2,
    cbsa_name = ...3,
    family = ...4,
    m_housing = Monthly,
    m_food = ...6,
    m_transportation = ...7,
    m_healthcare = ...8,
    m_other_necessities = ...9,
    m_childcare = ...10,
    m_taxes = ...11,
    m_total = ...12,
    a_housing = Annual,
    a_food = ...14,
    a_transportation = ...15,
    a_healthcare = ...16,
    a_other_necessities = ...17,
    a_childcare = ...18,
    a_taxes = ...19,
    a_total = ...20,
    median_family_income = Rankings,
    is_top100 = ...22,
    top100_cost_rank = ...23,
    top100_med_aff_rank = ...24,
    top100_med_faminc_rank = ...25
  )

# Again deleting first row to fit to normal df
metro_fbc_data_2018 <- metro_fbc_data_2018[-1, ]

# Again converting Character to numeric:
metro_fbc_data_2018[6:25] <- sapply(metro_fbc_data_2018[6:25], as.numeric)


# Assessing Missing Observations or Values:

## Counties:

nrow(county_fbc_data_2018) # 31,710
county_na <- county_fbc_data_2018 %>% # command that tells the number of nas for each column in the data
  select(everything()) %>%
  summarise_all(funs(sum(is.na(.))))
# Finding: There are no na's

summary(county_fbc_data_2018) # Visually inspect Min and Max for each of columns to check for outliers
# Finding: I do not see any glaring issues or miscodes in the data


## Metro Areas:
nrow(metro_fbc_data_2018) # 6560
metro_na <- metro_fbc_data_2018 %>% # command that tells the number of nas for each column in the data
  select(everything()) %>%
  summarise_all(funs(sum(is.na(.))))

# There are only na's in the last three columns. Those columns are rankings for the top 100 metro areas.
# Rows for areas without top 100 are left blank and thus have nas.

summary(metro_fbc_data_2018)
# Again I don't detect issues here





# Adding FIPS Data

## County:
crosswalk_fips_county <- crosswalk_county_fips_fbc_guide %>% # creating new df to only join fips column
  select(st_abb = state_alpha, stco_code = fips2010, stco_name = county_name) 

county_fbc_data_2018 <- left_join(county_fbc_data_2018, 
                                  crosswalk_fips_county, by = c("stco_name","st_abb"))

sum(is.na(county_fbc_data_2018$fips2010)) # Check to see if join works right. the number of na's is 0, a good sign!

save(county_fbc_data_2018, file = "data/county_fbc_data_2018.rda")

## Similar process but with the metro data
metro_fbc_data_2018 <- metro_fbc_data_2018 %>% 
  left_join(crosswalk_metro_county %>% 
              mutate(cbsa_code = str_sub(metro_code, -5,-1)) %>% 
              mutate(cbsa_code = ifelse(str_detect(cbsa_code, "M"),str_sub(metro_code,6,10), cbsa_code)) %>% 
              select(cbsa_code, cbsa_name = metroname) %>% 
              distinct(), 
            by = "cbsa_name")

save(metro_fbc_data_2018, file = "data/metro_fbc_data_2018.rda")

# Note that this uses the metro crosswalk as opposed to the county crosswalk
# crosswalk_fips_metro2 <- crosswalk_metro_county %>%
#   select(fips2010, state_alpha, metroname) %>%
#   rename(area_name2 = metroname)
# crosswalk_fips_metro2$area_name2 <- sub(",.*", "", crosswalk_fips_metro2$area_name2) # Modifying strings to facilitate join
# 
# 
# metro_fbc_data_2018$area_name2 <- sub(",.*", "", metro_fbc_data_2018$area_name) # Modifying strings to facilitate join
# 
# metro_fbc_data_2018 <- left_join(metro_fbc_data_2018, crosswalk_fips_metro2, by = "area_name2") # joins two dfs together, thereby adding fips
# metro_fbc_data_2018 <- metro_fbc_data_2018 %>%
#   select(-state_alpha)
# 
# 
# sum(is.na(metro_fbc_data_2018$fips2010)) # Check to ensure that all fipscodes went across
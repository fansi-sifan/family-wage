library(tidyverse)
library(tidylog)

# SETUP ================
target_cbsa <- metro.data::cbsa_18 %>%
  filter(str_detect(cbsa_size, "large")) %>%
  # filter(cbsa_size != "micro") %>%
  pull(cbsa_code)

# target_cbsa <- str_sub(list.files("data/staffing"), 1, 5)

# LOAD =================
occ <- data.table::fread("../../_metro_data_warehouse/data_raw/Emsi/Stafiing_Patterns_MSA_NAICS3_SOC5_Sep282020.csv")
occ_ind <- occ %>%
  filter(MSAID %in% target_cbsa) %>%
  select(-IndustryName) %>%
  pivot_wider(names_from = "Industry", values_from = "2019 Jobs")

# read_staffing <- function(name) {
#   file <- paste0("data/staffing/", name, ".csv")
#   read_csv(file) %>%
#     mutate(cbsa_code = name)
# }
#
# occ <- target_cbsa %>%
#   map_dfr(read_staffing)
# occ_ind <- occ %>%
#   mutate(across(2:-1, as.numeric))

secure <- read_csv("result/cbsa_wages19.csv") %>%
  filter(cbsa_code %in% target_cbsa)

wage <- read_csv("../../_metro_data_warehouse/data_raw/Emsi/Raw/2020.9/Emsi_2020.3_occ_data.csv")
naics3 <- read_csv("../../_metro_data_warehouse/data_raw/Emsi/Raw/2020.9/Emsi_2020.3_ind_data.csv")

# education
occ_edu <- read_csv("../metro-self-sufficient/job_test/us_occ_edu.csv")

# clean =========
occ_wage <- wage %>%
  right_join(secure, by = c("Area" = "cbsa_code")) %>%
  # mutate(across(contains("Percentile"), ~ (.x > kids * 2080),
  #   .names = "good_{col}"
  # )) %>%
  # mutate(pct_good = case_when(
  #   `good_10th Percentile Earnings` ~ 0.9,
  #   `good_25th Percentile Earnings` ~ 0.75,
  #   `good_50th Percentile Earnings` ~ 0.5,
  #   `good_75th Percentile Earnings` ~ 0.25,
  #   `good_90th Percentile Earnings` ~ 0.1,
  #   T ~ 0
  # )) %>%
mutate(across(contains("Percentile"),
              list(good = ~ (.x > kids * 2080), num = ~ (.x - kids * 2080)),
              .names = "{fn}_{col}"
), ) %>%
  mutate(pct_good = case_when(
    `good_10th Percentile Earnings` ~ 0.9,
    `good_25th Percentile Earnings` ~ 0.75- (0.9 - 0.75) * (kids * 2080 - `25th Percentile Earnings`) / (`10th Percentile Earnings` - `25th Percentile Earnings`),
    `good_50th Percentile Earnings` ~ 0.5 - (0.75 - 0.5) * (kids * 2080 - `50th Percentile Earnings`) / (`25th Percentile Earnings` - `50th Percentile Earnings`),
    `good_75th Percentile Earnings` ~ 0.25- (0.5 - 0.25) * (kids * 2080 - `75th Percentile Earnings`) / (`50th Percentile Earnings` - `75th Percentile Earnings`),
    `good_90th Percentile Earnings` ~ 0.1 - (0.25 - 0.1) * (kids * 2080 - `90th Percentile Earnings`) / (`75th Percentile Earnings` - `90th Percentile Earnings`),
    T ~ 0
  )) %>%
  rename(SOC = Occupation) %>%
  left_join(occ_edu, by = "SOC") %>%
  mutate(
    high_skill = ifelse(`Typical Entry Level Education` %in% c(
      "Doctoral or professional degree",
      "Master's degree",
      "Bachelor's degree"
    ), 1, 0),
    mid_skill = ifelse(`Typical Entry Level Education` %in% c(
      "Some college, no degree",
      "Postsecondary nondegree award",
      "Associate's degree"
    ), 1, 0),
    low_skill = ifelse(`Typical Entry Level Education` %in% c(
      "High school diploma or equivalent", "N/A",
      "No formal educational credential"
    ), 1, 0)
  )


occ_ind_wage <- occ_ind %>%
  rename(SOC = Occupation) %>%
  mutate(cbsa_code = as.character(MSAID)) %>%
  # mutate(across("111":"999", as.numeric)) %>%
  left_join(occ_wage %>%
              mutate(cbsa_code = as.character(Area)),
            by = c("SOC", "cbsa_code")
  ) %>%
  group_by(cbsa_code) %>%
  summarise(
    across("111":"999", ~ sum(.x * pct_good, na.rm = T),
           .names = "{col}_n"
    ),
    across("111":"999", ~ sum(.x * pct_good, na.rm = T) / sum(.x, na.rm = T),
           .names = "{col}_pct"
    ),
    across("111":"999", ~ sum(.x * pct_good * high_skill, na.rm = T) / sum(.x, na.rm = T),
           .names = "{col}_highskill"
    ),
    across("111":"999", ~ sum(.x * pct_good * mid_skill, na.rm = T) / sum(.x, na.rm = T),
           .names = "{col}_midskill"
    ),
    across("111":"999", ~ sum(.x * pct_good * low_skill, na.rm = T) / sum(.x, na.rm = T),
           .names = "{col}_lowskill"
    )
  )

final <- as.tibble(cbind(nms = names(occ_ind_wage[, -1]), t(occ_ind_wage[, -1])))
names(final) <- c("nms", occ_ind_wage[, 1] %>% pull())

final <- final %>%
  mutate(across(-nms, as.numeric)) %>%
  pivot_longer(where(is.numeric), names_to = "cbsa_code", values_to = "value") %>%
  separate(nms, c("NAICS", "name"), sep = "_") %>%
  pivot_wider(names_from = "name", values_from = "value") %>%
  mutate(across(n:lowskill, as.numeric))

# match to industry classification (insurance, traded)
load("../metro-self-sufficient/job_test/naics_emp.rda")
load("../metro.data/data-raw/naics3_sc.rda")

# traded 3-digit
modify_naics3 <- function(df, col) {
  col <- enquo(col)
  name <- ensym(col)
  
  df %>%
    mutate(!!col := case_when(
      str_sub(!!col, 1, 2) == "23" ~ "23",
      str_sub(!!col, 1, 2) == "55" ~ "55",
      !!col %in% c("523", "521", "525") ~ "52M",
      !!col == "533" ~ "53M",
      T ~ !!col
    ))
}

naics3_cat <- naics_emp %>%  
  left_join(naics3_sc %>% 
              modify_naics3(naics_code) %>% 
              group_by(naics_code) %>% 
              slice(1) %>% 
              select(naics3_code = naics_code, sector)
  ) %>%
  mutate(sector = ifelse(naics3_code == "482", "B2C Traded Services", sector)) %>% 
  select(NAICS = naics3_code,
         sector,
         insurance_coverage = pct,
         contains("pct_"))


final_naics3 <- final %>%
  # group_by(naics_3) %>%
  # mutate(across(pct:lowskill, ~ifelse(.x == 0, NA,.x))) %>%
  # summarise(n_good = sum(n, na.rm = T),
  #           pct_good = sum(n, na.rm = T)/sum(n/pct, na.rm = T),
  #           high_skill = sum(n*highskill, na.rm = T)/sum(n, na.rm = T),
  #           mid_skill = sum(n*midskill, na.rm = T)/sum(n, na.rm = T),
  #           low_skill = sum(n*lowskill, na.rm = T)/sum(n, na.rm = T)) %>%
  left_join(naics3 %>%
              mutate(
                NAICS = as.character(Industry),
                cbsa_code = as.character(Area)
              ) %>%
              select(cbsa_code, NAICS, `Industry Name`, Jobs, `Earnings Per Worker`),
            by = c("NAICS", "cbsa_code")
  ) %>%
  filter(pct != "NaN") %>%
  arrange(-n) %>%
  select(cbsa_code, NAICS, `Industry Name`,
         n_good_jobs = n,
         pct_good_jobs = pct, contains("skill"), everything()
  ) %>%
  mutate(NAICS = as.character(NAICS)) %>%
  modify_naics3(NAICS) %>% 
  distinct() %>%
  left_join(naics3_cat,by = "NAICS") 

save(final, file = "result/final.rda")
save(final_naics3, file = "result/final_naics3.rda")

# ANALYSIS  =======================
load("Industry/final_naics3.rda")

get_industry <- function(df, cbsa) {
  df %>%
    filter(cbsa_code == cbsa) %>%
    filter(pct_good_jobs > median(pct_good_jobs, na.rm = T)) %>%
    # filter(pct_traded > median(pct_traded, na.rm = T)) %>%
    # filter(pct_traded == 1) %>%
    mutate(pct_accessible = midskill + lowskill, 
           sector = ifelse(is.na(sector), "Government", sector)) %>%
    select(cbsa_code, sector, `Industry Name`, n_good_jobs, contains("pct"), insurance_coverage) %>%
    arrange(-pct_good_jobs)
}

plot_industry <- function(df) {
  metro <- metro.data::cbsa_18 %>% 
    filter(cbsa_code  == df[1, ]$cbsa_code) %>% 
    pull(cbsa_name)
  
  df %>%
    ggplot(aes(
      x = pct_good_jobs,
      y = pct_accessible,
      color = sector,
      text = `Industry Name`
    )) +
    geom_point(aes(size = n_good_jobs), 
               # color = "#3182bd", 
               alpha = 0.8) +
    scale_size(guide = F)+
    scale_color_manual("",
                       values = c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f"))+
    scale_x_continuous(labels = scales::percent) +
    scale_y_continuous(labels = scales::percent) +
    labs(
      x = "share of secure wage jobs", 
      y = "share of secure wage jobs accessible without 4-year degree",
      title = paste0("Industries provide above median share of secure wage jobs in ", metro)
    ) +
    theme_classic()
}

final_naics3 %>%
  get_industry("19740") %>% 
  plot_industry() %>%
  plotly::ggplotly()

final_naics3 %>%
  get_industry("17460") %>% 
  plot_industry() %>%
  plotly::ggplotly()



# SANDBOX =====
final_naics4 <- final %>%
  group_by(naics_4) %>%
  summarise(
    n_good = sum(n, na.rm = T),
    pct_good = sum(n, na.rm = T) / sum(n / pct, na.rm = T)
  ) %>%
  left_join(naics4) %>%
  filter(pct_good != "NaN") %>%
  arrange(-n_good) %>%
  select(NAICS, Description,
         n_good_jobs = n_good,
         pct_good_jobs = pct_good, everything()
  ) %>%
  mutate(naics_code = as.character(NAICS)) %>%
  left_join(metro.data::naics[c("naics_code", "naics4_aitype")]) %>%
  left_join(metro.data::naics %>%
              filter(naics_level == 6) %>%
              mutate(naics4_code = str_sub(naics_code, 1, 4)) %>%
              group_by(naics4_code) %>%
              summarise(pct_traded = sum(traded, na.rm = T) / n()) %>%
              mutate(traded = case_when(
                pct_traded >= 0.5 ~ "traded",
                pct_traded < 0.5 ~ "local",
                T ~ "mixed"
              )) %>% select(naics_code = naics4_code, traded))

final_naics4 %>%
  count(naics4_aitype)

final_naics6 <- final %>%
  left_join(naics6) %>%
  filter(pct != "NaN") %>%
  arrange(-n) %>%
  select(NAICS, Description, n_good_jobs = n, pct_good_jobs = pct, everything())

write.csv(final_naics4, "final_13820.csv")


# job openings gap ---
targets <- "17460"
wage <- secure %>%
  filter(cbsa_code == targets) %>%
  pull(kids) * 2080

jobs <- read_csv(paste0("data/posting/jobs_", targets, ".csv"))
jobs90 <- read_csv(paste0("data/posting/jobs90_", targets, ".csv"))

jobs_unfilled <- jobs %>%
  filter(`Median Annual Earnings` > wage) %>%
  left_join(jobs90, by = "SOC") %>%
  mutate(unfilled = `Unique Postings from Dec 2018 - Dec 2019.x` -
           `Unique Postings from Dec 2018 - Dec 2019.y`) %>%
  select(SOC, Occupation.x, unfilled, `Median Annual Earnings.x`)

jobs_unfilled %>%
  summarise(total = sum(unfilled))

jobs_unfilled %>%
  arrange(-unfilled) %>%
  head(10) %>%
  view()

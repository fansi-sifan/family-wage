# $15 minimal wage
source("2_func.R")

target_struggle <- target_struggle %>% 
  mutate(fam_cat = ifelse(str_detect(cat, "child"), "families with kids", cat),
         cbsa_size = fct_collapse(cbsa_size, `micropolitans and rural areas` = c("nonmetro", "micro")), 
         cbsa_size = fct_relevel(cbsa_size, c("very large metros", "large metros", "midsized metros", "small metros", "micropolitans and rural areas"))) %>% 
  # remove college students
  filter(!(RELP == 20 & SCH %in% c(2,3)))

target_wages_semp <- target_wages_semp %>% 
  mutate(fam_cat = ifelse(str_detect(cat, "child"), "families with kids", cat), 
         cbsa_size = fct_collapse(cbsa_size, `micropolitans and rural areas`  = c("nonmetro", "micro")), 
         cbsa_size = fct_relevel(cbsa_size, c("very large metros", "large metros", "midsized metros", "small metros", "micropolitans and rural areas"))) %>% 
  # remove college students
  filter(!(RELP == 20 & SCH %in% c(2,3))) 
 

# findinfct_lump()
target_struggle %>% 
  # filter(str_detect(cat, "child")) %>%
  mutate(n_child = as.numeric(str_sub(cat, 9, 10))) %>%
  filter(!is.na(struggle_alt)) %>%
  group_by(struggle_alt) %>%
  summarise(
    all_kids = sum(weight * n_child, na.rm = T),
    all_fam = sum(weight, na.rm = T)
  ) %>%
  mutate(pct = all_fam / sum(all_fam))

chart1 <- target_struggle %>%
  plot_struggle(struggle_alt)

cbsa_data <- target_struggle %>%
  select(-contains("cbsa")) %>% 
  left_join(metro.data::county_cbsa_st_18[c("stco_code", "cbsa_code", "cbsa_size", "cbsa_name")], by = "stco_code") %>% 
  # filter(str_detect(cat, "child")) %>%
  filter(!is.na(struggle_alt)) %>%
  group_by(cbsa_code, struggle_alt) %>%
  summarise(n = sum(weight)) %>%
  mutate(pct = n / sum(n)) %>%
  filter(struggle_alt) %>%
  left_join(metro.data::cbsa_18) %>%
  select(cbsa_code, cbsa_name, cbsa_size, cbsa_emp, pct_struggle = pct, n_struggle = n) %>%
  filter(cbsa_size != "micro")

# cbsa_data %>%
#   write.csv("result/cbsa_all_family.csv")

target_struggle %>%
  # filter(str_detect(cat, "child")) %>%
  filter(!is.na(struggle_alt)) %>%
  group_by(cbsa_size, struggle_alt) %>%
  summarise(n = sum(weight)) %>%
  mutate(pct = n / sum(n)) %>%
  filter(struggle_alt) %>%
  ggplot(aes(x = cbsa_size, y = pct)) +
  geom_col() +
  theme_classic()

# map
library(tmap)
load("../metro-recession/data/map.rda")
cbsa_ct <- sf::st_centroid(cbsa)

cbsa_struggle_map <- merge(cbsa_ct, cbsa_data %>%
  filter(str_detect(cbsa_size, "large")) %>%
  rename(GEOID = cbsa_code))

map_struggle <- tm_shape(st, projection = 2163) +
  tm_polygons(alpha = 0.5, border.col = "white") +
  tm_layout(frame = F) +
  tm_shape(cbsa_struggle_map %>% filter(!is.na(pct_struggle))) +
  tm_bubbles(
    size = 0.3,
    col = "pct_struggle",
    alpha = 0.8,
    # style = "quantile",
    border.col = "white",
    title.col = "Percentage of households that are struggling",
    legend.format = list(fun = function(x) scales::percent(x, accuracy = 1))
  ) +
  tm_layout(legend.outside = TRUE) +
  NULL

map_struggle

# findings 2
compare_lifted <- function(col){
  col <- enquo(col)
  
  bind_rows(target_wages_semp %>%
              mutate(lifted = expected_wage <= 15) %>%
              group_by(lifted, !!col) %>%
              summarise(n = sum(weight)) %>%
              group_by(lifted) %>%
              mutate(pct = n / sum(n)) %>% 
              filter(lifted) %>% 
              mutate(lifted = "newly self-sufficient households at $15"),
            
            target_wages_semp %>% 
              group_by(!!col) %>%
              summarise(n = sum(weight)) %>%
              mutate(pct = n / sum(n)) %>% 
              mutate(lifted = "all struggling households"),
            
            target_struggle %>% 
              filter(!is.na(struggle_alt)) %>% 
              group_by(!!col) %>% 
              summarise(n = sum(weight)) %>%
              mutate(pct = n / sum(n)) %>% 
              mutate(lifted = "all households")) %>% 
    
    ggplot(aes(x = lifted, y = pct, fill = !!col))+
    geom_col()+
    geom_text(aes(label = scales::percent(pct, accuracy = 1)), position = "stack", hjust = 1,check_overlap = T) +
    scale_fill_manual(name = "", values = c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f"))+
    coord_flip()+
    scale_y_continuous(labels = scales::percent)+
    labs(x = "", y = "")+
    theme_classic()
  
}


compare_lifted(edu_cat) 

chart_2 <- compare_lifted(fam_cat)
chart_3a <- compare_lifted(sex_cat) +
  scale_fill_manual(values = c("#a6cee3", "#b2df8a"), name = "")

chart_3b <- compare_lifted(race_cat)
chart_4 <- compare_lifted(cbsa_size)

ggsave(chart_2, filename = "result/mw_chart_2.png", height = 3)
ggsave(chart_3a, filename = "result/mw_chart_3a.png", height = 2)
ggsave(chart_3b, filename = "result/mw_chart_3b.png", height = 2)
ggsave(chart_4, filename = "result/mw_chart_4.png", height = 3)
#[depreciated] findings 2
find_lifted <- function(df, col, ...) {
  col <- enquo(col)

  df %>%
    mutate(lifted = expected_wage <= 15) %>%
    group_by(lifted, !!col, ...) %>%
    summarise(n = sum(weight)) %>%
    group_by(!!col, ...) %>%
    mutate(pct = n / sum(n)) %>%
    filter(lifted) %>%
    rename(type = !!col)
}

target_wages_semp %>%
  find_lifted(1)


# chart 2
chart2 <- target_wages_semp %>%
  find_lifted(cat) %>%
  ggplot(aes(
    x = type, y = pct,
    label = scales::percent(pct, accuracy = 0.1)
  )) +
  geom_col(fill = "#1f78b4") +
  geom_text(vjust = -0.5) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    y = "", x = "",
    title = "Households without children are much more likely to achieve self-sufficiency as a result of a $15 federal minimum wage",
    subtitle = "Percentage of struggling households, by family composition, that would achieve self-sufficiency with a $15 wage"
  ) +
  theme_classic()

# chart 3
chart3 <- target_wages_semp %>%
  mutate(fam_cat = case_when(
    str_detect(cat, "1 adult,") ~ "single parent",
    cat %in% c("1 adult", "2 adult") ~ "no children",
    T ~ "two parents"
  )) %>%
  # mutate(fam_cat = ifelse(str_detect(cat, "child"), "families with children", "households without children")) %>%
  find_lifted(fam_cat, race_cat) %>%
  ggplot(aes(x = reorder(race_cat, pct), y = pct, 
             fill = reorder(type, pct), 
             label = scales::percent(pct, accuracy = 0.1))) +
  geom_col(position = "dodge") +
  geom_text(position = position_dodge(1), vjust = -0.5) +
  scale_fill_manual(values = c("#a6cee3", "#1f78b4", "#b2df8a"), name = "") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    y = "", x = "",
    title = "Households headed by white individuals are more likely to make ends meet at $15 minimum wage",
    subtitle = "Percentage of struggling households, by household head, that would achieve self-sufficiency with a $15 wage"
  ) +
  theme_classic()

# chart 4
chart4 <- target_wages_semp %>%
  find_lifted(cbsa_size) %>%
  ggplot(aes(
    x = reorder(type, pct), y = pct,
    label = scales::percent(pct, accuracy = 0.1)
  )) +
  geom_col(fill = "#1f78b4") +
  geom_text(vjust = -0.5) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    y = "", x = "",
    title = "Households in very large metro areas are less likely to achieve self-sufficiency with a $15 minimum wage",
    subtitle = "Percentage of struggling households, by residing metro areas, that would achieve self-sufficiency with a $15 wage"
  ) +
  theme_classic()

target_wages_semp %>% 
  select(stco_code, pct_kids_fam, pct_all_fam, expected_wage) %>%
  filter(expected_wage > 14.9) %>%
  slice(1) %>%
  select(-contains("cbsa")) %>% 
  left_join(metro.data::county_cbsa_st_18[c("stco_code", "cbsa_code", "cbsa_size", "cbsa_name")], by = "stco_code") %>% 
  filter(cbsa_size != "micro") %>%
  write.csv("mw_lifted_cbsa.csv")

# findings 3
find_15 <- function(col) {
  col <- enquo(col)

  target_wages_semp %>%
    # remove college students
    filter(!(RELP == 20 & SCH %in% c(2,3))) %>%
    
    filter(expected_wage <= 15) %>%
    group_by(!!col) %>%
    summarise(n = sum(weight)) %>%
    mutate(pct = n / sum(n)) %>%
    rename(type = !!col)
}

find_15(sex_cat)
find_15(race_cat)
find_15(edu_cat)
find_15(cat)


ggsave(chart1, filename = "result/mw_chart1.pdf",height = 3)
tmap_save(map_struggle, "result/mw_map1.pdf")

ggsave(chart2, filename = "result/mw_chart2.pdf", height = 3)
ggsave(chart3, filename = "result/mw_chart3.pdf", height = 3)
ggsave(chart4, filename = "result/mw_chart4.pdf", height = 3)

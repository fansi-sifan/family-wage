# data and charts for report
# LOAD ========
source("2_func.R")
load("result/final_naics3.rda")

final_naics3 <- final_naics3 %>%
  filter(n_good_jobs > 0)

# IN REPORT =====
# number of kids struggling
target_struggle %>%
  filter(str_detect(cat, "child")) %>%
  mutate(n_child = as.numeric(str_sub(cat, 9, 10))) %>%
  filter(!is.na(struggle_alt)) %>%
  group_by(struggle_alt) %>%
  summarise(
    all_kids = sum(weight * n_child, na.rm = T),
    all_fam = sum(weight, na.rm = T)
  ) %>%
  mutate(pct = all_fam / sum(all_fam))

# TABLE ===
# load("../metro.data/data-raw/naics_sc.rda")

naics_sc %>%
  group_by(sector) %>%
  slice_max(emp15, n = 3) %>%
  # group_by(sector) %>%
  # summarise(industry = paste0(naics6_name, collapse = ",")) %>%
  view()

# APPENDIX =====
df <- get_cbsa_summary(target_wages_semp, struggle_alt) %T>%
  write.csv("result/cbsa_wages19.csv") %>%
  filter(str_detect(cbsa_size, "large"))

final_naics3 %>%
  group_by(cbsa_code) %>%
  filter(
    pct_good_jobs > median(pct_good_jobs, na.rm = T),
    Jobs > 500
  ) %>%
  mutate(
    pct_accessible = midskill + lowskill,
    sector = ifelse(is.na(sector), "Government", sector)
  ) %>%
  select(cbsa_code, sector, `Industry Name`, n_good_jobs, contains("pct"), insurance_coverage) %>%
  write.csv("result/cbsa_naics3.csv")

# chart: struggling share changes as wage increases -----
chart1 <- target_wages_semp %>%
  filter(cbsa_code == "13820") %>%
  select(pct_kids_fam, pct_all_fam, expected_wage) %>%
  # write.csv("result/wage_test.csv")
  test_wage(0.5)

library(plotly)
ggplotly(chart1) %>%
  layout(xaxis = list(showspikes = T)) %>%
  layout(yaxis = list(showspikes = T))

# chart: struggling share by demography -----------
chart2 <- target_struggle %>%
  plot_struggle(struggle_alt)

# map ===============
# read in median earnings
cbsa_median <- readxl::read_xlsx("data-raw/OES_Report.xlsx", skip = 5)

cbsa_data <- cbsa_median %>%
  mutate(cbsa_code = str_sub(`Area Name`, -6, -2)) %>%
  # fix BLS codes for top 106 metros
  mutate(cbsa_code = case_when(
    cbsa_code == "71650" ~ "14460",
    cbsa_code == "71950" ~ "14860",
    cbsa_code == "19380" ~ "19430",
    cbsa_code == "75700" ~ "35300",
    cbsa_code == "76750" ~ "38860",
    cbsa_code == "73450" ~ "25540",
    cbsa_code == "77200" ~ "39300",
    cbsa_code == "44100" ~ "44140",
    cbsa_code == "79600" ~ "49340",
    T ~ cbsa_code
  )) %>%
  right_join(df) %>%
  mutate(
    wage = as.numeric(`Hourly median wage`),
    delta = wage - kids,
    delta_cat = cut(delta, c(-Inf, -6, -4, -2, 0, 2))
  )

chart5 <- cbsa_data %>%
  mutate(point_col = case_when(
    cbsa_code %in% c("47900", "42660", "25540", "44060", "31540", "40900") ~ "high",
    cbsa_code %in% c("46520", "41940", "33100", "41860", "19660", "42220", "37100") ~ "low",
    T ~ "other"
  )) %>%
  # filter(cbsa_size == "very large metros") %>%
  ggplot(aes(x = kids_fam, y = wage, text = `Area Name`)) +
  geom_point(alpha = 0.5, size = 4, aes(color = point_col)) +
  scale_color_manual(values = c("#33a02c", "#e31a1c", "#1f78b4"), guide = F) +
  scale_x_continuous("family sustaining wage thresholds",
    limits = c(15, 38),
    labels = scales::dollar
  ) +
  scale_y_continuous("median wage",
    limits = c(15, 38),
    labels = scales::dollar
  ) +
  # diagonal line
  geom_abline(slope = 1, linetype = "dotted", color = "#1f78b4") +
  geom_errorbarh(aes(xmax = 32.33446, xmin = 22.07, y = 22.07),
    color = "#e31a1c",
    # linetype = "dashed",
    size = 1
  ) +
  annotate("text",
    x = 30, y = 34,
    label = "median wage = family sustaining wage",
    color = "#1f78b4"
  ) +
  # label points above
  ggrepel::geom_text_repel(
    data = . %>%
      filter(point_col == "high"),
    aes(label = str_extract(cbsa_name, "[^-|,|\\/]+")),
    min.segment.length = 0,
    nudge_x = -2
  ) +
  # label points below
  ggrepel::geom_text_repel(
    data = . %>%
      filter(point_col == "low"),
    aes(label = str_extract(cbsa_name, "[^-|,|\\/]+")),
    min.segment.length = 0,
    nudge_x = 2
  ) +

  # geom_hline(yintercept = median(cbsa_data$wage), linetype = "dashed")+
  # geom_vline(xintercept = median(cbsa_data$kids), linetype = "dashed")+
  theme_light()

chart5

# ggplotly(chart5)

cbsa_data %>%
  filter(cbsa_size == "very large metros") %>%
  ggplot(aes(x = reorder(`Area Name`, delta), y = delta)) +
  geom_col() +
  theme_classic()



# MAP -------------
library(tmap)
load("../metro-recession/data/map.rda")
cbsa_ct <- sf::st_centroid(cbsa)
cbsa_struggle_map <- merge(cbsa_ct, cbsa_data %>% rename(GEOID = cbsa_code))

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
    title.col = "Share of families with children who are struggling",
    legend.format = list(fun = function(x) scales::percent(x, accuracy = 1))
  ) +
  tm_layout(legend.outside = TRUE) +
  NULL

map_delta <- tm_shape(st, projection = 2163) +
  tm_polygons(alpha = 0.5, border.col = "white") +
  tm_layout(frame = F) +
  tm_shape(cbsa_struggle_map %>% filter(!is.na(pct_struggle))) +
  tm_bubbles(
    size = 0.3,
    # size = "pct_kids_gap",
    col = "delta_cat",
    # style = "equal",
    palette = c("#d7191c", "#fee08b", "#d9ef8b", "#91cf60", "#1a9850"),
    alpha = 0.8,
    border.col = "white",
    label = c("> $6", "$4 - 6", "$2 - 4", "$0 - 2", " < $0"),
    title.col = "Family sustaining wage gap",
    title.size = "Family sustaining wage job deficit as share of all jobs",
    legend.format = list(fun = function(x) scales::percent(x, accuracy = 1))
  ) +
  tm_layout(legend.outside = TRUE) +
  NULL

# chart: secure wage share by sector across large metros
final_sector <- final_naics3 %>%
  filter(!is.na(sector)) %>%
  group_by(cbsa_code, sector) %>%
  summarise(pct_good_jobs = sum(n_good_jobs) / sum(n_good_jobs / pct_good_jobs))

chart3 <- final_sector %>%
  mutate(type = "sector") %>%
  bind_rows(final_sector %>%
    mutate(
      type = "traded",
      sector = ifelse(str_detect(sector, "Traded"),
        "Traded", "Local"
      )
    )) %>%
  ggplot(aes(
    x = fct_reorder(sector, pct_good_jobs, .fun = "median"),
    y = pct_good_jobs,
    fill = sector
  )) +
  geom_boxplot() +
  scale_fill_manual("Industry sectors",
    guide = F,
    values = c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6")
  ) +
  scale_x_discrete("") +
  scale_y_continuous("Share of jobs providing family sustaining wage", labels = scales::percent) +
  # coord_flip()+
  stat_summary(
    aes(label = sprintf("%1.0f%%", 100 * ..y..)),
    geom = "text",
    fun.y = function(y) boxplot.stats(y, do.conf = F)$stats,
    position = position_nudge(x = 0.5),
    size = 3.5
  ) +
  theme_classic() +
  coord_flip() +
  # theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  facet_grid(type ~ ., scales = "free_y", space = "free") +
  theme(
    strip.background = element_blank(),
    strip.text.y = element_blank()
  ) +
  NULL


# industries ranked by % secure jobs
final_naics3 %>%
  filter(cbsa_code %in% target_cbsa) %>%
  group_by(`Industry Name`) %>%
  slice_min(pct_good_jobs) %>%
  arrange(-pct_good_jobs) %>%
  select(`Industry Name`, pct_good_jobs) %>%
  distinct() %>%
  view()

# chart 4 --------
get_industry <- function(df, cbsa) {
  df %>%
    filter(cbsa_code == cbsa) %>%
    filter(pct_good_jobs > median(pct_good_jobs, na.rm = T)) %>%
    # filter(pct_traded > median(pct_traded, na.rm = T)) %>%
    # filter(pct_traded == 1) %>%
    mutate(
      pct_accessible = midskill + lowskill,
      sector = ifelse(is.na(sector), "Government", sector)
    ) %>%
    select(cbsa_code, sector, `Industry Name`, Jobs, n_good_jobs, contains("pct"), insurance_coverage) %>%
    arrange(-pct_good_jobs)
}

plot_industry_bar <- function(df) {
  metro <- metro.data::cbsa_18 %>%
    filter(cbsa_code == df[1, ]$cbsa_code) %>%
    pull(cbsa_name)

  df %>%
    mutate(pct_inaccessible = pct_good_jobs - pct_accessible) %>%
    pivot_longer(contains("accessible")) %>%
    ggplot(aes(
      x = reorder(Industry, value),
      y = value,
      alpha = factor(name, levels = c("pct_inaccessible", "pct_accessible")),
      fill = sector,
      text = Industry
    )) +
    geom_col() +
    scale_alpha_discrete("education requirements",
      # values = c(0.5,1),
      label = c("BA or higher", "All others")
    ) +
    scale_fill_manual("sector",
      values = c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f")
    ) +
    # scale_x_continuous(labels = scales::percent) +
    scale_y_continuous(labels = scales::percent) +
    coord_flip() +
    labs(
      x = "NAICS 3-digit industries",
      y = "share of jobs that are accessible to workers without 4-year degree (bolded bars) and with 4-year degree (faded bars)",
      title = paste0("Industries provide above median share of family sustaining wage jobs in ", metro)
    ) +
    theme_classic()
}

shorten <- tibble::tribble(
  ~`Industry Name`, ~Industry,
  "Securities, Commodity Contracts, and Other Financial Investments and Related Activities", "Finance",
  "Monetary Authorities-Central Bank", "Central Bank",
  "Utilities", "Utilities",
  "Rail Transportation", "Rail Transportation",
  "Data Processing, Hosting, and Related Services", "Data Processing",
  "Professional, Scientific, and Technical Services", "Professional Services",
  "Federal Government", "Federal Government",
  "Management of Companies and Enterprises", "Management of Companies",
  "Broadcasting (except Internet)", "Broadcasting",
  "Air Transportation", "Air Transportation",
  "Petroleum and Coal Products Manufacturing", "Petroleum and Coal Products Mfg",
  "Other Information Services", "Other Information Services",
  "Insurance Carriers and Related Activities", "Insurance Carriers",
  "Publishing Industries (except Internet)", "Publishing Industries",
  "Computer and Electronic Product Manufacturing", "Computer and Electronic Product Mfg",
  "Telecommunications", "Telecommunications",
  "Heavy and Civil Engineering Construction", "Heavy and Civil Engineering Construction",
  "Hospitals", "Hospitals",
  "Construction of Buildings", "Construction of Buildings",
  "Credit Intermediation and Related Activities", "Credit Intermediation",
  "Specialty Trade Contractors", "Specialty Trade Contractors",
  "Wholesale Electronic Markets and Agents and Brokers", "Wholesale Electronic Markets",
  "Mining (except Oil and Gas)", "Mining",
  "Waste Management and Remediation Services", "Waste Management",
  "State Government", "State Government",
  "Local Government", "Local Government",
  "Chemical Manufacturing", "Chemical Mfg",
  "Merchant Wholesalers, Durable Goods", "Merchant Wholesalers, Durable Goods",
  "Educational Services", "Educational Services",
  "Machinery Manufacturing", "Machinery Mfg",
  "Ambulatory Health Care Services", "Ambulatory Health Care Services",
  "Electrical Equipment, Appliance, and Component Manufacturing", "Electrical Equipment, and Component Mfg",
  "Truck Transportation", "Truck Transportation",
  "Transportation Equipment Manufacturing", "Transportation Equipment Mfg",
  "Support Activities for Transportation", "Support Activities for Transportation",
  "Real Estate", "Real Estate",
  "Miscellaneous Manufacturing", "Miscellaneous Mfg",
  "Nonmetallic Mineral Product Manufacturing", "Nonmetallic Mineral Product Mfg",
  "Fabricated Metal Product Manufacturing", "Fabricated Metal Product Mfg",
  "Religious, Grantmaking, Civic, Professional, and Similar Organizations", "Religious Organizations"
)



modified_ind <- final_naics3 %>%
  get_industry("17460") %>%
  # get_industry("41740") %>%
  filter(Jobs > 500) %>%
  left_join(shorten) %>%
  mutate(Industry = ifelse(is.na(Industry), `Industry Name`, Industry))

chart4 <- modified_ind %>%
  plot_industry_bar() +
  # theme(axis.ticks.y = element_blank(),
  #       axis.text.y = element_blank())+
  NULL

chart4 %>%
  plotly::ggplotly(tooltip = c("sector", "value")) %>%
  layout(showlegend = F)

# Export ===
ggsave(chart1, filename = "result/chart1.png", width = 8, height = 3)
ggsave(chart2, filename = "result/chart2.pdf", height = 3)
ggsave(chart3, filename = "result/chart3.png", height = 5)
ggsave(chart4, filename = "result/chart4.pdf", width = 12, height = 5)
ggsave(chart5, filename = "result/chart5.pdf", width = 6, height = 3)

tmap_save(map_struggle, "result/map1.png")
tmap_save(map_delta, "result/map2.png")

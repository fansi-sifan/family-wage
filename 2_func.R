library(tidyverse)
load("result/stco_struggle19.rda")
load("result/target_wage.rda")
# ANALYSIS ============
find_earners <- function(df){
  df %>%
    # filter(str_detect(cat, "child")) %>%
    mutate(n_worker = case_when(
      str_detect(cat, "2 adult") & PINCP.x - PINCP.y < 5000 ~ 2,
      T ~ 1
    )) 
}


# metro level summary
get_cbsa_summary <- function(df, semp){
  semp <- enquo(semp)
  
  # get overall struggling share and struggling family with kids
  target_struggle %>%
    filter(str_detect(cat, "child")) %>%
    filter(!is.na(struggle_alt)) %>%
    group_by(cbsa_code, !!semp) %>%
    summarise(n = sum(weight)) %>%
    mutate(pct = n / sum(n)) %>%
    filter(!!semp) %>%
    left_join(metro.data::cbsa_18) %>%
    select(cbsa_code, cbsa_name, cbsa_size, cbsa_emp, pct_struggle = pct, n_struggle = n) %>%
    filter(cbsa_size != "micro") %>%
    
    # get number of kids and workers that are struggling
    left_join(df %>%
                find_earners() %>% 
                group_by(cbsa_code) %>%
                mutate(
                  n_kids = sum(kids * weight, na.rm = T)/2 , # median
                  n_all = sum(n_worker * (kids!=0) * weight, na.rm = T)/2 # median
                ) %>%
                filter(n_all * n_kids != 0) %>%
                group_by(cbsa_code, n_all, n_kids) %>%
                summarise(
                  kids = spatstat::weighted.quantile(expected_wage, kids * weight, probs = 0.4, na.rm = T),
                  kids_fam = spatstat::weighted.quantile(expected_wage, kids_fam * weight,  probs = 0.4,na.rm = T),
                  fam = spatstat::weighted.quantile(expected_wage, fam * weight,  probs = 0.4,na.rm = T)
                )) %>%
                mutate(pct_kids_gap = n_all / cbsa_emp)
  }

# who are struggling families? --------

sum_struggle <- function(df, col, semp, ...) {
  col <- enquo(col)
  semp <- enquo(semp)
  
  df %>%
    group_by(!!semp, ...) %>%
    summarise(value = weighted.mean(!!col, weight, na.rm = T))
}

# target_struggle %>%
#   sum_struggle(WKHP, struggling)
#
# target_struggle %>%
#   filter(cbsa_code == target_cbsa) %>%
#   # sum_struggle(WKHP, struggle_alt, income == inc_wage) %>%
#   sum_struggle(WKHP, struggle_alt, COW %in% c(6,7,8))

who_struggle <- function(df, ...) {
  df %>%
    group_by(...) %>%
    summarise(n = sum(weight)) %>%
    mutate(pct = n / sum(n))
}

# target_struggle %>%
#   filter(cbsa_code == "13820") %>%
#   # filter(cbsa_code == target_cbsa) %>%
#   # filter(!is.na(struggle_alt)) %>%
#
#   who_struggle(struggling)
#   # who_struggle(WKW, struggle_alt)
#   who_struggle(minority_cat, struggle_alt)
#   who_struggle(sex_cat, struggle_alt)

# target_struggle %>%
#   mutate(is.owner = COW %in% c(6,7,8)) %>%
#   # who_struggle(is.owner, struggle_alt, SEMP.x >0)
#   sum_struggle(SEMP.x, COW %in% c(6,7,8))

# struggle share by race/gender/edu -------------------
# create plots for every cbsa
subplots <- function(df, col, semp, pal) {
  col <- rlang::enquo(col)
  semp <- rlang::enquo(semp)
  
  df %>%
    group_by(!!semp, !!col) %>%
    summarise(n = sum(weight)) %>%
    group_by(!!col) %>%
    mutate(pct = n / sum(n)) %>%
    # filter(struggling) %>%
    filter(!!semp) %>%
    ggplot(aes(x = reorder(!!col, pct), y = pct, 
               label = scales::percent(pct, accuracy = 1), 
               fill = reorder(!!col, pct))) +
    scale_fill_brewer(palette = pal, guide = F) +
    geom_col() +
    geom_text() +
    labs(x = "", y = "") +
    scale_y_continuous(labels = scales::percent) +
    theme_classic() +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.line.x = element_blank()
    )+
    coord_flip()
}

# target_struggle %>%
#   filter(cbsa_code == target_cbsa) %>%
#   subplots(race_cat, struggle_alt, "Greens")


plot_struggle <- function(df, semp) {
  semp <- rlang::enquo(semp)
  
  sex <- subplots(df, sex_cat, !!semp, "Purples")  %>% ggplotGrob()
  race <- subplots(df, race_cat, !!semp, "Blues") %>% ggplotGrob()
  edu <- subplots(df, edu_cat, !!semp, "Greens") %>% ggplotGrob()
  # type <- subplots(df, cat, !!semp, "Oranges") %>% ggplotGrob()
  
  gridExtra::grid.arrange(grobs = list(sex, race, edu),
                          widths = c(1, 2, 2),
                          top = "Percentage of households that are struggling by characteristics of household head")
  
}

# target_struggle %>%
#   filter(cbsa_code == target_cbsa) %>%
#   plot_struggle(struggle_alt)



# family composition by race/gender/edu + family budget ---

# source of income by family type

# target_wages %>%
#   filter(struggling) %>%
#   group_by(race_cat, cat) %>%
#   summarise(income = sum(income * weight) / sum(weight)) %>%
#   ggplot(aes(x = reorder(race_cat, income), y = income, fill = cat)) +
#   geom_col() +
#   facet_wrap(~cat)

test_wage <- function(df, percent) {
  
  # wage <- target_wages_semp %>%
  #   filter(cbsa_code == "13820") %>%
  #   select(pct_kids_fam, expected_wage) %>% 
  #   filter(pct_kids_fam >=0.5) %>%
  #   slice_min(expected_wage) %>% 
  #   pull(expected_wage)
  
  df %>%
    pivot_longer(contains("pct")) %>%
    ggplot() +
    geom_step(aes(x = expected_wage, y = value, color = name), size = 1.5) +
    # geom_segment(data = data.frame(x = c(12,wage),
    #                                xend = c(wage,wage),
    #                                y = c(percent,0),
    #                                yend = c(percent,percent)),
    #              aes(x = x, xend = xend, y = y, yend = yend),
    #              color = "grey", linetype = "dashed", size = 1) +

    scale_color_manual("Wage threshold tests",
                       values = c("#e41a1c", "#377eb8"),
      labels = c("All families", "Families with kids")
    ) +
    scale_x_continuous(name = "Expected hourly wage",
                       limits = c(12, 40), labels = scales::dollar, n.breaks = 20) +
    scale_y_continuous(labels = scales::percent, 
                       name = "% lifted out of struggling status") +
    labs(title = "Hourly wage required to lift families out of struggling status")+
    theme_classic()
}



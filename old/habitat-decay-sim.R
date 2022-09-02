library(tidyverse)
library(lubridate)
library(gridExtra)

habitat <- read_csv("data/chinook_habitat_decay_graph.csv", 
                    skip = 1, col_names = c("flow", "area", "location"))

habitat %>% distinct(location)

benton <- habitat %>% filter(location == "Benton Dr. to Cypress Ave. Bridge")
cypress <- habitat %>% filter(location == "Cypress Ave. Bridge to Bonnyview")
keswick <- habitat %>% filter(location == "Keswick to Benton Dr. Bridge")
bonnyview <- habitat %>% filter(location == "Bonnyview to Clear Creek")

flow_vals <- seq(110, 2000, by = 50)

benton_vals <- approxfun(benton$flow, benton$area)(flow_vals)
cypress_vals <- approxfun(cypress$flow, cypress$area)(flow_vals)
keswick_vals <- approxfun(keswick$flow, keswick$area)(flow_vals)
bonnyview_vals <- approxfun(bonnyview$flow, bonnyview$area)(flow_vals)

upper_sac_area_by_location <- tibble(
  flow = rep(flow_vals, 4), 
  area = c(benton_vals, cypress_vals, keswick_vals, bonnyview_vals), 
  location = rep(c("benton_vals", "cypress_vals", "keswick_vals", "bonnyview_vals"), each = length(flow_vals))
) %>% 
  mutate(
    area_sqm = area * 10^5, 
    acres = area_sqm / 4047
  )

upper_sac_area_by_location %>% 
  ggplot(aes(flow, acres, color=location)) + geom_line() + geom_po

upper_sac_area_watershed <- upper_sac_area_by_location %>% 
  group_by(flow) %>% 
  summarise(
    acres = sum(acres, na.rm = TRUE)
  ) 

upper_sac_area_watershed %>% 
  ggplot(aes(flow, acres)) + 
  geom_point() + 
  geom_line()

# START -------------

starting_vol <- 66.9 * (43560) * 2

kwk_usgs <- dataRetrieval::readNWISdv("11370500", parameterCd = "00060", startDate = "1980-01-01", 
                                      endDate = "2021-01-01") %>% 
  dataRetrieval::renameNWISColumns() %>% 
  as_tibble()

kwk_usgs_daily <- kwk_usgs %>% 
  mutate(flow_day = Flow * 86400)

kwk_transport <- kwk_usgs_daily %>% 
  mutate(
    sed_transport_min = upper_sac_func_min(flow_day),
    sed_transport_avg = upper_sac_func_avg(flow_day),
    sed_transport_max = upper_sac_func_max(flow_day)
  ) %>% as_tibble() %>% 
  select(-agency_cd, -site_no, Flow_cd)



augmentation_dates <- c() %>% 
  as_date()

sim <- kwk_transport %>% 
  filter(Date >= "2016-01-01", Date <= "2020-01-01") %>% 
  mutate(sed_transport = ifelse(is.na(sed_transport), 0, sed_transport), 
         sed_transport_accum = cumsum(sed_transport),
         start_vol = starting_vol,
         current_vol = start_vol - sed_transport_accum)

# WIP 


augmentation_dates <- tibble(
  date = as_date(c()), 
  amount = as.numeric(c())
)

augmentation_sim <- kwk_transport %>% 
  filter(Date >= "1990-05-01", Date <= "1995-05-01") %>%
  mutate(
    sed_transport_min = ifelse(is.na(sed_transport_min), 0, sed_transport_min), 
    sed_transport_max = ifelse(is.na(sed_transport_max), 0, sed_transport_max), 
    sed_transport_avg = ifelse(is.na(sed_transport_avg), 0, sed_transport_avg), 
    # add_gravel = case_when(
    #   Date %in% augmentation_dates$date ~ augmentation_dates$amount, 
    #   TRUE ~ 0
    # ),
    add_gravel = ifelse(Date %in% augmentation_dates$date, augmentation_dates$amount, 0),
    starting_vol = starting_vol,
    current_vol_min = as.numeric(accumulate2(sed_transport_min[2:n()], add_gravel[2:n()], ~..1 - ..2 + ..3, .init = starting_vol[1]-sed_transport_min[1])),
    current_vol_avg = as.numeric(accumulate2(sed_transport_avg[2:n()], add_gravel[2:n()], ~..1 - ..2 + ..3, .init = starting_vol[1]-sed_transport_avg[1])),
    current_vol_max = as.numeric(accumulate2(sed_transport_max[2:n()], add_gravel[2:n()], ~..1 - ..2 + ..3, .init = starting_vol[1]-sed_transport_max[1]))
    ) %>% 
  mutate(
    acres_min = current_vol_min / 43560 / 2,
    acres_avg = current_vol_avg / 43560 / 2,
    acres_max = current_vol_max / 43560 / 2
  )

monthly_sim <- augmentation_sim %>% 
  group_by(year = year(Date), month = month(Date)) %>% 
  summarise(
    avg_flow = mean(Flow, na.rm = TRUE),
    avg_acres_min = mean(acres_min),
    avg_acres_avg = mean(acres_avg),
    avg_acres_max = mean(acres_max)
  ) %>% ungroup() %>% 
  mutate(Date = as_date(paste0(year, "-", month, "-01")))


grid.arrange(
  augmentation_sim %>% ggplot(aes(Date, Flow)) + geom_line() + labs(title = "Keswick Flow and Transport (1990-1995)"), 
  augmentation_sim %>% ggplot() + 
    geom_line(aes(Date, acres_min, color = "min"), size=.7) + 
    geom_line(aes(Date, acres_avg, color = "avg"), size=.7) + 
    geom_line(aes(Date, acres_max, color = "max"), size=.7) + 
    geom_hline(yintercept = 0, linetype = 2) + 
    scale_y_continuous(labels = scales::comma) + labs(y = "Acres", color = "") + 
    theme(legend.position="bottom"), 
  nrow = 2) 

grid.arrange(
  monthly_sim %>% ggplot(aes(Date, avg_flow)) + geom_line() + 
    labs(title = "Keswick Flow and Transport (1990-1995)", 
         y = "Average Monthly Flow (cfs)"), 
  monthly_sim %>% ggplot() + 
    geom_line(aes(Date, avg_acres_min, color = "min"), size=.7) + 
    geom_line(aes(Date, avg_acres_avg, color = "avg"), size=.7) + 
    geom_line(aes(Date, avg_acres_max, color = "max"), size=.7) + 
    geom_hline(yintercept = 0, linetype = 2) + 
    scale_y_continuous(labels = scales::comma) + labs(y = "Acres", color = "") + 
    theme(legend.position="bottom"), 
  nrow = 2) 

# next steps
# be able to add augmentations through the time series whenever we want
# look at the spreadsheet tab "mainstem sacramento river" for gravel augs
# data back to 1996
# how to distribute augmentation across time? 

# 
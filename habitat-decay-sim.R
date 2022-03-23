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

upper_sac_area <- tibble(
  flow = rep(flow_vals, 4), 
  area = c(benton_vals, cypress_vals, keswick_vals, bonnyview_vals)
) %>% 
  mutate(
    area_sqm = area * 10^5, 
    acres = area_sqm / 4047
  ) %>% 
  group_by(flow) %>% 
  summarise(
    acres = sum(acres, na.rm = TRUE)
  ) 

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
    sed_transport = upper_sac_func(flow_day)
  ) %>% as_tibble() %>% 
  select(-agency_cd, -site_no, Flow_cd)


gravel_augs <- tibble(
  Date = seq.Date(as_date("1980-01-01"), as_date("2000-01-01"), by="1 day")
) 

augmentation_dates <- c(as_date("1985-01-01"), "1983-01-01") %>% 
  as_date()

sim <- kwk_transport %>% 
  filter(Date >= "2016-01-01", Date <= "2020-01-01") %>% 
  mutate(sed_transport = ifelse(is.na(sed_transport), 0, sed_transport), 
         sed_transport_accum = cumsum(sed_transport),
         start_vol = starting_vol,
         current_vol = start_vol - sed_transport_accum)

# WIP 


kwk_transport %>% 
  filter(Date >= "1982-05-01", Date <= "1985-05-01") %>%
  mutate(sed_transport = ifelse(is.na(sed_transport), 0, sed_transport), 
         add_gravel = case_when(
           Date %in% augmentation_dates ~ 100000, 
           TRUE ~ 0
         ),
         starting_vol = starting_vol,
         current_vol = as.numeric(accumulate2(sed_transport[2:n()], add_gravel[2:n()], ~..1 - ..2 + ..3, .init = starting_vol[1]-sed_transport[1]))) 

grid.arrange(sim %>% ggplot(aes(Date, Flow)) + geom_line() + labs(title = "Keswick using Max Scale Down for 40mm"), 
             sim %>% ggplot(aes(Date, current_vol)) + geom_line() + geom_hline(yintercept = 0) , 
             nrow = 2) 

# next steps
# be able to add augmentations through the time series whenever we want
# look at the spreadsheet tab "mainstem sacramento river" for gravel augs
# data back to 1996
# how to distribute augmentation across time? 

# 
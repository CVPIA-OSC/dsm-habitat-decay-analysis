# the goal here is to compare the spawning habitat currently used 
# in the model vs the spawning habitat that is compute from this 
# new analysis

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
    flow_cfs = flow * 3.281,
    area_sqm = area * 10^5, 
    acres = area_sqm / 4047, 
    vol_m3 = area_sqm * 2
  )

upper_sac_area_by_location %>% 
  ggplot(aes(flow_cfs, acres, color=location)) + geom_line() + geom_point()

upper_sac_area_watershed <- upper_sac_area_by_location %>% 
  group_by(flow_cfs) %>% 
  summarise(
    acres = sum(acres, na.rm = TRUE),
    vol_m3 = sum(vol_m3, na.rm = TRUE), 
    area_sqm = sum(area_sqm, na.rm = TRUE)
  ) 

upper_sac_area_watershed %>% 
  ggplot(aes(flow_cfs, acres)) + 
  geom_point() + 
  geom_line() +
  scale_x_continuous(breaks = seq(500, 7000, by=250))

upper_sac_habitat_func <- approxfun(upper_sac_area_watershed$flow_cfs, upper_sac_area_watershed$area_sqm)
model_flow_to_habitat_vol <- approxfun(upper_sac_area_watershed$flow_cfs, upper_sac_area_watershed$vol_m3)


habitats <- list(
  spawning_habitat = fallRunDSM::params$spawning_habitat,
  inchannel_habitat_fry = fallRunDSM::params$inchannel_habitat_fry,
  inchannel_habitat_juvenile = fallRunDSM::params$inchannel_habitat_juvenile,
  floodplain_habitat = fallRunDSM::params$floodplain_habitat,
  weeks_flooded = fallRunDSM::params$weeks_flooded
)


scenario_data <- DSMscenario::load_scenario(DSMscenario::scenarios$NO_ACTION,
                                            habitat_inputs = habitats,
                                            species = DSMscenario::species$FALL_RUN,
                                            spawn_decay_rate = fallRunDSM::params$spawn_decay_rate,
                                            rear_decay_rate = fallRunDSM::params$rear_decay_rate,
                                            stochastic = FALSE)

dim(scenario_data$spawning_habitat)

upper_sac_current <- as.data.frame(scenario_data$spawning_habitat["Upper Sacramento River",,]) %>% 
  as_tibble() %>% 
  mutate(month = month.abb) %>% 
  gather(year, habitat, -month) %>% 
  mutate(year = as.numeric(year), 
         date = as_date(paste0(year, "-", month, "-01")), 
         type = "Baseline w/ Decay") 

upper_sac_no_decay <- as.data.frame(fallRunDSM::params$spawning_habitat["Upper Sacramento River",,]) %>% 
  as_tibble() %>% 
  mutate(month = month.abb) %>% 
  gather(year, habitat, -month) %>% 
  mutate(year = as.numeric(year), 
         date = as_date(paste0(year, "-", month, "-01")), 
         type = "no decay")

upper_sac_habitat <- bind_rows(
  upper_sac_current, 
  upper_sac_no_decay
)


scenario_hab <- upper_sac_habitat %>% 
  ggplot(aes(date, habitat, color=type)) + geom_line() + 
  labs(y = expression("Habitat "~m^2))


upper_sac_calsim_flows <- as.data.frame(DSMflow::upper_sacramento_flows) %>% 
  as_tibble() %>% 
  mutate(month = month.abb) %>% 
  gather(year, flow, -month) %>% 
  mutate(year = as.numeric(year), 
         date = as_date(paste0(year, "-", month, "-01")))

# habitat from package is in sqaure meters, here convert to cube feet
starting_vol <- max(DSMhabitat::fr_spawn["Upper Sacramento River",,]) * 10.7639 * 2

# daily data at kwk gage from USGS
kwk_usgs <- dataRetrieval::readNWISdv("11370500", parameterCd = "00060", startDate = "1975-01-01", 
                                      endDate = "2021-01-01") %>% 
  dataRetrieval::renameNWISColumns() %>% 
  as_tibble()


kwk_usgs %>% 
  mutate(month = month(Date), 
         year = year(Date)) %>% 
  group_by(year, month) %>% 
  mutate(avg_in_month = mean(Flow)) %>% 
  ggplot(aes(Flow, avg_in_month)) + geom_point()
  


upper_sac_new_habitat_75 <- tibble(
  date = kwk_usgs$Date, 
  flow = kwk_usgs$Flow,
  sed_transport = upper_sac_cfs_sed_daily_func(kwk_usgs$Flow)
) %>% 
  mutate(
    sed_transport = ifelse(is.na(sed_transport), 0, sed_transport), 
    add_gravel = 0,
    starting_vol = starting_vol,
    current_vol_min = as.numeric(accumulate2(sed_transport[2:n()], add_gravel[2:n()], ~..1 - ..2 + ..3, .init = starting_vol[1]-sed_transport[1])),
  ) %>% 
  mutate(
    square_meters = (current_vol_min / (2 / 3.281)) 
  ) %>% 
  select(date, habitat = square_meters) %>% 
  mutate(type = "New Method 75%") %>% 
  filter(date >= "1980-01-01", date <= "2001-01-01")

# 30 %
upper_sac_new_habitat_30 <- tibble(
  date = kwk_usgs$Date, 
  flow = kwk_usgs$Flow,
  sed_transport = upper_sac_cfs_sed_daily_func_30(kwk_usgs$Flow)
) %>% 
  mutate(
    sed_transport = ifelse(is.na(sed_transport), 0, sed_transport), 
    add_gravel = 0,
    starting_vol = starting_vol,
    current_vol_min = as.numeric(accumulate2(sed_transport[2:n()], add_gravel[2:n()], ~..1 - ..2 + ..3, .init = starting_vol[1]-sed_transport[1])),
  ) %>% 
  mutate(
    square_meters = (current_vol_min / 3.281 / 2) 
  ) %>% 
  select(date, habitat = square_meters) %>% 
  mutate(type = "New Method 30%") %>% 
  filter(date >= "1980-01-01", date <= "2001-01-01")

# 25 %
upper_sac_new_habitat_25 <- tibble(
  date = kwk_usgs$Date, 
  flow = kwk_usgs$Flow,
  sed_transport = upper_sac_cfs_sed_daily_func_25(kwk_usgs$Flow)
) %>% 
  mutate(
    sed_transport = ifelse(is.na(sed_transport), 0, sed_transport), 
    add_gravel = 0,
    starting_vol = starting_vol,
    current_vol_min = as.numeric(accumulate2(sed_transport[2:n()], add_gravel[2:n()], ~..1 - ..2 + ..3, .init = starting_vol[1]-sed_transport[1])),
  ) %>% 
  mutate(
    square_meters = (current_vol_min / 3.281 / 2) 
  ) %>% 
  select(date, habitat = square_meters) %>% 
  mutate(type = "New Method 25%") %>% 
  filter(date >= "1980-01-01", date <= "2001-01-01")

upper_sac_new_habitat_30 %>% 
  mutate(change_in_habitat = (habitat - lag(habitat)) / lag(habitat)) %>%   
  group_by(year = year(date), month = month(date)) %>% 
  summarise(total = sum(change_in_habitat)) %>% 
  ungroup() %>% 
  mutate(total_diff = lag(total) - total)

habitat_change <- upper_sac_new_habitat_75 %>% 
  mutate(month = month(date), year = year(date)) %>% 
  group_by(year, month) %>% 
  summarise(month_avg = min(habitat)) %>% 
  ungroup() %>% 
  mutate(prop_change = (month_avg - lag(month_avg)) / lag(month_avg),
         prop_change = prop_change * 100,
         month_name = month.abb[month])

habitat_change <- upper_sac_new_habitat_75 %>% 
  select(date, habitat) %>% 
  mutate(habitat_change = (habitat - lag(habitat))/lag(habitat), 
         habitat_change = habitat_change * 100) %>% 
  group_by(year = year(date), month = month(date)) %>% 
  summarise(
    prop_change = min(habitat_change)
  ) %>% ungroup() %>% 
  mutate(month_name = month.abb[month])

habitat_change_75 <- upper_sac_new_habitat_75 %>% 
  select(date, habitat) %>% 
  mutate(first_amount = first(habitat), 
         prop_change = (habitat - first_amount)/first_amount) %>% 
  group_by(year = year(date), month = month(date)) %>% 
  summarise(
    prop_change = min(prop_change)
  ) %>% ungroup() %>% 
  mutate(month_name = month.abb[month])

habitat_change_30 <- upper_sac_new_habitat_30 %>% 
  select(date, habitat) %>% 
  mutate(first_amount = first(habitat), 
         prop_change = (habitat - first_amount)/first_amount) %>% 
  group_by(year = year(date), month = month(date)) %>% 
  summarise(
    prop_change = min(prop_change)
  ) %>% ungroup() %>% 
  mutate(month_name = month.abb[month])

habitat_change_25 <- upper_sac_new_habitat_25 %>% 
  select(date, habitat) %>% 
  mutate(first_amount = first(habitat), 
         prop_change = (habitat - first_amount)/first_amount) %>% 
  group_by(year = year(date), month = month(date)) %>% 
  summarise(
    prop_change = min(prop_change)
  ) %>% ungroup() %>% 
  mutate(month_name = month.abb[month])

amount_decayed_75 <- upper_sac_no_decay %>% 
  left_join(habitat_change_75, 
            by = c("year"="year", "month"="month_name")) %>% 
  filter(year >= 1980) %>% 
  mutate(amount_decrease = habitat * prop_change, 
         habitat_decayed = habitat + amount_decrease, 
         method = "75%")

amount_decayed_30 <- upper_sac_no_decay %>% 
  left_join(habitat_change_30, 
            by = c("year"="year", "month"="month_name")) %>% 
  filter(year >= 1980) %>% 
  mutate(amount_decrease = habitat * prop_change, 
         habitat_decayed = habitat + amount_decrease, 
         method = "30%")
  
amount_decayed_25 <- upper_sac_no_decay %>% 
  left_join(habitat_change_25, 
            by = c("year"="year", "month"="month_name")) %>% 
  filter(year >= 1980) %>% 
  mutate(amount_decrease = habitat * prop_change, 
         habitat_decayed = habitat + amount_decrease, 
         method = "25%")

decay_df <- bind_rows(
  amount_decayed_25, 
  amount_decayed_30, 
  amount_decayed_75
)

decay_all_plot <- decay_df %>% 
  ggplot() + 
  geom_line(aes(date, habitat_decayed, color=method)) + 
  geom_line(aes(date, habitat), color = "purple") 

decay_25_plot <- amount_decayed_25 %>% 
  ggplot() + 
  geom_line(aes(date, habitat_decayed), color = "blue") + 
  geom_line(aes(date, habitat), color = "green") + 
  labs(title = "Threshold = 25%")

upper_sac_habitat <- bind_rows(
  upper_sac_current, 
  upper_sac_no_decay, 
  upper_sac_new_habitat_75
  # , 
  # upper_sac_new_habitat_30, 
  # upper_sac_new_habitat_75
) 


upper_sac_habitat %>% 
  # filter(type != "no decay") %>% 
  ggplot(aes(date, habitat, color = type)) + geom_line()


# how do we go from sediment transport capacities to a habitat amount

# run calsim flows through the habitat curves from the model report
upper_sac_habitat_func(DSMflow::flows_cfs$`Upper Sacramento River`)


kwk_plot <- kwk_usgs %>% 
  ggplot(aes(Date, Flow)) + geom_line()

gridExtra::grid.arrange(decay_25_plot, kwk_plot, nrow=2)




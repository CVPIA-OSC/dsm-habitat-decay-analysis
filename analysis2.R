library(tidyverse)
library(lubridate)
library(gridExtra)
# combine the habitat curves 
habitat <- read_csv("data-raw/chinook_habitat_decay_graph.csv", 
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


# get another way to estimate this value
starting_vol <- 66.9 * (43560) * 2

kwk_data <- CDECRetrieve::cdec_query("kwk", "20", "h", "1994-07-01",  "2022-03-18")
kwk_usgs <- dataRetrieval::readNWISdv("11370500", parameterCd = "00060", startDate = "1980-01-01", 
                                      endDate = "2000-01-01") %>% 
  dataRetrieval::renameNWISColumns() %>% 
  as_tibble()
# start the algorithm here:
kwk_daily <- kwk_data %>% 
  mutate(parameter_value = ifelse(parameter_value < 0, NA_real_, parameter_value)) %>% 
  group_by(date = as_date(datetime)) %>% 
  summarise(
    mean_flow_cfs = mean(parameter_value, na.rm = TRUE)
  ) %>% 
  mutate(mean_flow_cfs = ifelse(is.nan(mean_flow_cfs), NA_real_, mean_flow_cfs), 
         flow_day = mean_flow_cfs * 86400)

kwk_usgs_daily <- kwk_usgs %>% 
  mutate(flow_day = Flow * 86400)

kwk_transport <- kwk_usgs_daily %>% 
  mutate(
    sed_transport = upper_sac_func(flow_day)
  ) %>% as_tibble()

gravel_augs <- tibble(
  Date = seq.Date(as_date("1980-01-01"), as_date("2000-01-01"), by="1 day")
) 

d <- kwk_transport %>% 
  filter(year(Date) %in% 1980:1985) %>% 
  mutate(sed_transport = ifelse(is.na(sed_transport), 0, sed_transport), 
         sed_transport_accum = cumsum(sed_transport), 
         start_vol = starting_vol,
         current_vol = start_vol - sed_transport_accum)

grid.arrange(d %>% ggplot(aes(Date, Flow)) + geom_line() + labs(title = "Keswick 1987 using Max Scale Down for 40mm"), 
             d %>% ggplot(aes(Date, current_vol)) + geom_line() + geom_hline(yintercept = 0), 
             nrow = 2)


x <- rep(starting_vol, length(d$sed_transport))
a <- x - cumsum(d$sed_transport * 60 * 60 * 24)
plot(x = 1:length(a), y = a)

upper_sac_area %>% 
  ggplot(aes(flow, total_volume)) + geom_line()

# Create a daily time series that subtracts capacity from supply 
# each day and then lowers the supply for the next day in the time series




# make sure to capture the sediment that get redisitributed this is not 
# a loss, 

# create relationship that is flow -> destrivution 

# scale 1: use the 40mm proportion to scale the sediment transport curve
# scale 2: only 75% is continue down the reach, so scale down by .75




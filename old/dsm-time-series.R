library(tidyverse)
library(lubridate) 

calib_scaledown <- 0.6056206
calib_scaledown <- 0.3

files_to_read <- list.files("data-raw/CrossSectionFluxes/SedimentRatingCurves/",
                            pattern = ".txt",
                            full.names = TRUE)

d50_scaledown <- read_csv("data-raw/sediment-prop-move.csv")

d50_scaledown_summarized <- d50_scaledown %>% 
  mutate(flow_cfday = flow_m3s * 35.315 * 86400) %>% 
  group_by(flow_cfday) %>% 
  summarise(
    min_fraction = min(fraction), 
    avg_fraction = mean(fraction),
    max_fraction = max(fraction)
  ) %>% 
  mutate(flow_cfs = flow_cfday / 86400)


rating_curves_by_rm <- map_df(files_to_read, function(x) {
  river_mile <- str_match(x, "[0-9]+\\.?[0-9]+")[,1]
  read_tsv(x, skip = 1, col_names = c("flow", "parker_qs", "wilcock_qs", "gaeuman_qs")) %>%
    mutate(river_mile = as.numeric(river_mile))
})

rating_curve <- rating_curves_by_rm %>% 
  rename(flow_cms = flow) %>% 
  pivot_longer(parker_qs:gaeuman_qs, names_to = "curve", values_to = "transport_m3_per_second") %>% 
  mutate(
    flow_cfs = 35.315 * flow_cms,
    flow_cfd = flow_cfs * 86400, # cubic feet per day
    transport_ft3_per_second = 35.315 * transport_m3_per_second,
    transport_ft3_per_day = 86400 * transport_ft3_per_second # transport per day
  ) %>% 
  group_by(flow_cfs, river_mile) %>% 
  summarise(
    sed_ft3_per_second_min = min(transport_ft3_per_second), 
    sed_ft3_per_second_avg = mean(transport_ft3_per_second),
    sed_ft3_per_second_max = max(transport_ft3_per_second),    
    sed_ft3_per_day_min = min(transport_ft3_per_day), 
    sed_ft3_per_day_avg = mean(transport_ft3_per_day),
    sed_ft3_per_day_max = max(transport_ft3_per_day)
  ) %>% 
  ungroup() %>% 
  group_by(flow_cfs) %>% 
  summarise(
    sed_ft3_per_second_min = mean(sed_ft3_per_second_min), 
    sed_ft3_per_second_avg = mean(sed_ft3_per_second_avg),
    sed_ft3_per_second_max = mean(sed_ft3_per_second_max),
    sed_ft3_per_day_min = mean(sed_ft3_per_day_min), 
    sed_ft3_per_day_avg = mean(sed_ft3_per_day_avg),
    sed_ft3_per_day_max = mean(sed_ft3_per_day_max),
  ) %>%
  ungroup() %>% 
  mutate(flow_cfd = flow_cfs * 86400)


flow_cfs_to_sed_cfd_calibrated <- approxfun(
  x = rating_curve$flow_cfs, 
  y = rating_curve$sed_ft3_per_day_min * rep(calib_scaledown, 
                                             length(rating_curve$flow_cfs)) * 
    d50_scaledown_summarized$avg_fraction, 
  rule = 1
)

dsm_flows <- DSMflow::flows_cfs |> 
  select(date, flow = `Upper Sacramento River`) |> 
  filter(year(date) %in% 1979:2000)

# calculate the daily decay by applying flows to the flow->sed func
upper_sac_decay <- tibble(
  date = dsm_flows$date,
  decay_amount = ifelse(is.na(
    (x <- flow_cfs_to_sed_cfd_calibrated(dsm_flows$flow))), 
    0, x
  )
)

upper_sac_decay |> 
  ggplot(aes(date, decay_amount)) + geom_line()

monthly_decay <- upper_sac_decay |> 
  transmute(
    date,
    month_decay_sqf = (decay_amount * days_in_month(month(date)))/2,
    month_decay_acres = month_decay_sqf / 43560
  ) |> 
  filter(year(date) >= 1979) |> 
  left_join(dsm_flows)


# accum_prod <- prop_hab_decay |> 
#   transmute(
#     date,
#     month_decay_area = ((mnth_decay / 2) / 10.764) / 4047,
#     prop_decay = month_decay_area / 49.9069, 
#     prop_avail = 1 - prop_decay, 
#     accum_prod = cumprod(prop_avail)
#   ) |> 
#   left_join(dsm_flows) 


# accum_prod |> 
#   ggplot(aes(date, accum_prod)) + geom_line()
  

write_csv(prop_hab_decay, "data/prop-of-habitat-decay.csv")

prop_hab_decay |> 
  ggplot(aes(date, accum_decay)) + geom_line()

DSMhabitat::upper_sacramento_river_instream

DSMhabitat::fr_spawn["Upper Sacramento River",,]

microbenchmark::microbenchmark(
  a =  {
    ifelse(is.na(
      (x <- flow_cfs_to_sed_cfd_calibrated(dsm_flows$`Upper Sacramento River`))), 
      0, x
    )
  }, 
  b = {
    ifelse(is.na(flow_cfs_to_sed_cfd_calibrated(dsm_flows$`Upper Sacramento River`)), 
      0, 
      flow_cfs_to_sed_cfd_calibrated(dsm_flows$`Upper Sacramento River`)
    )
  }
)


habitats <- list(
  spawning_habitat =           fallRunDSM::params$spawning_habitat,
  inchannel_habitat_fry =      fallRunDSM::params$inchannel_habitat_fry,
  inchannel_habitat_juvenile = fallRunDSM::params$inchannel_habitat_juvenile,
  floodplain_habitat =         fallRunDSM::params$floodplain_habitat,
  weeks_flooded =              fallRunDSM::params$weeks_flooded
)

upsac_spawning_base <- 
  habitats$spawning_habitat["Upper Sacramento River",,] |> 
  as_tibble() |> 
  mutate(month = 1:12, 
         month_label = month.abb) |> 
  pivot_longer(names_to = "year", values_to = "spawning_habitat", 
               `1979`:`2000`) |> 
  mutate(year = as.numeric(year), 
         date = lubridate::as_date(paste0(year, "-", month, "-", days_in_month(month)))) |> 
  arrange(date) |> 
  filter(year(date) >= 1979) |> 
  mutate(spawning_acres = spawning_habitat / 4047)

upsac_spawning_base |> 
  ggplot(aes(spawning_acres)) + geom_density()



# new idea 
mean(upsac_spawning_base$spawning_acres)









upsac_spawning_base |> 
  ggplot(aes(date, spawning_habitat)) + geom_line()

upsac_spawning_base |>
  left_join(prop_hab_decay, by = c("date" = "date")) |> 
  mutate(decay_spawning = spawning_habitat * accum_decay) |> 
  ggplot() + 
  geom_line(aes(date, decay_spawning, color = "decayed")) + 
  geom_line(aes(date, spawning_habitat, color = "base"))


# augmentations 

sac_augs <- read_csv("data/sacramento_river_gravel_augmentation_data.csv")

sac_aug_totals <-
  sac_augs |> 
  group_by(date) |> 
  summarise(tons = sum(tons)) |> 
  mutate(
    date = as_date(paste0(date, "-01-31")),
    cubic_yard = tons / 1.5, 
    cubic_feet = cubic_yard * 27,
    sq_feet = cubic_feet / 2, 
    acres = sq_feet / 43560
  ) |> 
  select(date, tons, acres) |> 
  bind_rows(
    tibble(date = seq(as_date("1980-01-31"), as_date("1996-01-31"), by="1 year"), 
           acres = 5)
  ) |> 
  arrange(date) 


mean(sac_aug_totals$acres, na.rm = TRUE)


# time series creation ---------------------

# 50 acres
start_hab_acres <- mean(DSMhabitat::fr_spawn["Upper Sacramento River",,]/4047)

# get corresponding augs and decays 
decays <- monthly_decay |> select(date, month_decay_acres, flow)
augmentations <- sac_aug_totals |> select(date, aug_acres=acres)

decays_and_augs <- decays |> 
  left_join(augmentations, by=c("date"="date")) |> 
  mutate(aug_acres = ifelse(is.na(aug_acres), 0, aug_acres), 
         aug_minus_decay = aug_acres - month_decay_acres, 
         month = month(date),
         year = year(date))

sum(decays_and_augs$aug_minus_decay)

decays_and_augs |> 
  ggplot(aes(date, aug_minus_decay)) + geom_line()


habitat_change <- numeric(nrow(decays_and_augs))
habitat_change[1] <- start_hab_acres + decays_and_augs$aug_minus_decay[1] 

for (i in 2:length(habitat_change)) {
  habitat_change[i] <- habitat_change[i-1] + decays_and_augs$aug_minus_decay[i]
}

decays_and_augs$habitat_change <- habitat_change

decays_and_augs |> 
  ggplot(aes(date, habitat_change)) + geom_line()


# d <- 
#   DSMhabitat::set_spawning_habitat(watershed = "Upper Sacramento River", 
#                                  species = "fr", 
#                                  month = 9,
#                                  flow = decays_and_augs$flow[9]) / 4047

sac_habitat_acres <- DSMhabitat::fr_spawn["Upper Sacramento River",,]/4047

# 
habitat_change * (sac_habitat_acres / start_hab_acres)

decays_and_augs |> 
  group_by(year) |> 
  summarise(
    total = sum(month_decay_acres)
  )
  

upsac_spawning_base |> 
  select(date, month, year, spawning_acres) |> 
  mutate(prop_area = spawning_acres / start_hab_acres) |> 
  left_join(
    decays_and_augs, 
    by = c("year"="year", "month"="month"))


tibble(flow_cfs = c(1000, 1500, 2000), area = c(40, 50, 60)) %>% 
  mutate(prop_area = area/50)


kwk <- CDECRetrieve::cdec_query("kwk", sensor_num = "20", "1997-01-01", "2021-01-01", dur_code = "h")

kwk_flows_metrics <- kwk |> 
  group_by(year = year(datetime)) |> 
  summarise(
    max_flow = max(parameter_value, na.rm = TRUE),
    mean_flow = mean(parameter_value, na.rm = TRUE),
    median_flow = median(parameter_value, na.rm = TRUE),
  ) |> ungroup()

kwk |> 
  filter(year(datetime) == 1997) |> 
  ggplot(aes(datetime, parameter_value)) + geom_line()

sac_augs_annual |> 
  left_join(kwk_flows_metrics) |> 
  ggplot(aes(max_flow, tons)) + geom_point()

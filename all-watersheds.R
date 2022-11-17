library(tidyverse)
library(gridExtra)
library(lubridate)

gravel_size_scaledown <- read_csv("data/sediment-prop-move.csv")

gravel_size_scaledown_summarized <- gravel_size_scaledown |> 
  mutate(
    flow_cfs = flow_m3s * 35.315,
    flow_cfday = flow_cfs * 86400) |> 
  group_by(flow_cfs) |> 
  summarise(
    min_fraction = min(fraction), 
    avg_fraction = mean(fraction),
    max_fraction = max(fraction)
  ) 

write_rds(gravel_size_scaledown_summarized, "data/gravel-size-scaledowns.rds")


rating_curve <- read_rds("data/rating-curves-with-threshold-of-movement.rds")

MIN_flow_cfs_to_sed_cfd_final <- approxfun(
  x = rating_curve$flow_cfs, 
  y = rating_curve$sed_ft3_per_day_min * 
    gravel_size_scaledown_summarized$avg_fraction * 
    .075
)

AVG_flow_cfs_to_sed_cfd_final <- approxfun(
  x = rating_curve$flow_cfs, 
  y = rating_curve$sed_ft3_per_day_avg * 
    gravel_size_scaledown_summarized$avg_fraction * 
    .075
)

MAX_flow_cfs_to_sed_cfd_final <- approxfun(
  x = rating_curve$flow_cfs, 
  y = rating_curve$sed_ft3_per_day_max * 
    gravel_size_scaledown_summarized$avg_fraction * 
    .075
)


watersheds_with_decay <- read_csv("data/watersheds-with-decay.csv")


dsm_flows <- DSMflow::flows_cfs$biop_2008_2009 |> 
  # filter(year(date) %in% 1979:2000) |> 
  pivot_longer(names_to = "watershed", values_to = "flow_cfs", -date) |> 
  filter(watershed %in% watersheds_with_decay$watershed)

# Exceedance probs --------------------------------------

exceedance_curves <- map(watersheds_with_decay$watershed, function(w) {
  d <- dsm_flows |> filter(watershed == w) |> 
    mutate(cume_dist = dplyr::cume_dist(-flow_cfs)) |> 
    arrange(desc(cume_dist))
    
  approxfun(x = d$cume_dist, y = d$flow_cfs)
}) |> 
  set_names(watersheds_with_decay$watershed)


exceedance_curves$`Upper Sacramento River`(.07919235)
exceedance_curves$`Upper Sacramento River`(.07919235)


upper_sac_exceedance_at_1800 <- 0.0569578

watershed_offsets <- map_dbl(watersheds_with_decay$watershed, function(w) {
  exceedance_curves[[w]](upper_sac_exceedance_at_1800)
}) |> set_names(watersheds_with_decay$watershed)

watershed_decays <- map2(watersheds_with_decay$watershed, watershed_offsets, function(w, x) {
  dsm_flows |>
    filter(watershed == w, year(date) %in% 1979:2000) |> 
    mutate(flow_adjusted = flow_cfs - x, 
           decay_min = MIN_flow_cfs_to_sed_cfd_final(flow_adjusted), 
           decay_avg = AVG_flow_cfs_to_sed_cfd_final(flow_adjusted), 
           decay_max = MAX_flow_cfs_to_sed_cfd_final(flow_adjusted) 
           ) |> 
    pivot_longer(names_to = "decay_type", values_to = "decay_amount", -c(date, watershed, 
                                                                         flow_cfs, flow_adjusted)) |> 
    mutate(decay_cfd = ifelse(is.na(decay_amount), 0, decay_amount), 
           decay_cfm = decay_cfd * days_in_month(month(date)),
           decay_sqm = decay_cfm / 2, 
           decay_acres_month = decay_sqm / 43560) |> 
    select(date, watershed, flow_cfs, decay_type, decay_acres_month)
}) |> 
  set_names(watersheds_with_decay$watershed)


write_rds(watershed_decays, "data/watershed-decay-acres.rds")


plot_instant_decay <- map(watershed_decays, function(w) {
  w |> 
    ggplot(aes(date, decay_acres_month, color = decay_type)) + geom_line() + 
    labs(title = head(w, 1)$watershed)
}) |> 
  set_names(watersheds_with_decay$watershed)

plot_accum_decay <- map(watershed_decays, function(w) {
  w |> 
    group_by(decay_type) |> 
    mutate(accum_decay = 0 - cumsum(decay_acres_month)) |> 
    ungroup() |> 
    ggplot(aes(date, accum_decay, color = decay_type)) + geom_line() + 
    labs(title = head(w, 1)$watershed, y = "Acres")
}) |> 
  set_names(watersheds_with_decay$watershed)


plot_accum_decay$`American River` + 
  labs(x = "", y = "Accumulated Decay (acres)", color = "Transport Curve")


summary_stats <- map_df(watershed_decays[-1], function(w) {
  watershed <- distinct(w, watershed) |> pull()
  w |> 
    group_by(year = year(date), decay_type) |> 
    summarise(
      total = sum(decay_acres_month)
    ) |> 
    ungroup() |> 
    group_by(decay_type) |> 
    summarise(
      avg_decay = mean(total)
    ) |> 
    ungroup() |> 
    mutate(
      watershed = watershed
    )
})

# summary_stats |> 
#   select(watershed, decay_type, avg_decay) |> 
#   mutate(avg_decay = round(avg_decay, 5)) |> 
#   pivot_wider(names_from = "decay_type", values_from = "avg_decay") |> 
#   select(watershed, decay_min, decay_avg, decay_max) |> 
#   mutate(
#     incipient_motion_flow = watershed_offsets[watershed]
#   ) |> 
#   write_csv("data/all-watershed-decay-summary.csv")


# Calaveras, Mokelumne, Stanislaus, Tuolumne, and Merced ------------------
summary_stats |> 
  select(watershed, decay_type, avg_decay) |> 
  mutate(avg_decay = round(avg_decay, 5)) |> 
  pivot_wider(names_from = "decay_type", values_from = "avg_decay") |> 
  select(watershed, decay_min, decay_avg, decay_max) |> 
  mutate(
    incipient_motion_flow = watershed_offsets[watershed]
  ) |> 
  filter(watershed %in% c(
    "Calaveras River", "Mokelumne River", "Stanislaus River", "Tuolumne River", "Merced River"
  ))


plot_accum_decay$`Calaveras River`
plot_accum_decay$`Mokelumne River`
plot_accum_decay$`Stanislaus River`
plot_accum_decay$`Tuolumne River`
plot_accum_decay$`Merced River`

plot_instant_decay$`Merced River`


# Yuba River ------------------
summary_stats |> 
  select(watershed, decay_type, avg_decay) |> 
  mutate(avg_decay = round(avg_decay, 5)) |> 
  pivot_wider(names_from = "decay_type", values_from = "avg_decay") |> 
  select(watershed, decay_min, decay_avg, decay_max) |> 
  mutate(
    incipient_motion_flow = watershed_offsets[watershed]
  ) |> 
  filter(watershed %in% c(
    "Yuba River"
  ))


plot_accum_decay$`Yuba River`

plot_instant_decay$`Yuba River`


# Feather River ------------------
summary_stats |> 
  select(watershed, decay_type, avg_decay) |> 
  mutate(avg_decay = round(avg_decay, 5)) |> 
  pivot_wider(names_from = "decay_type", values_from = "avg_decay") |> 
  select(watershed, decay_min, decay_avg, decay_max) |> 
  mutate(
    incipient_motion_flow = watershed_offsets[watershed]
  ) |> 
  filter(watershed %in% c(
    "Feather River"
  ))


plot_accum_decay$`Feather River`

plot_instant_decay$`Feather River`



# American 


summary_stats |> 
  select(watershed, decay_type, avg_decay) |> 
  mutate(avg_decay = round(avg_decay, 5)) |> 
  pivot_wider(names_from = "decay_type", values_from = "avg_decay") |> 
  select(watershed, decay_min, decay_avg, decay_max) |> 
  mutate(
    incipient_motion_flow = watershed_offsets[watershed]
  ) |> 
  filter(watershed %in% c(
    "American River"
  ))

plot_accum_decay$`American River`

plot_instant_decay$`American River`









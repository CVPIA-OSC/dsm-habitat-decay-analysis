library(tidyverse)

# Read Data  -------------------------------------------------------------------
res <- read_csv("data/rating-curves-compiled.csv")

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

d50_scaledown_summarized %>% 
  gather(stat, value, min_fraction:max_fraction) %>%  
  ggplot(aes(flow_cfs, value, color = stat)) + geom_line() + 
  scale_x_continuous(labels = scales::comma, breaks = c(8000, 20000, 40000, 60000, 80000))



# Rating Curves -----------------------------------------------------------

rating_curves <- res %>% gather(curve, sediment_transport, parker_qs:gaeuman_qs) %>% 
  mutate(
    sediment_transport = sediment_transport *  35.315 * 86400, # cf per day conversion
    flow = flow * 35.315 * 86400 # cfs per day conversion
  )

# this curve will be used for plots, its prefered to show the average daily cfs in the x-axis
# and the total daily transport that results from that on the y-axis
rating_cfs_to_sed_daily <- res %>% gather(curve, sediment_transport, parker_qs:gaeuman_qs) %>% 
  mutate(
    sediment_transport = sediment_transport *  35.315 * 86400, 
    flow = flow * 35.315
  )

upper_sac_rating_curve <- rating_curves %>% 
  group_by(river_mile, flow) %>% 
  summarise(min = min(sediment_transport), 
            avg = mean(sediment_transport), 
            max = max(sediment_transport)) %>% 
  ungroup() %>% 
  group_by(flow) %>% 
  summarise(
    avg_min = median(min), 
    avg_avg = median(avg), 
    avg_max = median(max)
  )

# using the median since the distribution for these are very skewed
upper_sac_rating_curve_cfs_to_sed_daily <- rating_cfs_to_sed_daily %>% 
  group_by(river_mile, flow) %>% 
  summarise(min = min(sediment_transport), 
            avg = mean(sediment_transport), 
            max = max(sediment_transport)) %>% 
  ungroup() %>% 
  group_by(flow) %>% 
  summarise(
    avg_min = median(min), 
    avg_avg = median(avg), 
    avg_max = median(max)
  )



upper_sac_func <- approxfun(upper_sac_rating_curve$flow, 
                            upper_sac_rating_curve$avg_min * 
                              d50_scaledown_summarized$max_fraction * 
                              rep(.75, length(upper_sac_rating_curve$avg_avg)))

upper_sac_cfs_sed_daily_func <- approxfun(upper_sac_rating_curve_cfs_to_sed_daily$flow, 
                                          upper_sac_rating_curve_cfs_to_sed_daily$avg_avg * 
                                            d50_scaledown_summarized$max_fraction * 
                                            rep(.75, length(upper_sac_rating_curve_cfs_to_sed_daily$avg_avg)))


rating_curves %>% 
  mutate(flow = flow / (60 * 60 * 24)) %>% 
  group_by(river_mile, flow) %>% 
  summarise(min = min(sediment_transport), 
            avg = mean(sediment_transport), 
            max = max(sediment_transport)) %>% 
  ungroup() %>% 
  group_by(flow) %>% 
  summarise(
    avg_min = mean(min), 
    avg_avg = mean(avg), 
    avg_max = mean(max)
  ) %>% 
  ggplot() + 
  geom_line(aes(x = flow,y = avg_min, color = "min")) + 
  geom_line(aes(x = flow,y = avg_avg, color = "mean")) + 
  geom_line(aes(x = flow,y = avg_max, color = "max")) + 
  labs(y = "cubic feet per day", 
       x = "flow (cfd)") + 
  scale_y_continuous(labels = scales::comma) + 
  scale_x_continuous(labels = scales::comma) 


# Plots -------------------------------------------------------------------

flows <- seq(7000, 80000, by = 1000)


upper_sac_rating_curve_df <- tibble(
  flow = flows, 
  sed_transport = upper_sac_cfs_sed_daily_func(flow)
)

p1 <- upper_sac_rating_curve_df %>% 
  ggplot(aes(flow, sed_transport)) + 
  geom_line(size=1.25)  + 
  scale_x_continuous(breaks = c(8000, 20000, 40000, 60000, 80000), 
                     labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) + 
  labs(x = "Average Daily Flow (cfs)", 
       y = "Sediment Transport Capacity (cfd)")


p2 <- kwk_transport %>% 
  ggplot(aes(Flow)) + geom_histogram()

grid.arrange(p1, p2, nrow = 2)




library(CDECRetrieve)
library(tidyverse)
library(lubridate)
library(fallRunDSM)
# keswick ------------------------------------------------
cdec_datasets("KWK")

kwk_flow_raw <- cdec_query("kwk", "20", "h", "1994-07-01")

kwk_flow_raw %>% 
  ggplot(aes(datetime, parameter_value)) + geom_line()

# error codes in the data
kwk_flow_raw %>% 
  filter(parameter_value < 0) %>% 
  distinct(parameter_value)

# avg daily flow as keswick
kwk_flow <- kwk_flow_raw %>% 
  mutate(
    flow_hourly = case_when(
      parameter_value <= -9997 ~ NA_real_, 
      TRUE ~ parameter_value
    )) %>% 
  group_by(date = as_date(datetime)) %>% 
  summarise(avg_daily_flow = mean(flow_hourly, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(date <= "2000-01-01") %>% 
  mutate(avg_daily_flow_cms = avg_daily_flow / 35.315)

kwk_flow_monthly <- kwk_flow %>% 
  group_by(year = year(date), month = month(date)) %>% 
  summarise(monthly_flow = mean(avg_daily_flow, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(date = as_date(paste(year, "-", month, "-", days_in_month(month)))) %>% 
  select(-year, -month)

calsim_flow <- DSMflow::flows_cfs %>% 
  select(date, monthly_flow = "Upper Sacramento River") %>% 
  filter(date >= "1994-07-01")

ggplot() + 
  geom_line(data = kwk_flow_monthly, aes(date, monthly_flow, color = "cdec"), size = 1.5) + 
  geom_line(data = calsim_flow, aes(date, monthly_flow, color = "calsim")) + 
  labs(x = "", y = "Average Monthly Flow (cfs)", title = "CALSIM vs CDEC Average Monthly Flow")

# convert daily flows to daily sediment transport
wilcock_curve <- read_csv("wilcock-curve.csv")
wilcock_func <- approxfun(x = wilcock_curve$water_q, y = wilcock_curve$total_qs, rule = 2)

gaeuman_curve <- read_csv("gaeuman-curve.csv")
gaeuman_func <- approxfun(x = gaeuman_curve$water_q, y = gaeuman_curve$total_qs, rule = 2)

parker_curve <- read_csv("parker-surve.csv")
parker_func <- approxfun(x = parker_curve$water_q, y = parker_curve$total_qs, rule = 2)

kwk_sediment_transport <- kwk_flow %>% 
  mutate(wilcock = wilcock_func(avg_daily_flow_cms), 
         gaeuman = gaeuman_func(avg_daily_flow_cms), 
         parker = parker_func(avg_daily_flow_cms)) %>% 
  gather(transport_curve, sediment_transport, wilcock:parker)

kwk_sediment_transport %>% 
  filter(date >= "1995-01-01") %>% 
  ggplot(aes(date, sediment_transport, color = transport_curve)) + geom_line() +
  scale_x_date(date_breaks = "year", date_labels = "%b/%y") + 
  labs(x = "", y = "Sediment Transport (m^3/s)", color = "Transport Rating Curve", 
       title = "Keswick (KWK) Daily Average Sediment Transport", 
       caption = "rating curves used were obtained from ACID (RM 292.428)")




# Updated Analysis 2/16 --------------------------------------------

benton_cypress <- read_csv("benton-cypress-parker-average.csv", col_names = c("flow", "sed_transport"))
benton_cypress_curve <- approxfun(x = benton_cypress$flow, y = benton_cypress$sed_transport, rule = 2)

upper_sac_flows <- DSMflow::flows_cfs %>% 
  select(date, flow = `Upper Sacramento River`) %>% 
  filter(year(date) %in% 1980:2000)

upper_sac_transport_cap <- benton_cypress_curve(upper_sac_flows$flow)

# suit curves
keswich_to_benton_suit <- 
  read_csv("keswick-to-benton-suit-curve.csv", col_names = c("flow_m3s", "area_m2")) %>% 
  mutate(reach = "Keswick to Benton")
benton_to_cypress <- 
  read_csv("benton-to-cypress-suit-curve.csv", col_names = c("flow_m3s", "area_m2")) %>% 
  mutate(reach = "Benton to Cypress")
cypress_to_bonnyview <- 
  read_csv("cypress-to-bonnyview-suit-curve.csv", col_names = c("flow_m3s", "area_m2")) %>% 
  mutate(reach = "Cypress to Bonnyview")
bonnyview_to_clear <- 
  read_csv("bonnyview-to-clear-suit-curve.csv", col_names = c("flow_m3s", "area_m2")) %>% 
  mutate(reach = "Bonnyview to Clear")

suit_curves <- bind_rows(
  keswich_to_benton_suit, 
  benton_to_cypress, 
  cypress_to_bonnyview, 
  bonnyview_to_clear
)


# need to make sure that the flow values line up so that we can sum the areas
# to solve this problem I will just create approxfuns
flow_inputs <- seq(100, 2500, by = 150)

keswich_to_benton_curve <- approxfun(keswich_to_benton_suit$flow_m3s, 
                                     keswich_to_benton_suit$area_m2, rule = 2)(flow_inputs)

benton_to_cypress_curve <- approxfun(benton_to_cypress$flow_m3s, 
                                     benton_to_cypress$area_m2, rule = 2)(flow_inputs)

cypress_to_bonnyview_curve <- approxfun(cypress_to_bonnyview$flow_m3s, 
                                        cypress_to_bonnyview$area_m2, rule = 2)(flow_inputs)

bonnyview_to_clear_curve <- approxfun(bonnyview_to_clear$flow_m3s, 
                                      bonnyview_to_clear$area_m2, rule = 2)(flow_inputs)





suit_curves <- tibble(
  flow = rep(flow_inputs, 4), 
  area = c(keswich_to_benton_curve, benton_to_cypress_curve, 
           cypress_to_bonnyview_curve, bonnyview_to_clear_curve), 
  reach = rep(c("Keswick to Benton", "Benton to Cypress", 
                "Cypress to Bonnyview", "Bonnyview to Clear"), each = length(flow_inputs))
)

suit_curves %>% 
  group_by(flow) %>% 
  summarise(total_area = sum(area)) %>% 
  mutate(total_volume = total_area * 0.6096)

suit_curves %>% 
  ggplot(aes(flow, area, color = reach)) + geom_line()


average_cap <- tibble(flow=flow_inputs, 
                      volume_cap = benton_cypress_curve(flow_inputs)
                      )


average_cap %>% 
  ggplot(aes(flow, volume_cap)) + geom_line() + geom_point()


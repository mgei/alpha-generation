library(tidyverse)
library(tidyquant)
library(lubridate)
library(RcppRoll)

etf_prices <- readRDS("data/etf_prices.RDS")

strategy <- etf_prices %>% 
  group_by(symbol) %>% 
  mutate(SMA = roll_mean(adjusted, n = 200, align = "right", fill = NA), 
         r = adjusted/lag(adjusted)-1) %>% 
  ungroup() %>% 
  mutate(weight = as.double(lag(adjusted>SMA)),
         r_strategy = r * weight) %>% 
  group_by(date) %>% 
  summarise(r = mean(r_strategy, na.rm = T))

strategy %>% 
  mutate(series = "GTAA") %>% 
  bind_rows(etf_prices %>% 
              filter(symbol == "SPY") %>% 
              transmute(date, r = adjusted/lag(adjusted)-1, series = "SPY")) %>% 
  filter(!is.nan(r), !is.na(r)) %>% 
  group_by(series) %>% 
  mutate(p = cumprod(r+1)-1) %>% 
  ggplot(aes(x = date, y = p, color = series)) +
  geom_line()
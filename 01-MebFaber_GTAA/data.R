library(tidyverse)
library(tidyquant)

## ETFs

# US Stocks: SPY
# Foreign stocks: EFA
# Treasury: IEF
# Commodities: GSG
# Real estate: IYR
# Gold: GLD

etf_prices <- tq_get(c("SPY", "EFA", "IEF", "GSG", "IYR", "GLD"), from = as.Date("1990-01-01"))

etf_prices %>% saveRDS("data/etf_prices.RDS")

etf_prices %>% 
  ggplot(aes(x = date, y = adjusted, color = symbol)) +
  geom_line(size = 0.2) +
  facet_wrap(~symbol, scale = "free_y") +
  theme_bw() +
  theme(legend.position = "none")

# 3/6/9/12 month SMA --> 10 m SMA monthly


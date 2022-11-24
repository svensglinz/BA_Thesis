# load relevant packages
library(zoo)
library(tidyverse)
library(scales)
library(ggsci)

# import written functions and store master sheet in memory
source("functions.R")
master <- read_master("Data/data_input.xlsx")

# define parameters
start_date <- as.Date("2007-01-01")
end_date <- as.Date("2021-01-01")

# plot graph
daily_returns <-
  master$returns |>
  filter(INST == "FESX") |>
  filter(between(DATE, start_date, end_date)) |>
  arrange(desc(DATE)) |>
  ggplot(aes(x = DATE, y = exp(LOG_RET) - 1)) +
  geom_line() +
  scale_y_continuous(
    breaks = seq(from = -0.2, to = 0.2, by = 0.05),
    labels = scales::label_percent()
  ) +
  scale_x_continuous(
    breaks = seq.Date(from = start_date, to = end_date, by = "year"),
    labels = scales::label_date(format = "%Y")
  ) +
  theme_bw() +
  labs(
    title = "Daily Returns FESX",
    y = NULL,
    x = NULL
  ) +
  theme(plot.title = element_text(size = 12, face = "bold"))

ggsave("graphs/d_returns_FESX.png",
  plot = d_returns_FESX,
  device = "png", dpi = 300, height = 6.03, width = 15.9, units = "cm"
)

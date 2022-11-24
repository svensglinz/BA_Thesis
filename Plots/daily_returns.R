# load relevant packages
library(tidyverse)
library(scales)
library(ggsci)

# import written functions and store master sheet in memory
source("functions.R")
master <- read_master("Data/data_input.xlsx")

# define parameters
start_date <- as.Date("2005-01-01")
end_date <- as.Date("2021-01-01")

# filter out FESX returns
daily_returns <-
  master$returns |>
  filter(INST == "FESX") |>
  filter(between(DATE, start_date, end_date))

#plot graph
out <-
daily_returns |>
  ggplot(aes(x = DATE, y = exp(LOG_RET) - 1)) +
  geom_line() +
  scale_y_continuous(
    breaks = seq(from = -0.2, to = 0.2, by = 0.05),
    labels = scales::label_percent()
  ) +
  scale_x_continuous(
    breaks = seq.Date(from = start_date, to = end_date, by = "2 years"),
    labels = scales::label_date(format = "%Y")
  ) +
  theme_bw() +
  labs(
    title = "Daily Returns FESX",
    y = NULL,
    x = NULL
  ) +
  theme(plot.title = element_text(size = 12, face = "bold"))

ggsave("Plots/Output/daily_returns_FESX.png",
  plot = out,
  device = "png", dpi = 300, height = 6.03, width = 15.9, units = "cm"
)

# load relevant packages
library(tidyverse)
library(scales)
library(ggsci)
library(showtext)
library(showtext)

# import written functions and store master sheet in memory
source("functions.R")
master <- read_master("Data/data_input.xlsx")

# add fonts for plotting
font_add(
  family = "lmroman", regular = "Fonts/lmroman12-regular.otf",
  bold = "Fonts/lmroman12-bold.otf",
  italic = "Fonts/lmroman12-italic.otf",
  bolditalic = "Fonts/lmroman10_bolditalic.ttf"
)

showtext_auto(enable = TRUE)
showtext_opts(dpi = 300)

# define parameters
start_date <- as.Date("2005-01-01")
end_date <- as.Date("2021-01-01")

# filter out FESX returns
daily_returns <-
  master$returns |>
  filter(INST == "FESX") |>
  filter(between(DATE, start_date, end_date))

# plot graph
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
    labels = scales::label_date(format = "%y")
  ) +
  theme_bw() +
  labs(
    title = "Daily Returns FESX",
    y = NULL,
    x = NULL,
    caption = "All returns from front month contract (Expiry 0-90 days)
    Source: Eurex Clearing AG"
  ) +
  theme(
    text = element_text(family = "lmroman"),
    plot.title = element_text(size = 10, face = "bold"),
    plot.caption = element_text(size = 7),
    axis.text = element_text(size = 8),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(
      color = "darkgrey",
      linetype = "dashed", size = .3
    )
  )

ggsave("Plots/Output/daily_returns_FESX.png",
  plot = out,
  device = "png", dpi = 300, height = 6.32, width = 7.86, units = "cm",
)

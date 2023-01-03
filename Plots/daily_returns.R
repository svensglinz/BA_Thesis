# load relevant packages
library(tidyverse)
library(scales)
library(ggsci)
library(showtext)

# import written functions and store master sheet in memory
source("functions.R")
master <- read_master("Data/data_input.xlsx")

# add fonts for plotting
# add fonts for plotting
font_add(
  family = "lmroman",
  regular = "Fonts/lmroman10_regular.ttf",
  bold = "Fonts/lmroman10_bold.ttf",
  italic = "Fonts/lmroman10_italic.ttf",
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
  geom_line(size = .3) +
  scale_y_continuous(
    breaks = seq(from = -0.2, to = 0.2, by = 0.05),
    labels = scales::label_percent()
  ) +
  scale_x_date(
    breaks = seq.Date(from = start_date, to = end_date, by = "2 years"),
    labels = scales::label_date(format = "%y"),
    expand = expansion(mult = .02)
  ) +
  theme_bw() +
  labs(
    title = "Daily FESX Returns",
    y = NULL,
    x = NULL,
    subtitle = "Front month contract (Expiry 0-90 days)",
    caption = "Own Depiction | Data Source: Eurex Clearing AG, Bloomberg"
  ) +
  theme(
    text = element_text(family = "lmroman"),
    plot.title = element_text(size = 10, face = "bold"),
    plot.caption = element_text(size = 8),
    axis.text = element_text(size = 8),
    plot.margin = margin(0, 0, 0, 0),
    plot.subtitle = element_text(size = 7, face = "italic"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(
      color = "darkgrey",
      linetype = "dashed", size = .3
    )
  )

# save output
ggsave("Plots/Output/daily_returns_FESX.png",
  plot = out,
  device = "png", dpi = 300, height = 6.32,
  width = 7.86, units = "cm",
)

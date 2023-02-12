# load relevant packages
library(tidyverse)
library(scales)
library(ggsci)
library(showtext)
source("functions.R")


set_plot_theme()
master <- read_master("Data/data_input.xlsx")

# add fonts for plotting
font_add(
  family = "lmroman",
  regular = "Fonts/lmroman10_regular.ttf",
  bold = "Fonts/lmroman10_bold.ttf",
  italic = "Fonts/lmroman10_italic.ttf",
  bolditalic = "Fonts/lmroman10_bolditalic.ttf",
  symbol = "Fonts/lmroman10_math.otf"
)

showtext_auto(enable = TRUE)
showtext_opts(dpi = 600)

# define parameters
start_date <- as.Date("2005-01-01")
end_date <- as.Date("2021-01-01")

# filter out FESX returns
daily_returns <-
  master$returns |>
  filter(INST == "FESX") |>
  filter(between(DATE, start_date, end_date))

# plot graph
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
  labs(
    title = "Daily FESX Returns",
    y = NULL,
    x = NULL,
    subtitle = "Front month contract (Expiry 0-90 days)"
  )

# save output
ggsave("Plots/Output/daily_returns_FESX.png",
  plot = last_plot(), dpi = 600, height = 5,
  width = 7.86, units = "cm",
)

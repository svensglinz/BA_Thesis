# load relevant packages
library(tidyverse)
library(scales)
library(ggsci)
library(showtext)

# import written functions and store master sheet in memory
source("functions.R")
master <- read_master("Data/data_input.xlsx")

# add fonts for plotting
font_add(
  family = "lmroman",
  regular = "Fonts/lmroman10_regular.ttf",
  bold = "Fonts/lmroman10_bold.ttf",
  italic = "Fonts/lmroman10_italic.ttf",
  bolditalic = "Fonts/lmroman10_bolditalic.ttf"
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
  theme_bw() +
  labs(
    title = "Daily FESX Returns",
    y = NULL,
    x = NULL,
    subtitle = "Front month contract (Expiry 0-90 days)"
  ) +
  theme(
    text = element_text(family = "lmroman", colour = "#555555"),
    plot.subtitle = element_text(size = 8, margin = margin(0, 0, 0, b = 2)),
    plot.caption = element_text(size = 8, margin = margin(t = 5, 0, 0, 0)),
    panel.border = element_rect(colour = "#999999", fill = "transparent"),
    panel.background = element_rect(fill = "#FFFFFF", colour = "#999999", linewidth = 0),
    panel.grid.minor.y = element_line(colour = "#eeeeee", linewidth = 0.5),
    panel.grid.major = element_line(colour = "#eeeeee", linewidth = 0.5),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#F9F9F9", colour = "#CCCCCC", linewidth = 0, linetype = 1),
    axis.ticks = element_blank(),
    axis.text = element_text(size = 6),
    axis.text.y = element_text(margin = margin(0, 0, 0, 0)),
    axis.text.x = element_text(margin = margin(0, 0, 0, 0)),
    axis.title = element_text(size = 8),
    plot.title = element_text(size = 10, face = "bold", margin = margin(0, 0, 0, 0)),
    plot.margin = margin(5, 5, 5, 5),
  )

# save output
ggsave("Plots/Output/daily_returns_FESX.png",
  plot = last_plot(), dpi = 600, height = 5,
  width = 7.86, units = "cm",
)

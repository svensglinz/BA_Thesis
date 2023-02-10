# load relevant packages
library(tidyverse)
library(scales)
library(ggsci)
library(showtext)
library(latex2exp)
library(ggforce)

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

# function which calculates the 1d EWMA Volatility
fesx <- master$returns |>
  filter(INST == "FESX") |>
  select(INST, DATE, LOG_RET) |>
  arrange(desc(DATE))

fesx$VOL <- ewma_vol(fesx$LOG_RET, burn_in = 750, lambda = .96, mean = TRUE)

# define function inputs
start_date <- as.Date("2020-01-01")
end_date <- as.Date("2020-12-31")

# plot graph
fesx |>
  ggplot(aes(x = DATE, y = VOL, color = INST)) +
  geom_line(show.legend = FALSE) +
  labs(
    title = "1d Log-Ret EWMA Volatility (2020)",
    subtitle = TeX("$\\lambda$ = 0.96, burn-in = 750"),
    y = NULL,
    x = NULL,
    color = NULL
  ) +
  scale_y_continuous(
    limits = c(0, 0.04),
    breaks = seq(from = 0.01, to = 0.04, by = 0.01),
    labels = scales::label_percent()
  ) +
  scale_x_date(
    breaks = seq.Date(
      from = start_date,
      to = end_date,
      by = "month"
    ),
    limits = c(as.Date("2020-01-01"), as.Date("2020-12-31")),
    labels = scales::label_date(format = "%b"),
    expand = expansion(mult = .01)
  ) +
  theme(
    text = element_text(family = "lmroman", colour = "#555555"),
    plot.subtitle = element_text(size = 7, family = "times", margin = margin(0, 0, 0, b = 2)),
    plot.caption = element_text(size = 8, margin = margin(t = 8, 0, 0, 0)),
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
    plot.title = element_text(size = 10, face = "bold", margin = margin(b = 1, 0, 0, 0)),
    plot.margin = margin(5, 5, 5, 5),
  ) +
  scale_color_jama()

# save output
ggsave("Plots/Output/ewma_1d.png",
  plot = last_plot(),
  dpi = 600, width = 7.86, height = 5, unit = "cm"
)

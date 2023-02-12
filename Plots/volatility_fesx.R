# load relevant packages
library(tidyverse)
library(scales)
library(ggsci)
library(showtext)
library(latex2exp)

# import written functions and store master sheet in memory
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
  scale_color_jama()

# save output
ggsave("Plots/Output/ewma_1d.png",
  plot = last_plot(),
  dpi = 600, width = 7.86, height = 5, unit = "cm"
)

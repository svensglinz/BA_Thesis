# load relevant packages
library(tidyverse)
library(scales)
library(ggsci)
library(showtext)
library(latex2exp)

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

fsmi <- master$returns |>
  filter(INST == "FSMI") |>
  select(INST, DATE, LOG_RET) |>
  arrange(desc(DATE))

fgbx <- master$returns |>
  filter(INST == "FGBX") |>
  select(INST, DATE, LOG_RET) |>
  arrange(desc(DATE))

fgbl <- master$returns |>
  filter(INST == "FGBL") |>
  select(INST, DATE, LOG_RET) |>
  arrange(desc(DATE))

fesx$VOL <- ewma_vol(fesx$LOG_RET, burn_in = 750, lambda = .96, mean = TRUE)
fsmi$VOL <- ewma_vol(fsmi$LOG_RET, burn_in = 750, lambda = .96, mean = TRUE)
fgbl$VOL <- ewma_vol(fgbl$LOG_RET, burn_in = 750, lambda = .96, mean = TRUE)
fgbx$VOL <- ewma_vol(fgbx$LOG_RET, burn_in = 750, lambda = .96, mean = TRUE)

combined <- bind_rows(fesx, fsmi, fgbl, fgbx)

# define function inputs
start_date <- as.Date("2020-01-01")
end_date <- as.Date("2020-12-31")

# plot graph
out <-
  combined |>
  ggplot(aes(x = DATE, y = VOL, color = INST)) +
  geom_line() +
  labs(
    title = "1d EWMA Volatility, 2020 (Log-Returns)",
    subtitle = TeX("$\\lambda$ = .95, burn-in = 750", italic = TRUE),
    y = NULL,
    x = NULL,
    color = NULL,
    caption = "Own Depiction | Data Source: Eurex Clearing AG"
  ) +
  scale_y_continuous(
    limits = c(0, 0.04),
    breaks = seq(from = 0.01, to = 0.04, by = 0.01),
    labels = scales::label_percent()
  ) +
  scale_x_date(
    breaks = seq.Date(
      from = as.Date("2020-01-01"),
      to = as.Date("2020-12-31"),
      by = "month"
    ),
    limits = c(as.Date("2020-01-01"), as.Date("2020-12-31")),
    labels = scales::label_date(format = "%b"),
    expand = expansion(mult = .01)
  ) +
  theme(
    text = element_text(family = "lmroman", colour = "#555555"),
    legend.position = "bottom",
    legend.key.width = unit(.35, "cm"),
    legend.background = element_rect(fill = "transparent", colour = "#cccccc", linewidth = 0),
    legend.justification = .5,
    plot.subtitle = element_text(size = 8, family = "sans"),
    plot.caption = element_text(size = 8, margin = margin(0, 0, 0, 0)),
    panel.border = element_rect(colour = "#999999", fill = "transparent"),
    panel.background = element_rect(fill = "#FFFFFF", colour = "#999999", linewidth = 0),
    panel.grid.minor.y = element_line(colour = "#eeeeee", linewidth = 0.5),
    panel.grid.major = element_line(colour = "#eeeeee", linewidth = 0.5),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#F9F9F9", colour = "#CCCCCC", linewidth = 0, linetype = 1),
    legend.box.spacing = unit(-.2, "cm"),
    # legend.box.margin = margin(0, 0, 0, 0),
    axis.ticks = element_blank(),
    axis.text = element_text(size = 6),
    axis.text.y = element_text(margin = margin(0, 0, 0, 0)),
    axis.text.x = element_text(margin = margin(0, 0, 0, 0)),
    axis.title = element_text(size = 8),
    plot.title = element_text(size = 10, face = "bold"),
    legend.direction = "horizontal",
    legend.text = element_text(size = 8, margin = margin(b = 0, 0, 0, 0)),
    plot.margin = margin(5, 5, 5, 5),
    legend.key = element_rect(fill = "transparent"),
    strip.background = element_rect(fill = "#FFFFFF", color = "#808080", linewidth = 0.5),
    strip.text = element_text(size = 8, margin = margin(2, 2, 2, 2))
  ) +
  ggsci::scale_color_jama(breaks = c("FESX", "FSMI", "FGBX", "FGBL"))

# save output
ggsave("Plots/Output/ewma_1d.png",
  plot = out,
  dpi = 600, width = 7.86, height = 6.33, unit = "cm"
)

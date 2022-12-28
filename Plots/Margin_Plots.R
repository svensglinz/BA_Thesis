# load relevant packages
library(tidyverse)
library(scales)
library(ggsci)
library(patchwork)
library(showtext)

# add fonts for plotting
font_add(
  family = "lmroman",
  regular = "Fonts/lmroman10_regular.ttf",
  bold = "Fonts/lmroman10_bold.ttf",
  italic = "Fonts/lmroman10_italic.ttf",
  bolditalic = "Fonts/lmroman10_bolditalic.ttf"
)

showtext_auto(enable = TRUE)
showtext_opts(dpi = 350)

# import written functions and store master sheet in memory
source("functions.R")
master <- read_master("Data/data_input.xlsx")

# define function parameters
start_date <- as.Date("2020-01-01")
end_date <- as.Date("2020-12-31")

# store args for margin calculations
args_long_fesx <-
  list(
    MPOR = 3, factor = 1.35, quantile = 0.975,
    lambda = 0.95, n_day = 750, floor = FALSE,
    absolute = FALSE, liq_group = "PEQ01",
    short = FALSE
  )

args_short_fesx <-
  list(
    MPOR = 3, factor = 1.35, quantile = 0.975,
    lambda = 0.95, n_day = 750, floor = FALSE,
    absolute = FALSE, liq_group = "PEQ01",
    short = TRUE
  )

args_long_fgbx <-
  list(
    MPOR = 3, factor = 1.35, quantile = 0.975,
    lambda = 0.95, n_day = 750, floor = FALSE,
    absolute = FALSE, liq_group = "PFI01",
    short = FALSE
  )

args_short_fgbx <-
  list(
    MPOR = 3, factor = 1.35, quantile = 0.975,
    lambda = 0.95, n_day = 750, floor = FALSE,
    absolute = FALSE, liq_group = "PFI01",
    short = TRUE
  )

# calculate long & short margin for FGBX & FESX
fesx_margin_long <-
  calculate_margin(
    product = "FESX", start = start_date, end = end_date,
    args = args_long_fesx, steps = TRUE
  ) |>
  select(DATE, RET_MPOR, MARGIN) |>
  mutate(
    MARGIN = MARGIN * -1,
    RET_MPOR = lag(RET_MPOR, 3),
    BREACH = ifelse(RET_MPOR < MARGIN, TRUE, FALSE)
  )

fesx_margin_short <-
  calculate_margin(
    product = "FESX", start = start_date, end = end_date,
    args = args_short_fesx, steps = TRUE
  ) |>
  select(DATE, RET_MPOR, MARGIN) |>
  mutate(
    RET_MPOR = lag(RET_MPOR * -1, 3),
    BREACH = ifelse(RET_MPOR > MARGIN, TRUE, FALSE)
  )

fgbx_margin_long <-
  calculate_margin(
    product = "FGBX", start = start_date, end = end_date,
    args = args_long_fgbx, steps = TRUE
  ) |>
  select(DATE, RET_MPOR, MARGIN) |>
  mutate(
    MARGIN = MARGIN * -1,
    RET_MPOR = lag(RET_MPOR, 3),
    BREACH = ifelse(RET_MPOR < MARGIN, TRUE, FALSE)
  )

fgbx_margin_short <-
  calculate_margin(
    product = "FGBX", start = start_date, end = end_date,
    args = args_short_fgbx, steps = TRUE
  ) |>
  select(DATE, RET_MPOR, MARGIN) |>
  mutate(
    RET_MPOR = lag(RET_MPOR * -1, 3),
    BREACH = ifelse(RET_MPOR > MARGIN, TRUE, FALSE)
  )

# join LONG & SHORT Margin sets
fesx_margin <-
  fesx_margin_long |>
  full_join(fesx_margin_short, by = c("DATE")) |>
  rename(
    LONG = MARGIN.x,
    SHORT = MARGIN.y,
    RET_MPOR_LONG = RET_MPOR.x,
    RET_MPOR_SHORT = RET_MPOR.y,
    BREACH_LONG = BREACH.x,
    BREACH_SHORT = BREACH.y
  ) |>
  mutate(
    BREACH = ifelse((BREACH_LONG | BREACH_SHORT), TRUE, FALSE),
    COLOR = ifelse(BREACH, "red", "black"),
    SIZE = ifelse(BREACH, 1, .5)
  ) |>
  pivot_longer(
    c("SHORT", "LONG"),
    names_to = "DIRECTION", values_to = "MARGIN"
  ) |>
  select(-c(RET_MPOR_SHORT, BREACH_LONG, BREACH_SHORT))

fgbx_margin <-
  fgbx_margin_long |>
  full_join(fgbx_margin_short, by = c("DATE")) |>
  rename(
    LONG = MARGIN.x,
    SHORT = MARGIN.y,
    RET_MPOR_LONG = RET_MPOR.x,
    RET_MPOR_SHORT = RET_MPOR.y,
    BREACH_LONG = BREACH.x,
    BREACH_SHORT = BREACH.y
  ) |>
  mutate(
    BREACH = ifelse((BREACH_LONG | BREACH_SHORT), TRUE, FALSE),
    COLOR = ifelse(BREACH, "red", "black"),
    SIZE = ifelse(BREACH, 1, .5)
  ) |>
  pivot_longer(
    c("SHORT", "LONG"),
    names_to = "DIRECTION", values_to = "MARGIN"
  ) |>
  select(-c(RET_MPOR_SHORT, BREACH_LONG, BREACH_SHORT))

# set general theme for both plots
theme_set(
  theme(
    text = element_text(family = "lmroman"),
    panel.grid = element_blank(),
    panel.background = element_rect(color = "black", fill = "white"),
    axis.text = element_text(size = 7),
    axis.text.x = element_text(angle = 45, vjust = .5),
    legend.position = "bottom",
    plot.subtitle = element_text(
      size = 8, face = "italic",
      hjust = 0, margin = margin(t = 0, r = 0, b = 5, l = 0)
    ),
    plot.title = element_text(size = 10, hjust = 0),
    legend.key = element_rect(color = "white", fill = "white"),
    legend.key.size = unit(.4, "cm"),
    legend.text = element_text(size = 7)
  )
)

# FESX Plot
fesx_plot <-
  fesx_margin |>
  ggplot(aes(x = DATE, y = MARGIN, color = DIRECTION)) +
  geom_line() +
  scale_y_continuous(
    breaks = seq(from = -0.25, to = 0.25, by = 0.05),
    labels = scales::label_percent()
  ) +
  scale_x_date(
    breaks = seq.Date(
      from = start_date,
      to = end_date,
      by = "1 month"
    ),
    labels = scales::date_format(format = "%b")
  ) +
  geom_point(aes(y = RET_MPOR_LONG, fill = "lagged n-day Returns"),
    color = fesx_margin$COLOR,
    size = fesx_margin$SIZE
  ) +
  labs(
    title = "FESX", subtitle = "n = 3",
    x = NULL, y = NULL, color = NULL, fill = NULL
  ) +
  scale_color_jama()

# FGBX Plot
fgbx_plot <-
  fgbx_margin |>
  ggplot(aes(x = DATE, y = MARGIN, color = DIRECTION)) +
  geom_line() +
  scale_y_continuous(
    breaks = seq(from = -0.08, to = 0.08, by = 0.02),
    labels = scales::label_percent()
  ) +
  scale_x_date(
    breaks = seq.Date(
      from = start_date,
      to = end_date,
      by = "1 month"
    ),
    labels = scales::date_format(format = "%b")
  ) +
  geom_point(aes(y = RET_MPOR_LONG, fill = "lagged n-day Returns"),
    color = fgbx_margin$COLOR,
    size = fgbx_margin$SIZE
  ) +
  labs(
    title = "FGBX",
    subtitle = "n = 2",
    x = NULL, y = NULL, color = NULL,
    fill = NULL
  ) +
  scale_color_jama()

# paste two plots together
out <-
  fesx_plot + fgbx_plot +
  plot_annotation(
    title = "Margin Requirement MRIM (2020)",
    caption = "Own Depiction | Source: Eurex Clearing AG",
    theme = theme(
      text = element_text(family = "lmroman"),
      plot.title = element_text(hjust = .5, size = 14, face = "bold"),
      legend.position = "bottom",
      plot.caption = element_text(size = 7, hjust = 1)
    )
  ) +
  plot_layout(guides = "collect")

# save output
ggsave("margins.png",
  plot = out, device = png, dpi = 350,
  width = 15.9, height = 8.5, unit = "cm"
)

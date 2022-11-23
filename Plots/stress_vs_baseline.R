# load relevant packages
library(readxl)
library(lubridate)
library(zoo)
library(tidyverse)
library(scales)
library(ggsci)

# import written functions and store master sheet in memory
master <- read_master("Data/data_input.xlsx")
source("functions.R")

# define parameters
start_date <- as.Date("2020-01-01")
end_date <- as.Date("2020-12-31")

args_long <-
  list(
    MPOR = 3, factor = 1.37, quantile = 0.974,
    lambda = 0.9593, n_day = 750, floor = FALSE,
    absolute = FALSE, liq_group = "PEQ01", short = FALSE
  )

args_short <-
  list(
    MPOR = 3, factor = 1.37, quantile = 0.974,
    lambda = 0.9593, n_day = 750, floor = FALSE,
    absolute = FALSE, liq_group = "PEQ01", short = TRUE
  )

stress_short <- calculate_SP_margin(
  product = "FESX", start = start_date,
  end = end_date, args = args_short
) |>
  mutate(type = "STRESS")

# calculate margin requirements (FHS & STRESS for short and long)
stress_long <- margin_calculator(
  product = "FESX", start = start_date,
  end = end_date, args = stress_long,
  steps = FALSE
) |>
  mutate(type = "STRESS")

fhs_long <- calculate_FHS_margin(
  product = "FESX", start = start_date,
  end = end_date, args = fhs_long,
  steps = FALSE
) |>
  mutate(type = "BASELINE") |>
  rename(Margin = FHS_Margin)

joined <- stress_long |>
  bind_rows(baseline_long)

joined |>
  ggplot(aes(x = DATE, y = Margin, color = type)) +
  geom_line() +
  labs(
    title = "",
    color = NULL,
    x = NULL,
    y = NULL
  ) +
  theme(
    panel.grid = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.background = element_rect(color = "black", fill = "white"),
    axis.text.x = element_text(angle = 45, vjust = .5),
    legend.position = "bottom",
    plot.subtitle = element_text(size = 8, face = "italic"),
    plot.title = element_text(size = 10),
    legend.key = element_rect(fill = "white")
  ) +
  scale_color_jama()

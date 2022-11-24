# load relevant packages
library(tidyverse)
library(scales)
library(ggsci)

# import written functions and store master sheet in memory
source("functions.R")
master <- read_master("Data/data_input.xlsx")

# define function inputs
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

# calculate margin requirements (FHS & Floored for long and short)
floored_short <- calculate_margin(
  product = "FESX", start = start_date,
  end = end_date, args = args_short,
  steps = FALSE
) |>
  mutate(type = "FLOORED")

floored_long <- calculate_margin(
  product = "FESX", start = start_date,
  end = end_date, args = args_long,
  steps = FALSE
) |>
  mutate(TYPE = "FLOORED")

fhs_long <- calculate_FHS_margin(
  product = "FESX", start = start_date,
  end = end_date, args = args_long,
  steps = FALSE
) |>
  mutate(TYPE = "FHS")

fhs_short <- calculate_FHS_margin(
  product = "FESX", start = start_date,
  end = end_date, args = args_short,
  steps = FALSE
) |>
mutate(type = "FHS")

# join floored and unfloored margin requirements
joined <- floored_long |>
  bind_rows(fhs_long)

# assemble plot
joined |>
  ggplot(aes(x = DATE, y = MARGIN, color = TYPE)) +
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

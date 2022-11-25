# load relevant packages
library(tidyverse)
library(scales)
library(ggsci)

# import written functions and store master sheet in memory
source("functions.R")
master <- read_master("Data/data_input.xlsx")

# add fonts for plotting
font_add(
  family = "lmroman", regular = "Fonts/lmroman10_regular.ttf",
  bold = "Fonts/lmroman10_bold.ttf",
  italic = "Fonts/lmroman10_italic.ttf",
  bolditalic = "Fonts/lmroman10_bolditalic.ttf"
)

showtext_auto(enable = TRUE)
showtext_opts(dpi = 350)


# function which calculates the 1d EWMA Volatility
calculate_vola <- function(product, start, end, lambda, n_day, MPOR) {

  require(magrittr)
  require(zoo)
  require(dplyr)

  # filter by product & Date
  returns <-
    master$returns |>
    filter(INST == product & DATE <= end) |>
    select(-INST)

  # select necessary amount of rows to calculate vola
  # until "end" Date
  cutoff <- max(which(returns$DATE >= start)) + n_day
  adj_cutoff <- round(cutoff / MPOR) * MPOR
  returns <- returns[(1:adj_cutoff), ]

  # weights for calculation of Volatility
  # -> INCLUDE ADJUSTMENT FACTOR AS IN PAPER
  weights <- (1 - lambda) * ((lambda)^c(0:(n_day - 1)))

  vola <- rollapply(returns["LOG_RET"], n_day,
    FUN = function(x) {
      sqrt(sum(x^2 * weights))
    }, align = "left"
  )

  out <- tibble(
    DATE = returns$DATE[seq_along(vola)],
    VOLA = as.vector(vola)
  )
  return(out)
}

# define function inputs
start_date <- as.Date("2020-01-01")
end_date <- as.Date("2020-12-31")
lambda <- .95
n_day <- 750
MPOR <- 1

# calculate 1d EWMA for FESX FSMI, FBGX & FGBL
vol_fesx <-
  calculate_vola(
    product = "FESX", start = start_date, end = end_date,
    lambda = lambda, n_day = n_day, MPOR = MPOR
  ) |>
  rename(FESX = VOLA)

vol_fsmi <-
  calculate_vola(
    product = "FSMI", start = start_date, end = end_date,
    lambda = lambda, n_day = n_day, MPOR = MPOR
  ) |>
  rename(FSMI = VOLA)

vol_fgbx <-
  calculate_vola(
    product = "FGBX", start = start_date, end = end_date,
    lambda = lambda, n_day = n_day, MPOR = MPOR
  ) |>
  rename(FGBX = VOLA)

vol_fgbl <-
  calculate_vola(
    product = "FGBL", start = start_date, end = end_date,
    lambda = lambda, n_day = n_day, MPOR = MPOR
  ) |>
  rename(FGBL = VOLA)

# combine calculated volatilities for instruments
combined <- vol_fesx |>
  full_join(vol_fsmi, by = c("DATE")) |>
  full_join(vol_fgbx, by = c("DATE")) |>
  full_join(vol_fgbl, by = c("DATE")) |>
  na.omit() |>
  pivot_longer(
    cols = 2:5,
    names_to = "SECURITY",
    values_to = "VOLA"
  )

# plot graph
out <-
  combined |>
  ggplot(aes(x = DATE, y = VOLA, color = SECURITY)) +
  geom_line() +
  labs(
    title = "1d EWMA weighted Volatility Returns",
    subtitle = bquote(paste(
      lambda, " = ", .(lambda),
      ", Burn-in = ", .(n_day)
    )),
    y = NULL,
    x = NULL,
    color = NULL
  ) +
  scale_y_continuous(
    breaks = seq(from = 0.01, to = 0.06, by = 0.01),
    labels = scales::label_percent()
  ) +
  scale_x_date(
    breaks = seq.Date(
      from = as.Date("2020-01-01"),
      to = as.Date("2020-12-31"),
      by = "month"
    ),
    labels = scales::label_date(format = "%b")
  ) +
  theme(
    text = element_text(family = "lmroman"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid = element_line(
      color = "darkgrey",
      linetype = "dashed",
      size = .3
    ),
    panel.background = element_rect(color = "black", fill = "white"),
    axis.text.x = element_text(angle = 45),
    legend.position = "bottom",
    plot.title = element_text(size = 10, face = "bold"),
    plot.subtitle = element_text(size = 7, face = "italic", family = "sans"),
    legend.text = element_text(size = 7),
    axis.text = element_text(size = 8),
    legend.key.size = unit(.2, "cm"),
    legend.box.spacing = unit(0, "pt"),
    axis.ticks.x = element_line(color = "black"),
    plot.background = element_rect(fill = "white")
  ) +
  scale_color_jama()

ggsave("Plots/Output/ewma_1d.png",
  dpi = 350, height = 6.32,
  width = 7.86, units = "cm"
)

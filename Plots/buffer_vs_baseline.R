# load relevant packages
library(readxl)
library(tidyverse)
library(scales)
library(ggsci)

# import written functions and store master sheet in memory
source("functions_redone.R")
master <- read_master("Data/data_input.xlsx")

# define function arguments
start_date <- as.Date("2005-01-01")
end_date <- as.Date("2021-12-31")

# define model parameters
args_long_FESX <-
  list(
    MPOR = 3, factor = 1.37, quantile = 0.974,
    lambda = 0.9593, n_day = 750, floor = FALSE,
    absolute = FALSE, liq_group = "PEQ01",
    short = FALSE
  )

FHS_Margin <-
  calculate_FHS_margin(
    product = "FESX", start = start_date, end = end_date,
    args = args_long_FESX, steps = FALSE
  )

#calculate day to day margin difference in % and absolute!

diff_abs <- c(diff(FHS_Margin$FHS_Margin, lag = 1), NA)
FHS_Margin |>
  mutate(diff_abs = diff_abs)
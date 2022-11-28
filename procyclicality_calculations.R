# load relevant packages
library(tidyverse)
library(scales)
library(ggsci)

# import written functions and store master sheet in memory
source("functions.R")
master <- read_master("Data/data_input.xlsx")

# define function paremeters
start_date <- as.Date("2007-01-01")
end_date <- as.Date("2020-12-31")

args_long <-
    list(
        MPOR = 3, factor = 1.37, quantile = 0.974,
        lambda = 0.9593, n_day = 750, floor = FALSE,
        absolute = FALSE, liq_group = "PEQ01",
        short = FALSE, abs = TRUE
    )

args_short <-
    list(
        MPOR = 3, factor = 1.37, quantile = 0.974,
        lambda = 0.9593, n_day = 750, floor = FALSE,
        absolute = FALSE, liq_group = "PEQ01",
        short = TRUE, abs = TRUE
    )

# calculate floored margin requirements
floored_long <-
    calculate_margin(
        product = "FESX",
        start = start_date, end = end_date,
        args = args_long, steps = TRUE
    )

floored_short <-
    calculate_margin(
        product = "FESX",
        start = start_date, end = end_date,
        args = args_short, steps = TRUE
    )

# calculate unfloored / baseline margin requirements
fhs_long <-
    calculate_fhs_margin(
        product = "FESX",
        start = start_date, end = end_date,
        args = args_long, steps = TRUE
    )

fhs_short <-
    calculate_fhs_margin(
        product = "FESX",
        start = start_date, end = end_date,
        args = args_short, steps = TRUE
    )


# set inputs (15 year and 2020 scenario)
start_15y <- as.Date("")
end_15y <- as.Date("")

start_2020 <- as.Date("")
end_2020 <- as.Date("")

# calculate summary stats (15 year scenario)
floored_long_summary <-
    summary_stats(floored_long, start = start_date, end = end_date)

floored_short_summary <-
    summary_stats(floored_short, start = start_date, end = end_date)

fhs_long_summary <-
    summary_stats(fhs_long, start = start_date, end = end_date)

fhs_short_summary <-
    summary_stats(fhs_short, start = start_date, end = end_date)

# calculate summary stats (2020 scenario)
floored_long_summary <-
    summary_stats(floored_long, start = start_date, end = end_date)

floored_short_summary <-
    summary_stats(floored_short, start = start_date, end = end_date)

fhs_long_summary <-
    summary_stats(fhs_long, start = start_date, end = end_date)

fhs_short_summary <-
    summary_stats(fhs_short, start = start_date, end = end_date)

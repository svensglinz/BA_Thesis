#load relevant packages
library(readxl)
library(lubridate)
library(zoo)
library(tidyverse)
library(scales)
library(ggthemes)
library(ggsci)
library(ggExtra)
library(ggforce)
library(patchwork)

#define initial variables
source("functions_redone.R")
master <- read_master("Data/data_input.xlsx")
start_date <- as.Date("01.01.2005", format = "%d.%m.%Y")
end_date <- as.Date("31.12.2021", format = "%d.%m.%Y")

#define model parameters
args_long_FESX <-
  list(MPOR = 3, factor = 1.37, quantile = 0.974,  quantile = 0.974,
       lambda = 0.9593, n_day = 750, floor = FALSE,
       absolute = FALSE, liq_group  = "PEQ01",
       short = FALSE)

FHS_Margin <- 
  calculate_FHS_margin(product = "FESX", start = start_date, end = end_date,
                     args = args_long_FESX, steps = F)

#calculate day to day margin difference in % and absolute!

diff_abs = c(diff(FHS_Margin$FHS_Margin, lag = 1), NA)
FHS_Margin |> 
  mutate(diff_abs = diff_abs)



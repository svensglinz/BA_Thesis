
#import test data etc. 
library(tidyverse)
library(readxl)
library(lubridate)
library(zoo)

#import stress periods 
stress_periods <- read_csv("Data/stress_periods.csv")
stress_periods[1:3] <- lapply(stress_periods[1:3],
                              function(x) as.Date(x, format = "%d/%m/%Y"))

stress_periods <- stress_periods |> filter(Liq_Group == "PEQ01")
dat <- read_xlsx("Data/selected_futures_returns.xlsx")
dat <- dat |> filter(INSTRUMENT == "FGBX")

FGBX <- dat |> select(-INSTRUMENT)
dat$DATE <- as.Date(dat$DATE)
dates <-  dat |>
  filter(INSTRUMENT == "FGBX") |>
  select(DATE) |>
  as.vector()

dates <- dates[[1]]

FGBX <- dat |>
  filter(INSTRUMENT == "FGBX") |>
  select(LOG_RET_1D) |>
  unlist()

attr(FGBX, "names") <- NULL

#--------------------------------------------------------------
args <- list(MPOR = 3, 
             factor = 1.37, 
             quantile = 0.025, 
             lambda = 0.94, 
             n_day = 750,
             floor = FALSE,
             absolute = FALSE,
             short = TRUE)

a <- Sys.time()
df_out <- calculate_margin_TS(returns = FGBX, dates = dates, args = args)
b <- Sys.time()
a-b

df_out |> 
  ggplot(aes(x = dates, y = margin))+
  geom_line()

rets <- tibble(returns = FGBX, 
               values = 1:length(FGBX))

#warning function to highlight outliers? 
Q3 <- returns |> quantile(.75)
Q1 <- returns |> quantile(.25)
IQR <- Q3[[1]]-Q1[[1]]

no_outliers <- FGBX[abs(FGBX) < 5*IQR]
rets |> 
  ggplot(aes(x = values, y = returns))+
  geom_boxplot()
test <- returns[100:1600]

df <- tibble(a = rolling_vola, b = 1:length(rolling_vola))
df |> 
  ggplot(aes(x = b, y = a))+
  geom_point()

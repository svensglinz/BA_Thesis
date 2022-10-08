
#import test data etc. 
library(tidyverse)
library(readxl)
library(lubridate)
library(zoo)

#--------------------------------------------------------------



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

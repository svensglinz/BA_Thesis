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
library(latex2exp)

#' Function description
#' @param returns 
#' @param n_day
#' @param lambda
#' @return  
#' @examples 

calculate_vola <- function(product, start, end, lambda, n_day, MPOR){
  
  start <- as.Date(start, format = "%d/%m/%Y")
  end <- as.Date(end, format = "%d/%m/%Y")
  
  returns <- 
    master$returns |> 
    filter(INST == product & DATE <= end) |> 
    select(-INST)
  
  #if cutoff does not exist --> make 1
  cutoff <- max(which(returns$DATE >= start))+ n_day
  adj_cutoff <- round(cutoff/MPOR)*MPOR
  returns <- returns[(1:adj_cutoff),]
  
  #depending on MPOR returns must be summed up here!
  returns |> 
    gather()
  weights <- (1-lambda)* ((lambda) ^c(0:(n_day-1)))
  
  vola <- rollapply(returns["LOG_RET"], n_day,
                    FUN = function(x) 
                      sqrt(sum(x^2 *weights)), align = "left")
  
  out <- tibble(returns = returns$DATE[1:length(vola)],
                vola = as.vector(vola))
  return(out)
}

#define basic parameters 
master <- read_master("Data/data_input.xlsx")
source("functions_redone.R")

start_date = "01/01/2020"
end_date = "31/12/2020"
lambda = .95
n_day = 750
MPOR = 1

vol_FESX <-
  calculate_vola(product = "FESX", start = start_date, end = end_date,
    lambda = lambda, n_day = n_day, MPOR = MPOR) |>
  rename(date = returns, FESX = vola)

vol_FSMI <-
  calculate_vola(product = "FSMI",start = start_date, end = end_date,
    lambda = lambda,n_day = n_day, MPOR = MPOR) |>
  rename(date = returns, FSMI = vola)

vol_FGBX <-
  calculate_vola(product = "FGBX",start = start_date, end = end_date,
    lambda = lambda,n_day = n_day, MPOR = MPOR) |>
  rename(date = returns, FGBX = vola)

vol_FGBL <-
  calculate_vola(product = "FGBL",start = start_date, end = end_date,
                 lambda = lambda,n_day = n_day, MPOR = MPOR) |>
  rename(date = returns, FGBL = vola)

combined <- vol_FESX |>
  full_join(vol_FSMI) |>
  full_join(vol_FGBX) |>
  full_join(vol_FGBL) |>
  na.omit() |>
  pivot_longer(cols = 2:5,
               names_to = "product",
               values_to = "vola")

combined |> 
  ggplot(aes(x = date, y = vola, color = product))+
  geom_line()+
  labs(title = "1d EWMA weighted Volatility Returns",
       subtitle = bquote(paste(lambda," = ", .(lambda), ", Burn-in = ", .(n_day))),
       y = NULL,
       x = NULL,
       color = NULL)+
  scale_y_continuous(breaks = seq(from = 0.01, to = 0.06, by = 0.01),
                     labels = scales::label_percent())+
  scale_x_date(breaks = seq.Date(from = as.Date("2020-01-01"), to = as.Date("2020-12-31"), by = "month"),
               labels = scales::label_date(format = "%b"))+
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid = element_line(
      color = "grey",
      linetype = 2,
      size = 0.5
    ),
    panel.background = element_rect(color = "black", fill = "white"),
    axis.text.x = element_text(angle = 45),
    legend.position = "right",
    plot.title = element_text(size = 12, face = "bold"),
    legend.key = element_rect(fill = "white")
  )+
  scale_color_jama()

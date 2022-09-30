library(tidyverse)
library(readxl)
library(lubridate)
library(zoo)
library(runner)

dat <- read_xlsx("Data/selected_futures_returns.xlsx")
dat <- dat |> filter(INSTRUMENT == "FGBX")

returns$DATE <- as.Date(returns$DATE)
dates <-  returns |>
  filter(INSTRUMENT == "FGBX") |>
  select(DATE) |>
  unlist()

FGBX <- returns |>
  filter(INSTRUMENT == "FGBX") |>
  select(LOG_RET_1D) |>
  unlist()

attr(FGBX, "names") <- NULL

#--------------------------------
#Input:
#lambda: Decay factor (0,1]
#returns: numeric vector which contains a series of log returns
#The function calculates the n_day rolling exponentially-weighted-volatility 
#for each ordered return observation
#--------------------------------

vola <- function(lambda, returns, n_day){

options(warn = -1)  
weights <- (1-lambda)* ((lambda) ^c(0:(length(returns)-1)))

roll_vola <- rollapply(returns, n_day,
                       FUN = function(x){
                         sum(x^2 *weights), align = "left", fill = NA)}

return(roll_vola)

}

#--------------------------------
#Input:
#returns: numeric vector which contains a series of log returns
#MPOR: The liquidation period (ie. Time period over which VAR is calculated)
#factor: 
#log_ret: 
#quantile: The quantile needed to calculate the VAR
#absolute: 
#lambda: Decay factor (0,1]
#The function calculates the Margin Requirement according to the formula specified in XXX with 
#the provided parameter specifications. 
#--------------------------------

calculate_margin <- function(returns, MPOR, factor, log_ret = TRUE, quantile, absolute = FALSE, lambda,
                             floor = FALSE){
  
  #------------------------
  #load required packages
  #------------------------
  
  tryCatch(expr = {
  require("tidyverse")
  require("zoo")}, 
  erorr = function(e) stop("make sure that tidyverse and zoo are installed"))
  
  #-------------------------
  #ERROR HANDLING FOR ALL ARGUMENTS
  #-------------------------
  
  #returns
  if(!is.numeric(returns)) {stop("make sure that returns are a numeric vector")}
  
  #check here if prices or log returns are provided?
  #returns <- ifelse(isTRUE(log_ret), returns, log(returns))
  
  #MPOR
  if(!is.numeric(MPOR)){
    
    stop("MPOR must be an integer")
    
  } else if (!(as.integer(MPOR) == MPOR)){
    
    stop("MPOR must be an integer")
  }
  
  #quantile
  if(abs(quantile) > 1){
    stop("Quantile must be in range [-1,1]")
  }
  
  #----------------------------
  #Calculate Margin given all inputs
  #----------------------------
  
  rolling_vola <- vola(lambda = 0.94, returns = returns)
  MPOR_returns <- rollsum(returns, MPOR)
  vola_adj_returns <- MPOR_returns / rolling_vola[-c(1:2)] * rolling_vola[3]
  
  #-2 or something like that here for values at the end?
  buckets <- rep(1:MPOR, length.out = (length(returns)-2))
  
  combined <- tibble(MPOR_returns = MPOR_returns, 
                        buckets = buckets)
  
  bucket_VAR <- 
    combined |>
    group_by(buckets) |>
    summarize(VAR = quantile(MPOR_returns, 0.025, na.rm = T))
  
  
  VAR <- bucket_VAR |>
    summarize(mean = mean(VAR))

  VAR_upscaled <- VAR * factor
  
  return(VAR_upscaled[[1]])
}

#-------------------------------------
#-------------------------------------
#-------------------------------------

calculate_margins(returns, dates, ...){
  
  margins <- rollapply(data = returns,
                       width = n_day,
                       FUN = function(x){
                         calculate_margin(returns = x,
                                          MPOR = MPOR,
                                          factor = factor, log_ret = TRUE,
                                          quantile = quantile,
                                          absolute = FALSE,
                                          lambda = lambda,
                                          floor = FALSE))}
  
  out <- tibble(dates = dates, 
                returns = margins)
  
  return(out)
}

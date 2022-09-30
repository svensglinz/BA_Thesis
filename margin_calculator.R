library(tidyverse)
library(readxl)
library(lubridate)
library(zoo)


returns <- read_xlsx("Data/selected_futures_returns.xlsx")

FGBX <- returns |> filter(INSTRUMENT == "FGBX") |> select(LOG_RET_1D) |> unlist() |> row_na
attr(FGBX, "names") <- NULL

## somehow returns must also have a date indicated to them such that we can calculate historical margins as well! --> 
#can circumvent by building a function which always gives the latest 500 returns!

#calculates exponentially weighted volatility for a given vector of returns


vola <- function(lambda, returns){
  
  
}

calculate_margin <- function(date, returns, MPOR, factor, log = TRUE, quantile, absolute = FALSE, lambda){
  
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
  
  returns <- ifelse(isTRUE(log), returns, log(returns))
  
  #date
  if(!is.Date(date)){
    
    e <- stop("Make sure to input a correct date!")
    tryCatch(as.Date(date), 
             error = function(e) e)
  }
  
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
  
  MPOR_returns <- rollsum(returns, MPOR)
  
  #-2 or something like that here for values at the end?
  buckets <- c(rep(1:MPOR, length.out = length(returns)-2))
  
  combined <- tibble(MPOR_returns = MPOR_returns, 
                        buckets = buckets)
  
  stored <- 
    combined |> group_by(buckets) |> summarize(VAR = quantile(MPOR_returns, 0.025, na.rm = T))
  
  
  VAR <- stored |> summarize(mean = mean(VAR))

  VAR_upscaled <- VAR * factor
  
  return(VAR_upscaled)
}

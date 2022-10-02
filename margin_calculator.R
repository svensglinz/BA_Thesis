
#--------------------------------
#Input:
#lambda: Decay factor (0,1)
#returns: numeric vector which contains a series of log returns
#The function calculates the n_day rolling exponentially-weighted-volatility 
#for each ordered return observation
#--------------------------------

read_master <- function(path){
  stress_periods <- read_excel(path, sheet = "stress_periods")
  stress_periods[1:3] <- lapply(stress_periods[1:3],
                                function(x) as.Date(x, format = "%d/%m/%Y"))
  
  returns <- read_excel("Data/data_input.xlsx", sheet = "returns")
  returns[1] <- as.Date(returns[[1]])
  
  out <- list(stress_periods = stress_periods, 
              returns = returns)
  
  return(out)
}

master <- read_master("Data/data_input.xlsx")


#calculates volatility of a product between two user-defined dates
calculate_vola <- function(product, start, end = NA, lambda, n_day){
  
  returns <- 
    master$returns |> 
    filter(INST == product) |> 
    filter(DATE <= start) |> 
    select(-INST)
  
  cutoff <- max(which(returns$DATE >= end))
  returns <- returns[1:(cutoff+ n_day),]
  
  weights <- (1-lambda)* ((lambda) ^c(0:(n_day-1)))
  
  vola <- rollapply(returns["LOG_RET"], n_day,
                         FUN = function(x) 
                           sum(x^2 *weights), align = "left")
  
  out <- tibble(returns = returns$DATE[1:length(vola)],
                vola = as.vector(vola))
  return(out)

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


calculate_FHS_margin <- function(product, start, end = NA, args){
  
  returns <- 
    master$returns |> 
    filter(INST == product) |> 
    filter(DATE <= start) |> 
    select(-INST)
  
  cutoff <- max(which(returns$DATE >= end))+ 2*n_day
  cutoff_date <- max(which(returns$DATE >= end))
  returns <- returns[(1:cutoff),]
  
  #ensures that all arguments are specified in args
  stopifnot(c("floor", "lambda", "MPOR", "factor",
    "quantile", "n_day", "log_ret",
    "absolute", "short") %in% names(args))
  
  # tryCatch(
  #   expr = {
  # require("tidyverse")
  # require("zoo")
  #   },
  # erorr = stop("make sure that tidyverse and zoo are installed"))
  # -------------------------
  # ERROR HANDLING FOR ALL ARGUMENTS
  # -------------------------
  # returns
  # if(!is.numeric(returns)){
  #   stop("make sure that returns are a numeric vector")
  #   }
  #  #MPOR
  #  if(!is.numeric(args$MPOR)){
  # 
  #    stop("MPOR must be an integer")
  # 
  #  } else if (!(as.integer(args$MPOR) == args$MPOR)){
  # 
  #    stop("MPOR must be an integer")
  #  }
  # 
  #  #quantile
  #  if(abs(args$quantile > 1)){
  #    stop("Quantile must be in range [-1,1]")
  #  }
  # 
  # returns are inverted for short positions
  # if(isTRUE(args$short)){
  #   returns <- returns*-1
  # }
  # ----------------------------
  # Calculate Margin given all inputs
  #----------------------------
  #Calculate volatility for all desired days

  weights <- (1-lambda)* ((lambda) ^c(0:(n_day-1)))
  
  vola <- rollapply(returns["LOG_RET"], n_day,
                    FUN = function(x) 
                      sum(x^2 *weights), align = "left")
  
  MPOR_returns <- rollsum(returns$LOG_RET[1: (nrow(vola)+2)], args$MPOR)
  
  #convert log returns to actual returns
  MPOR_returns <- exp(MPOR_returns) - 1
  
  test <- tibble(vola = as.vector(vola),
                 returns = as.vector(MPOR_returns))
  
  
  #calculate margin for every date inside the rolling function
  out <- rollapply(test, n_day, 
            FUN = 
              function(x){
                
              vol_adj_returns <- x[,2]/x[,1] * x[1,1][[1]]
              
              buckets <- rep(1:args$MPOR, length.out = args$n_day)
              combined <- tibble(returns = unlist(vol_adj_returns), 
                                 buckets = buckets)
              
              
              bucket_VAR <- 
                combined |>
                group_by(buckets) |>
                summarize(VAR = quantile(returns, 0.025, na.rm = T))
              
              
              VAR <- bucket_VAR |>
                summarize(mean = mean(VAR))
              
              VAR_upscaled <- VAR * args$factor
              
              return(abs(VAR_upscaled[[1]]))},
              by.column = F)

  out <- tibble(dates = returns$DATE[1:(cutoff_date + 2)],
                margin = out)
  return(out)
  
}


#------------------------------------
#Function which calculates the Stress Period VAR for given Dates
#------------------------------------

calculate_SP_margin <- function(product, start, end = NA){
  
  returns <- 
    master$returns |> 
    filter(INST == product) |> 
    select(-INST)
  
  stress_dates <- 
    master$stress_periods |> 
    filter(LIQ_GROUP == "PEQ01") |> 
    select(-LIQ_GROUP)
  
  sp_var_df <- stress_dates |>
    nest(data = c(CONF_LEVEL, DATE)) |> 
    
    mutate(data = map(data, function(x) x |>
                        left_join(returns)|>
                        na.omit()),
           
           data = map(data, function(x) x |>
                        mutate(r = as.vector(rollsum(x["LOG_RET"], 3, fill = NA, align = "left")),
                               bucket = rep(1:3, length.out = nrow(x)))),
           
           data = map(data, function(x) x |>
                        group_by(bucket, CONF_LEVEL) |>
                        summarize(var = quantile(r, (1-x[["CONF_LEVEL"]]/100), na.rm = T)) |> 
                        summarize(VAR = mean(var)) |> 
                        ungroup() |> 
                        summarize(VAR = mean(VAR)))
           ) |> 
    unnest()
      
  
  test <- returns |> mutate(INDEX = 1:nrow(returns)) |> select(-LOG_RET) |> 
    nest(data = DATE) |> 
    mutate(sp_var = map(data,
        function(x) {
          vec <- x[[1]][[1]] >= sp_var_df$VALID_FROM & x[[1]][[1]] <sp_var_df$VALID_TO
    return(sp_var_df$VAR[vec])})) |> 
    unnest(cols = c(data, sp_var)) |> 
    filter(DATE <= start & DATE >= end) |> 
    select(-INDEX)
  return(test)
}


#--------------------------------
#Input:
#--------------------------------


margin_calculator <- function(product, start, end = NA, args, out = NA){
  
  #end is set to start if it is not specified --> Guarantees return of a single observation
  start <- as.Date(start, format = "%d-%m-%Y")
  end <- as.Date(start, format = "%d-%m-%Y")
  end <- ifelse(is.na(end), as.Date(start), end)
  end <- as.Date(end)
  
  SP_Margin()
  FHS <- calculate_FHS_Margin(product = product, start = start, end = end, args = args)
  SP <- calculate_SP_margin(product = product, start = start, end = end)
  
  combined <- FHS |> left_join(SP, by = c("dates" = "DATE"))
  combined$MARGIN <- pmax(combined$margin, combined$sp_var)
  
  margin <- combined |> select(dates, MARGIN)
  
  return(margin)
}

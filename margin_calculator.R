
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
  
  start <- as.Date(start, format = "%d/%m/%Y")
  end <- as.Date(end, format = "%d/%m/%Y")
  
  returns <- 
    master$returns |> 
    filter(INST == product) |> 
    select(-INST)
  
  #if cutoff does not exist --> make 1
  cutoff <- max(which(returns$DATE >= start))
  returns <- returns[1:(cutoff+ n_day),]
  
  weights <- (1-lambda)* ((lambda) ^c(0:(n_day-1)))
  
  vola <- rollapply(returns["LOG_RET"], n_day,
                         FUN = function(x) 
                           sqrt(sum(x^2 *weights)), align = "left")
  
  out <- tibble(returns = returns$DATE[1:length(vola)],
                vola = as.vector(vola))
  return(out)
}

calculate_FHS_margin <- function(product, start, end = NA, args){
  
  start <- as.Date(start, format = "%d/%m/%Y")
  end <- as.Date(end, format = "%d/%m/%Y")
  
  returns <- 
    master$returns |> 
    filter(INST == product) |> 
    filter(DATE <= end) |> 
    select(-INST)
  
  cutoff <- max(which(returns$DATE >= start))+ 2*args$n_day
  adj_cutoff <- round(cutoff/args$MPOR)*args$MPOR
  returns <- returns[(1:adj_cutoff),]
  
  #ensures that all arguments are specified in args
  # stopifnot(c("floor", "lambda", "MPOR", "factor",
  #   "quantile", "n_day", "log_ret",
  #   "absolute", "short") %in% names(args))
  
  # tryCatch(
  #   expr = {
  # require("tidyverse")
  # require("zoo")
  #   },
  # erorr = stop("make sure that tidyverse and zoo are installed"))
  
  # -------------------------
  # ERROR HANDLING FOR ALL ARGUMENTS
  # -------------------------
  
   #MPOR
   if(!is.numeric(args$MPOR)){

     stop("MPOR must be an integer")

   } else if (!(as.integer(args$MPOR) == args$MPOR)){

     stop("MPOR must be an integer")
   }

   #quantile
   if(abs(args$quantile > 1)){
     stop("Quantile must be in range [-1,1]")
   }

  #returns are inverted for short positions
  if(args$short){
    returns["LOG_RET"] <- returns["LOG_RET"]*-1
  }
  
  # ----------------------------
  # Calculate Margin given all inputs
  # ----------------------------
  
  #Calculate volatility for all desired days
  ### DO WE CALCULATE DAILY VOLATILITY OR VOLATILITY OF 3DAY RETURNS???!!!
  weights <- (1-args$lambda)* ((args$lambda) ^c(0:(args$n_day-1)))
  
  vola <- rollapply(returns["LOG_RET"], args$n_day,
                    FUN = function(x) 
                      sqrt(sum(x^2 *weights)), align = "left")
  
  MPOR_returns <- rollsum(returns$LOG_RET[1: (nrow(vola)+2)], args$MPOR)
  
  #convert log returns to actual returns
  MPOR_returns <- exp(MPOR_returns) - 1
  
  test <- tibble(vola = as.vector(vola),
                 returns = as.vector(MPOR_returns))
  
  #calculate margin for every date inside the rolling function
  out <- rollapply(test, args$n_day, 
            FUN = 
              function(x){
              
              #full revaluation of returns with most recent volatility observation
              floor <- quantile(x[,"vola"], 0.5)
              vol_adj_returns <- x[,2]/x[,1] * max(x[1,1][[1]], floor)
              
              buckets <- rep(1:args$MPOR, length.out = args$n_day)
              combined <- tibble(returns = unlist(vol_adj_returns), 
                                 buckets = buckets)
              
              bucket_VAR <- 
                combined |>
                group_by(buckets) |>
                summarize(VAR = quantile(returns, args$quantile, na.rm = T))
            
              VAR <- bucket_VAR |>
                summarize(mean = mean(VAR))
              
              VAR_upscaled <- VAR * args$factor
              
              return(abs(VAR_upscaled[[1]]))},
              by.column = F)

  out <- tibble(dates = returns$DATE[1:(cutoff - 2*args$n_day + 2)],
                margin = out)
  return(out)
}

#------------------------------------
#Function which calculates the Stress Period VAR for given Dates
#------------------------------------

calculate_SP_margin <- function(product, start, end = NA, args){
  
  returns <- 
    master$returns |> 
    filter(INST == product) |> 
    select(-INST)
  
  #invert returns for short positions
  if (args$short){
    returns["LOG_RET"] <- returns["LOG_RET"]*-1
  }
  
  stress_dates <- 
    master$stress_periods |> 
    filter(LIQ_GROUP == args$liq_group) |> 
    select(-LIQ_GROUP) |> 
    left_join(returns, by = c("DATE")) |> 
    na.omit()
  
  
  #nest data such that we can work on individual tibbles 
  sp_var_df <- stress_dates |>
    nest(data = c(CONF_LEVEL, DATE, LOG_RET)) 
  
  #add bucket columns and rolling 3 day returns 
  sp_var_df <- sp_var_df |> 
    mutate(data = map(data, function(x) x |> 
                        mutate(r = as.vector(rollsum(x["LOG_RET"], 3, fill = NA, align = "left")),
                               bucket = rep(1:3, length.out = nrow(x)))
                    ))
  #calculate VAR per bucket and per validity interval
  sp_var_df <- sp_var_df |> 
    mutate(data = map(data, function(x) x |>
                        group_by(bucket) |> 
                        summarize(var = quantile(r, 1-x[["CONF_LEVEL"]][1]/ 100, na.rm = T)) |> 
                        ungroup() |> 
                        summarize(var = abs(mean(var)))
                        )) |> 
    unnest(cols = c("data"))

  test <- returns |> 
    mutate(INDEX = 1:nrow(returns)) |>
    select(-LOG_RET) |> 
    nest(data = DATE) |> 
    mutate(sp_var = map(data,
        function(x) {
          
          vec <- x[[1]][[1]] >= sp_var_df$VALID_FROM & x[[1]][[1]] <sp_var_df$VALID_TO
    return(sp_var_df$var[vec])
          
          })) |> 
    unnest(cols = c(data, sp_var)) |> 
    filter(DATE >= start & DATE <= end) |> 
    select(-INDEX)
  return(test)
}

#--------------------------------
#Input:
#--------------------------------


margin_calculator <- function(product, start, end = NA, args){
  
  #end is set to start if it is not specified --> Guarantees return of a single observation
  start <- as.Date(start, format = "%d-%m-%Y")
  end <- as.Date(end, format = "%d-%m-%Y")


  FHS <- calculate_FHS_margin(product = product, start = start, end = end, args = args)
  SP <- calculate_SP_margin(product = product, start = start, end = end, args = args)
  
  combined <- FHS |>
    left_join(SP, by = c("dates" = "DATE"))
  
  combined$MARGIN <- pmax(combined$margin, combined$sp_var)
  
  margin <- combined |>
    select(dates, MARGIN)
  
  return(margin)
}


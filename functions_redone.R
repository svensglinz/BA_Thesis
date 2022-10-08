
#---------------------------------------------------------
#DESCRIPTION: 


#---------------------------------------------------------


#calculates the exponentially weighted volatility of
#a given return vector with a burn-in period of n-days and a 
#decay factor of lambda

#----------------------------------------------------------
#' Function description
#' @param returns 
#' @param n_day
#' @param lambda
#' @return  
#' @examples 
#' 
#----------------------------------------------------------

EWMA_vol <- function(returns, n_day, lambda){
  
  #calculate return weights
  weight <- (1-lambda)* ((lambda) ^c(0:(n_day-1))) 
  
  #rolling calculation of volatilities
  out <- rollapply(returns, n_day, align = "left", fill = NA, FUN = function(x){
    vol <- sqrt(sum(weight*x^2))
    return(vol)
  })
  
  #function output
  return(out)
}

#function which imports tha master excel-sheet where all 
#essential information is stored and properly formats it for further use
#in the below functions

#----------------------------------------------------------
#' Function description
#' @param returns 
#' @param n_day
#' @param lambda
#' @return  
#' @examples 
#' 
#----------------------------------------------------------

read_master <- function(path){
  stress_periods <- read_excel(path, sheet = "stress_periods")
  stress_periods[1:3] <- lapply(stress_periods[1:3],
                                function(x) as.Date(x, format = "%d/%m/%Y"))
  
  returns <- read_excel(path, sheet = "returns")
  returns[1] <- as.Date(returns[[1]])
  
  #function output / stress_periods and returns of all possible instruments 
  #in one list each
  out <- list(stress_periods = stress_periods, 
              returns = returns)
  
  return(out)
}

#----------------------------------------------------------
#' Function description
#' @param returns 
#' @param n_day
#' @param lambda
#' @return  
#' @examples 
#' 
#----------------------------------------------------------

#is this really necessary? we already have the function above!!!!
#if yes, add that returns can be aggregated over multiple days
#calculates volatility of a product between two user-defined dates
calculate_vola <- function(product, start, end = NA, lambda, n_day){
  
  start <- as.Date(start, format = "%d/%m/%Y")
  end <- as.Date(end, format = "%d/%m/%Y")
  
  returns <- 
    master$returns |> 
    filter(INST == product) |> 
    select(-INST)
  
  #if cutoff does not exist --> make 1
  cutoff <- max(which(returns$DATE >= start))+ 2*args$n_day
  adj_cutoff <- round(cutoff/args$MPOR)*args$MPOR
  returns <- returns[(1:adj_cutoff),]
  
  weights <- (1-lambda)* ((lambda) ^c(0:(n_day-1)))
  
  vola <- rollapply(returns["LOG_RET"], n_day,
                    FUN = function(x) 
                      sqrt(sum(x^2 *weights)), align = "left")
  
  out <- tibble(returns = returns$DATE[1:length(vola)],
                vola = as.vector(vola))
  return(out)
}

#function which calculates the filtered historical margin over 
#the two given dates for a specified instrument (product) and 
#the arguments stored in the list args

#----------------------------------------------------------
#' Function description
#' @param returns 
#' @param n_day
#' @param lambda
#' @return  
#' @examples 
#' 
#----------------------------------------------------------

calculate_FHS_margin <- function(product, start, end = NA, args){
  
  start <- as.Date(start, format = "%d/%m/%Y")
  end <- as.Date(end, format = "%d/%m/%Y")
  
  returns <- 
    master$returns |> 
    filter(INST == product) |> 
    filter(DATE <= end) |> 
    select(-INST) |> 
    na.omit() #<<--------------- is this the best thing to do here? do warning 
  #if too many nas exist!!!
  
  #cutoff date selected as real cutoff + n_day for calculation of last volatility 
  #observation!
  cutoff <- max(which(returns$DATE >= start))+ args$n_day
  
  #cutoff adjusted that the nrow are divisible by MPOR (such that there are no 
  #cut off periods, ie. each period/ bucket is represented the same amount of times 
  #in the data set)
  adj_cutoff <- round(cutoff/args$MPOR)*args$MPOR
  
  #shorten return vector for faster calculation!
  returns <- returns[(1:adj_cutoff),]
  
  #calculate MPOR day rolling returns, go from log returns to real returns
  #and add buckets to the days
  returns <- returns |> 
    mutate(MPOR_returns = rollsum(returns$LOG_RET, args$MPOR, fill = NA, align = "left")) |> 
    mutate(MPOR_returns = exp(MPOR_returns)-1,
           buckets = rep(1:args$MPOR, length.out = nrow(returns))) |> 
    
    #delete last observations which contain NA bc. the rollsum of returns cannot finish 
    #the last two observations (since it needs 3) --> Delete 3 such that all buckets 
    #are represented equally again!
    slice(1: (n() -args$MPOR))
  
  #calculate EWMA VOLA for each different bucket!!!
  #nest by bucket and calculate rolling volatilites with a time period of n_day / MPOR
  vol_returns <- returns |> 
    nest(data = -buckets) |> 
    mutate(
      vola = map(data, function(x){
        EWMA_vol(x[["MPOR_returns"]], args$n_day/ args$MPOR, args$lambda)
          })
    ) |>
    unnest(cols = c("data", "vola")) |> 
    #delete all omitted values now since we have calculated the volatilities
    na.omit() |> 
    #ensure there that length is always divisible by MPOR
    slice(1:(trunc(n()/args$MPOR)*args$MPOR))
  
  #create devalued returns for full revaluation further below
  vol_returns <- vol_returns |> 
    mutate(devalued = MPOR_returns / vola)
  
  #arrange by date for calculation of revaluation factor below
  vol_returns <- vol_returns |>
    ungroup() |>
    arrange(desc(DATE))
  
  #calculate mean over 1:3 bucket for each volatility and take mean. This is to smoothen 
  #out the revaluation factor and it is below joined onto the data frame!
  revalue_factor <- rep(rollapply(vol_returns$vola, args$MPOR, fill = NULL,
                              align = "left", by = 3, 
                              FUN = mean), args$MPOR)
  

  #join revaluation factor onto data set
vol_returns <- vol_returns |>
  arrange(buckets, desc(DATE)) |>
  mutate(revalue_factor = revalue_factor)  
  
  d <- vol_returns |> 
    nest(data = -buckets) |> 
    mutate(FHS_Margin = map(data, function(x){
      rollapply(x, width = args$n_day/args$MPOR, align = "left", fill = NA,
                by.column = F, FUN = function(x){
        #revalue returns
        reval <- as.numeric(x[,"devalued"]) * max(quantile(as.numeric(x[,"vola"]), .5),
                                                  as.numeric(x[, "revalue_factor"][1]))
        #output which is upscaled FHS_Margin
        out <- quantile(reval, ((1-args$quantile)/2), na.rm = T)[[1]] * args$factor
        return(out)
      }
      )})) |> 
    unnest(c(data, FHS_Margin)) |> 
    arrange(desc(DATE))
  
  #calculate the mean over three buckets as the final margin to smoothen out statistical fluctuations
    d <- d |> mutate(test = rollapply(d$FHS_Margin, width = args$MPOR,
                                      fill = NA, align = "left", FUN = mean)) |> 
      na.omit()
  
  return(d)
}

#----------------------------------------------------------
#' Function description
#' @param returns 
#' @param n_day
#' @param lambda
#' @return  
#' @examples 
#' 
#----------------------------------------------------------

calculate_SP_margin <- function(product, start, end = NA, args){
  
  start <- as.Date(start, format = "%d/%m/%Y")
  end <- as.Date(end, format = "%d/%m/%Y")
  
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

#----------------------------------------------------------
#' Function description
#' @param returns 
#' @param n_day
#' @param lambda
#' @return  
#' @examples 
#' 
#----------------------------------------------------------

margin_calculator <- function(product, start, end = NA, args){
  
  #formatting of dates
  start <- as.Date(start, format = "%d/%m/%Y")
  end <- as.Date(end, format = "%d/%m/%Y")
  
  FHS <- calculate_FHS_margin(product = product, start = start, end = end, args = args)
  SP <- calculate_SP_margin(product = product, start = start, end = end, args = args)
  
  combined <- FHS |>
    left_join(SP, by = c("DATE"))
  
  #larger of floor or FHS Margin!
  combined$test <- pmax(-combined$test, combined$sp_var)
  
  #output is only date and margin information (should be adjusted above for the FHS and SP Margin 
  #as well!)
  margin <- combined |>
    select(DATE, test)
  
  #function output
  return(margin)
}


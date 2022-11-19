
#' EWMA_Vol calculates the exponentially weighted volatility of a return 
#' vector with a burn-in period of n_day and a decay factor lambda
#' @param returns numerical vector of a financial return series
#' @param n_day amount of days considered for volatility calculation
#' @param lambda decay factor 
#' @return vector of calculated volatilities
#' @examples 
#' returns <- rnorm(n = 500, sd = 1, mean = 0)
#' n_day <- 50
#' lambda <- 0.95
#' EWMA_vol(returns, n_day, lambda)

EWMA_vol <- function(returns, n_day, lambda){
  
  #calculate return weights
  weight <- (1-lambda)* ((lambda) ^c(0:(n_day-1))) 
  
  #rolling calculation of volatility observations
  out <- rollapply(returns, n_day, align = "left", fill = NA, FUN = function(x){
    vol <- sqrt(sum(weight*x^2))
    return(vol)
  })

  return(out)
}

#' function which imports the master excel-sheet where all 
#' essential information is stored and properly formats it for further use
#' in the below functions
#' @param path local path where excel sheet with returns is stored
#' @return list which contains as sub-lists each individual sheet 
#' of the excel file 
#' @examples 
#' path <- C:/Users/sveng/OneDrive/Dokumente/Schule/Studium/HSG/Thesis/BA_Thesis/Data
#' master_file <- read_master(path)

read_master <- function(path){
  stress_periods <- read_excel(path, sheet = "stress_periods")
  stress_periods[1:3] <- lapply(stress_periods[1:3],
                                function(x) as.Date(x, format = "%d/%m/%Y"))
  
  returns <- read_excel(path, sheet = "returns")
  returns[1] <- as.Date(returns[[1]])
  
  out <- list(stress_periods = stress_periods, 
              returns = returns)
  
  return(out)
}

#' FUNCTION DESCRIPTION 
#' @param product name of product (string) for which margin should be calculated. 
#' Name must be in column INST of the masterfile
#' @param start string in format "dd/mm/yyyy". Start period for which margin should 
#' be calculated
#' @param end string in format "dd/mm/yyyy". End period for which margin should 
#' be calculated
#' @param args list with elements #short (T/F), #lambda (numeric), #MPOR (numeric),
#' #... which are needed to calculate a Margin
#' @return data frame with columns data & FHS_Margin which contains one margin 
#' calculation per date in between the specified date intervals
#' @examples 
#' #store master file with master function defined above 
#' master <- read_master(path)
#' #specify arguments in args list 
#' args <- list(short = F, lambda = 0.975, MPOR = 3, n_day = 750, ...)
#' #Calculate Margin for the Instrument "FESX"
#' FHS_Margin <- calculate_FHS_margin("FESX", "01/01/2020", "01/05/2020", args)

calculate_FHS_margin <- function(product, start, end = NA, args, steps = FALSE){
  
  start <- as.Date(start, format = "%d/%m/%Y")
  end <- as.Date(end, format = "%d/%m/%Y")
  
  #get returns of product from masterfile
  returns <- 
    master$returns |> 
    filter(INST == product) |> 
    filter(DATE <= end) |> 
    select(-INST) |> 
    na.omit() |> 
    arrange(desc(DATE))
  
  #cutoff date + values needed for vola calculation
  cutoff <- max(which(returns$DATE >= start))+ 2*args$n_day
  
  #adjusted cutoff that rows are divisible by MPOR
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
  
  if (args$short){
    returns <- returns |> 
      mutate(MPOR_returns = MPOR_returns*-1)
  }
  #calculate EWMA VOLA for each different bucket!!!
  #nest by bucket and calculate rolling volatilites with a time period of n_day / MPOR
  vol_returns <- returns |> 
    nest(data = -buckets) |> 
    mutate(
      vola = map(data, function(x){
        EWMA_vol(x[["MPOR_returns"]], (args$n_day/ args$MPOR), args$lambda)
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
  revalue_factor <- 
    rep(rollapply(vol_returns$vola, args$MPOR, fill = NULL,
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
        reval <- as.numeric(x[,"devalued"]) * max(quantile(as.numeric(x[,"vola"]), .5)[[1]],
                                                  as.numeric(x[, "revalue_factor"][1]))
        #output which is upscaled FHS_Margin
        out <- quantile(reval, ((1-args$quantile)/2), na.rm = T)[[1]] * args$factor #deleted / 2 behind
        return(out)
      }
      )})) |> 
    unnest(c(data, FHS_Margin)) |> 
    arrange(desc(DATE))
  
  #calculate the mean over three buckets as the final margin to smoothen out statistical fluctuations
    d <- d |> 
      mutate(FHS_Margin = abs(rollapply(d$FHS_Margin, width = args$MPOR,
                              fill = NA, align = "left", FUN = mean))) |> 
      na.omit()
  
  #return output
    if (steps) {
      return(d)
    } else {
      return(d |> select(DATE, FHS_Margin))
    }
    
}


#' FUNCTION DESCRIPTION 
#' @param product name of product (string) for which margin should be calculated. 
#' Name must be in column INST of the masterfile
#' @param start string in format "dd/mm/yyyy". Start period for which margin should 
#' be calculated
#' @param end string in format "dd/mm/yyyy". End period for which margin should 
#' be calculated
#' @param args list with elements #short (T/F), #lambda (numeric), #MPOR (numeric),
#' #... which are needed to calculate a Margin
#' @return data frame with columns data & FHS_Margin which contains one margin 
#' calculation per date in between the specified date intervals
#' @examples 
#' ... --> SPECIFY EXAMPLE

calculate_SP_margin <- function(product, start, end = NA, args){
  
  start <- as.Date(start, format = "%d/%m/%Y")
  end <- as.Date(end, format = "%d/%m/%Y")
  
  returns <- 
    master$returns |> 
    filter(INST == product) |> 
    select(-INST)
  
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
                        mutate(r = as.vector(rollsum(x["LOG_RET"], 3,
                                                     fill = NA, align = "left")),
                               bucket = rep(1:3, length.out = nrow(x))) |> 
                        mutate(ifelse(args$short, r*-1, r))
    ))
  
  # if (args$short){
  #   sp_var_df |> 
  #     unnest(cols = data) |> 
  #     mutate(r = r*-1) |> 
  #     nest(-c(VALID_FROM, VALID_TO))
  # }
  #calculate VAR per bucket and per validity interval
  
  sp_var_df <- sp_var_df |> 
    mutate(data = map(data, function(x) x |>
                        group_by(bucket) |> 
                        summarize(var = quantile(r,1-x[["CONF_LEVEL"]][1]/ 100, na.rm = T)) |> 
                        ungroup() |> 
                        summarize(var = abs(mean(var)))
    )) |> 
    unnest(cols = c("data"))
  
  Margin <- returns |> 
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
  return(Margin)
}

#' FUNCTION DESCRIPTION 
#' @param product name of product (string) for which margin should be calculated. 
#' Name must be in column INST of the masterfile
#' @param start string in format "dd/mm/yyyy". Start period for which margin should 
#' be calculated
#' @param end string in format "dd/mm/yyyy". End period for which margin should 
#' be calculated
#' @param args list with elements #short (T/F), #lambda (numeric), #MPOR (numeric),
#' #... which are needed to calculate a Margin
#' @return data frame with columns data & FHS_Margin which contains one margin 
#' calculation per date in between the specified date intervals
#' @examples 
#' ... --> SPECIFY EXAMPLE

margin_calculator <- function(product, start, end = NA, args, steps = FALSE){
  
  #formatting of dates
  start <- as.Date(start, format = "%d/%m/%Y")
  end <- as.Date(end, format = "%d/%m/%Y")
  
  FHS <- calculate_FHS_margin(product = product, start = start, end = end, args = args, steps = steps)
  SP <- calculate_SP_margin(product = product, start = start, end = end, args = args)
  
  combined <- FHS |>
    left_join(SP, by = c("DATE"))
  
  #larger of floor or FHS Margin
  combined$Margin <- pmax(combined$FHS_Margin, combined$sp_var)
  
  #conditional output
  if (steps){
   return(combined)
  }
   return(combined |> select(DATE, Margin))
}

#' Function Description
#' @param margins
#' @return 
#' @examples 

procyclicality_measures <- function(margins, start, end){
  
  start = as.Date(start, format = "%d/%m/%Y")
  end = as.Date(end, format = "%d/%m/%Y")
  
  #values are wrong not necessarily 20 days in between but rather biggest difference 
  #within a 20 day window!!!
  margins <- margins |> 
    arrange(DATE) |> 
    filter(between(DATE, start, end))
  
  peak_to_through <- max(margins$Margin, na.rm = T)/min(margins$Margin, na.rm = T)
  max_1d <- max(diff(margins$Margin, lag = 1) / margins$Margin[-length(margins$Margin)], na.rm = T)
  max_5d <- max(diff(margins$Margin, lag = 5) / margins$Margin[-c(length(margins$Margin):(length(margins$Margin)-4))], na.rm = T)
  max_30d <- max(diff(margins$Margin, lag = 20) / margins$Margin[-c(length(margins$Margin):(length(margins$Margin)-19))], na.rm = T) #30days = 20 business days!
  costs <- mean(margins$Margin, na.rm = T)
  out <- tibble(type = c("peak_to_through", "max_1d", "max_5d", "max_30d", "costs"),
                value = c(peak_to_through, max_1d, max_5d, max_30d, costs))
  return(out)
}

#' @param margins
#' @return 
#' @examples 
#returns some goodness Margins defined in the paper by which we measure a margin model 
#How is this connected to the default fund thing? 

summary_stats <- function(margin_df, start, end){
  
  start = as.Date(start, format = "%d/%m/%Y")
  end = as.Date(end, format = "%d/%m/%Y")
  
  margin_df <- margin_df |> 
    filter(between(DATE, start, end)) |> 
    mutate(MPOR_returns = exp(MPOR_returns)-1)
  
  N_observations <- length(na.omit(margin_df$MPOR_returns))
  N_breaches <- sum(margin_df$Margin < margin_df$MPOR_returns*-1, na.rm = T)

  perc_breaches <- (N_breaches / N_observations)*100
  realized_conf_level <- 100-perc_breaches
  
  #gives average shortfall in % not in absolute terms!!! (must check!!!)
  avg_shortfall <- margin_df |> filter(Margin < MPOR_returns*-1) |> 
    mutate(shortfall = MPOR_returns*-1 - Margin) |> 
    summarize(avg_shortfall = mean(shortfall)) |>
    pull(avg_shortfall)
  
  max_shortfall <- margin_df |> filter(Margin < MPOR_returns*-1) |> 
    mutate(shortfall = MPOR_returns - Margin) |> 
    summarize(max_shortfall = min(shortfall)) |> 
    pull(max_shortfall)
  
  costs <- mean(margin_df$Margin, na.rm = T)
  
  peak_to_through <- max(margin_df$Margin, na.rm = T)/min(margin_df$Margin, na.rm = T)
  max_1d <- max(diff(margin_df$Margin, lag = 1) / margin_df$Margin[-length(margin_df$Margin)], na.rm = T)
  max_5d <- max(diff(margin_df$Margin, lag = 5) / margin_df$Margin[-c(length(margin_df$Margin):(length(margin_df$Margin)-4))], na.rm = T)
  max_30d <- max(diff(margin_df$Margin, lag = 20) / margin_df$Margin[-c(length(margin_df$Margin):(length(margin_df$Margin)-19))], na.rm = T) #30days = 20 business days!
  
  out <- tibble(type = c("N_breaches", "N_observations", "perc_breaches", "conf_level", "avg_shortfall",
                  "max_shortfall", "costs", "peak_to_through", "max_1d", "max_5d", "max_30d"),
                values = c(N_breaches, N_observations, perc_breaches, realized_conf_level,
                           avg_shortfall, max_shortfall, costs, peak_to_through, max_1d, max_5d, max_30d))
  
  return(out)
}


#should the confidence level only be seen one sided ? IMO yes!!!



#Procyclicality Measure can be built into the Margin_Calculator Function where this can be specified!!!

#as a baseline scenario, we will use the unfloored FHS_Margin!!!

#Floor has already been done! --> No need to elaborate on this. 


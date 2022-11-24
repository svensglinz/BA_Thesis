
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

EWMA_vol <- function(returns, n_day, lambda) {
  require(zoo)

  # calculate return weights
  weight <- (1 - lambda) * ((lambda)^c(0:(n_day - 1)))

  # rolling calculation of volatility observations
  out <- rollapply(returns, n_day,
    align = "left", fill = NA,
    FUN = function(x) {
      vol <- sqrt(sum(weight * x^2))
      return(vol)
    }
  )

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

  require(readxl)

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

#MAYBE DO NOT WORK WITHIN MUTATE AND THE DATA FRAME BUT PICK APART INDIVIDUAL VECTORS???

calculate_FHS_margin <- function(product, start, end = NA, args,
                                 steps = FALSE, abs = FALSE) {

  require(tidyverse)
  require(zoo)

  # get returns of finanial instrument from master
  returns <-
    master$returns |>
    filter(INST == product) |>
    filter(DATE <= end) |>
    select(-INST) |>
    na.omit() |>
    arrange(desc(DATE))

  # cutoff date + values needed for vola calculation
  cutoff <- max(which(returns$DATE >= start)) + 2 * args$n_day
  adj_cutoff <- round(cutoff / args$MPOR) * args$MPOR

  # shorten return vector for faster calculation!
  returns <- returns[(1:adj_cutoff), ]

  # calculate MPOR day rolling returns, go from log returns to real returns
  # and add buckets to the days
  returns <- returns |>
    mutate(
      LOG_RET_MPOR = rollsum(returns$LOG_RET, args$MPOR,
        fill = NA, align = "left"
      ),
      RET_MPOR = exp(LOG_RET_MPOR) - 1,
      BUCKET = rep(seq_along(args$MPOR), length.out = nrow(returns))
    ) |>
    slice(1:(n() - args$MPOR))

  # delete last observations which contain NA bc. the rollsum of returns cannot finish
  # the last two observations (since it needs 3) --> Delete 3 such that all buckets
  # are represented equally again!

 # inverse returns for short positions
  if (args$short) {
    returns <-
      returns |>
      mutate(RET_MPOR = RET_MPOR * -1)
  }
  # calculate EWMA VOLA for each different bucket!!!
  # nest by bucket and calculate rolling volatilites with a time period of n_day / MPOR

  vol_returns <-
    returns |>
    nest(data = -BUCKET) |>
    mutate(
      EWMA_VOL = map(data, function(x) {
        EWMA_vol(x[["RET_MPOR"]], (args$n_day / args$MPOR), args$lambda)
      })
    ) |>
    unnest(cols = c("data", "EWMA_VOL")) |>
    na.omit() |>
    slice(1:(trunc(n() / args$MPOR) * args$MPOR)) # ensure there that length is always divisible by MPOR

  # create devalued returns for full revaluation further below
  vol_returns <- vol_returns |>
    mutate(DEVALUED = RET_MPOR / EWMA_VOL)

  # arrange by date for calculation of revaluation factor below
  vol_returns <- vol_returns |>
    ungroup() |>
    arrange(desc(DATE))

  # calculate mean over buckets for each volatility and take mean. This is to smoothen
  # out the revaluation factor and it is below joined onto the data frame!
  revalue_factor <-
    rep(
      rollapply(vol_returns$EWMA_VOL, args$MPOR,
        fill = NULL,
        align = "left", by = args$MPOR,
        FUN = mean
      ),
      args$MPOR
    )

  # join revaluation factor onto data set
  vol_returns <- vol_returns |>
    arrange(BUCKET, desc(DATE)) |>
    mutate(REVAL_FCT = revalue_factor)

  d <- vol_returns |>
    nest(data = -BUCKET) |>
    mutate(MARGIN = map(data, function(x) {
      rollapply(x,
        width = args$n_day / args$MPOR, align = "left", fill = NA,
        by.column = FALSE, FUN = function(x) {
          # revalue returns
          reval <- as.numeric(x[, "DEVALUED"]) * max(
            quantile(as.numeric(x[, "EWMA_VOL"]), .5)[[1]],
            as.numeric(x[, "REVAL_FCT"][1])
          )
          # output which is upscaled FHS_Margin
          out <- quantile(reval,
          ((1 - args$quantile) / 2), na.rm = T)[[1]] * args$factor

          return(out)
        }
      )
    })) |>
    unnest(c(data, MARGIN)) |>
    arrange(desc(DATE))

  # calculate the mean over three buckets as the final margin to smoothen out statistical fluctuations
  d <- d |>
    mutate(MARGIN = abs(rollapply(d$MARGIN,
      width = args$MPOR,
      fill = NA, align = "left", FUN = mean
    ))) |>
    na.omit()

  # returns absolute margin in dollar costs
  if (abs) {
    d <- d |>
      mutate(MARGIN = PRICE * MARGIN)
  }



  # return output
  if (steps) {
    return(d)
  } else {
    return(d |> select(DATE, MARGIN))
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

calculate_SP_margin <- function(product, start, end = NA, args, abs = F) {
  require(zoo)
  require(tidyverse)

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

  # nest data such that we can work on individual tibbles
  sp_var_df <- stress_dates |>
    nest(data = c(CONF_LEVEL, DATE, LOG_RET, PRICE))

  # add bucket columns and rolling 3 day returns
  sp_var_df <- sp_var_df |>
    mutate(data = map(data, function(x) {
      x |>
        mutate(
          r = as.vector(rollsum(x["LOG_RET"], 3,
            fill = NA, align = "left"
          )),
          bucket = rep(1:3, length.out = nrow(x))
        ) |>
        mutate(ifelse(args$short, r * -1, r))
    }))

  # calculate VAR per bucket and per validity interval

  sp_var_df <- sp_var_df |>
    mutate(data = map(data, function(x) {
      x |>
        group_by(bucket) |>
        summarize(var = quantile(r,
          1 - x[["CONF_LEVEL"]][1] / 100,
          na.rm = TRUE
        )) |>
        ungroup() |>
        summarize(var = abs(mean(var)))
    })) |>
    unnest(cols = c("data"))

  Margin <- returns |>
    mutate(INDEX = 1:nrow(returns)) |>
    select(-LOG_RET) |>
    nest(data = DATE) |>
    mutate(MARGIN = map(
      data,
      function(x) {
        vec <- x[[1]][[1]] >= sp_var_df$VALID_FROM & x[[1]][[1]] < sp_var_df$VALID_TO
        return(sp_var_df$var[vec])
      }
    )) |>
    unnest(cols = c(data, MARGIN)) |>
    filter(DATE >= start & DATE <= end) |>
    select(-c(INDEX, PRICE))

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

calculate_margin <- function(product, start, end = NA, args, steps = FALSE){

  FHS <- calculate_FHS_margin(product = product, start = start, end = end, args = args, steps = steps)
  SP <- calculate_SP_margin(product = product, start = start, end = end, args = args)

  combined <- FHS |>
    left_join(SP, by = c("DATE")) |>
    rename(FHS_MARGIN = MARGIN.x, SP_MARGIN = MARGIN.y)

  #larger of floor or FHS Margin
  combined$MARGIN <- pmax(combined$FHS_MARGIN, combined$SP_MARGIN)

  #conditional output
  if (steps){
   return(combined)
  }
   return(combined |> select(DATE, MARGIN))
}

# Function Descripiton
#' @param margins
#' @return
#' @examples
#returns some goodness Margins defined in the paper by which we measure a margin model 
#How is this connected to the default fund thing? 

summary_stats <- function(margin_df, start, end){

  require(tidyverse)

  margin_df <- margin_df |>
    filter(between(DATE, start, end)) |>
    mutate(RET_MPOR = lag(RET_MPOR, 3),
           CHANGE_1D = MARGIN / lead(MARGIN, 1),
           CHANGE_5D = MARGIN / lead(MARGIN, 5),
           CHANGE_30D = MARGIN / lead(MARGIN, 30))

  N_observations <- margin_df |>
    na.omit(RET_MPOR) |>
    length(RET_MPOR)

  N_breaches <- margin_df |>
    filter(MARGIN < RET_MPOR * -1) |>
    nrow()

  perc_breaches <- (N_breaches / N_observations) * 100
  realized_conf_level <- 100 - perc_breaches

  #gives average shortfall in % not in absolute terms!!! (must check!!!)
  avg_shortfall <- margin_df |>
  filter(MARGIN < RET_MPOR * -1) |>
    mutate(shortfall = MARGIN + RET_MPOR) |>
    summarize(avg_shortfall = mean(shortfall)) |>
    pull(avg_shortfall)

  max_shortfall <- margin_df |>
    filter(MARGIN < RET_MPOR * -1) |>
    mutate(shortfall = MARGIN + RET_MPOR) |>
    summarize(max_shortfall = min(shortfall, na.rm TRUE)) |>
    pull(max_shortfall)

  costs <- mean(margin_df$MARGIN, na.rm = TRUE)
  peak_to_through <- max(margin_df$MARGIN, na.rm = TRUE) /
    min(margin_df$MARGIN, na.rm = TRUE)

  max_1d <- margin_df |>
    summarize(MAX_1D = max(CHANGE_1D, na.rm = TRUE)) |>
    pull(MAX_1D)

  max_5d <- margin_df |>
    summarize(MAX_5D = max(CHANGE_5D, na.rm = TRUE)) |>
    pull(MAX_5D)

  max_30d <- margin_df |>
    summarize(MAX_30D = max(CHANGE_30D, na.rm = TRUE)) |>
    pull(MAX_30D)

  out <- tibble(
    type = c(
      "N_breaches", "N_observations", "perc_breaches",
      "conf_level", "avg_shortfall",
      "max_shortfall", "costs", "peak_to_through",
      "max_1d", "max_5d", "max_30d"
    ),
    values = c(
      N_breaches, N_observations, perc_breaches,
      realized_conf_level,
      avg_shortfall, max_shortfall, costs,
      peak_to_through, max_1d, max_5d, max_30d
    )
  )

  return(out)
}
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

#' function which imports the master excel-sheet where all
#' essential information is stored and properly formats it for further use
#' in the below functions
#' @param path local path where excel sheet with returns is stored
#' @return list which contains as sub-lists each individual sheet
#' of the excel file
#' @examples
#' path <- C:/Users/sveng/OneDrive/Dokumente/Schule/Studium/HSG/Thesis/BA_Thesis/Data
#' master_file <- read_master(path)

read_master <- function(path) {
    # load required packages
    require(readxl)
    stress_periods <- read_excel(path, sheet = "stress_periods")
    stress_periods[1:3] <- lapply(
        stress_periods[1:3],
        function(x) as.Date(x, format = "%d/%m/%Y")
    )

    returns <- read_excel(path, sheet = "returns")
    returns[1] <- as.Date(returns[[1]])

    out <- list(
        stress_periods = stress_periods,
        returns = returns
    )

    return(out)
}

ewma_vol <- function(returns, burn_in, lambda, mean = TRUE) {
    # load required packages
    require(zoo)
    # calculate return weights
    weight <- (1 - lambda) / (1 - (lambda^burn_in)) * ((lambda)^c(0:(burn_in - 1)))

    # calculate ewma mean (only use half a burn in period)
    ewma_mean <- rollapply(returns, burn_in,
        align = "left",
        fill = NA, FUN = function(x) {
            ewma_mean <- sum(weight * x)
            return(ewma_mean)
        }
    )

    # calculate ewma volatility
    out <- rollapply(
        tibble(returns = returns, ewma_mean = ewma_mean),
        burn_in,
        by.column = FALSE,
        align = "left",
        fill = NA,
        FUN = function(x) {

            if (mean) {
                vol <- sqrt(sum(weight * (x[, "returns"] - x[, "ewma_mean"])^2))
            } else {
                vol <- sqrt(sum(weight * (x[, "returns"])^2))
            }
            return(vol)
        }
    )

    return(out)
}

ewma_mean <- function(returns, burn_in, lambda) {
    # load required packages
    require(zoo)

    # calculate return weights
    weight <- (1 - lambda) / (1 - (lambda^burn_in)) * ((lambda)^c(0:(burn_in - 1)))
    # rolling calculation of volatility observations
    out <- rollapply(returns, burn_in,
        align = "left",
        fill = NA, FUN = function(x) {
            ewma_mean <- sum(weight * x)
            return(ewma_mean)
        }
    )

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
#' # store master file with master function defined above
#' master <- read_master(path)
#' # specify arguments in args list
#' args <- list(short = F, lambda = 0.975, MPOR = 3, n_day = 750, ...)
#' # Calculate Margin for the Instrument "FESX"
#' FHS_Margin <- calculate_FHS_margin("FESX", "01/01/2020", "01/05/2020", args)
calculate_fhs_margin <- function(product, start, end,
                                 args, steps = FALSE, abs = FALSE) {
    # load package dependencies
    require(dplyr)
    require(tidyr)
    require(zoo)
    require(purrr)
    require(runner)

    options(dplyr.summarise.inform = FALSE)
    # get returns of product from master
    df <-
        master$returns |>
        filter(INST == product) |>
        filter(DATE <= end) |>
        select(-INST) |>
        arrange(desc(DATE))

    # cutoff date + values needed for vola calculation
    cutoff <- max(which(df$DATE >= start)) + args$n_day + 2 * args$burn_in

    # adjusted cutoff that rows are divisible by MPOR
    adj_cutoff <- round(cutoff / args$MPOR) * args$MPOR

    # shorten return data frame (nrows) for faster calculation!
    df <- df[(1:adj_cutoff), ]

    # calculate MPOR day rolling returns, go from log returns to real returns
    # and add buckets to the days
    df <-
        df |>
        mutate(
            LOG_RET_MPOR = rollsum(x = df$LOG_RET, k = args$MPOR,
            fill = NA, align = "left"),
            RET_MPOR = exp(LOG_RET_MPOR) - 1,
            BUCKET = rep(seq_len(args$MPOR), length.out = nrow(df))
        ) |>
        slice(c(1:(n() - args$MPOR))) # delete last MPOR-1 observations (as NA)

    # invert returns for short positions (for summary stats calculation)
    if (args$short) {
        df <- df |>
            mutate(
                RET_MPOR = RET_MPOR * -1
            )
    }

    # nest by bucket, calculate rolling vola within each bucket
    df <- df |>
        nest(data = -BUCKET) |>
        mutate(
            EWMA_VOL = map(data, ~ ewma_vol(
                returns = .$LOG_RET_MPOR, burn_in = (args$burn_in / args$MPOR),
                lambda = args$lambda, mean = args$mean)),
            EWMA_MEAN = map(data, ~ ewma_mean(
                returns = .$LOG_RET_MPOR, burn_in = (args$burn_in / args$MPOR),
                lambda = args$lambda))
        ) |>
        unnest(everything()) |>
            # ensure there that length is always divisible by MPOR
            slice(1:(trunc(n() / args$MPOR) * args$MPOR)) |>
            arrange(desc(DATE)) |>
        drop_na(EWMA_VOL)

    df <- df |>
        mutate(
            deval = (LOG_RET_MPOR - lead(EWMA_MEAN, args$MPOR)) / lead(EWMA_VOL, args$MPOR, 1)
            #deval = lead(deval, 1)
        )

    margin_vector <- runner(
        df,
        na_pad = TRUE,
        lag = -(args$n_day) + 1,
        k = args$n_day,
        f = function(x) {
                margin <- x |>
                    mutate(EWMA_REVAL = mean(EWMA_VOL[1:args$MPOR])) |>
                    group_by(BUCKET) |>
                    summarize(
                        floor_revalue_fct = quantile(EWMA_VOL, .5, na.rm = TRUE, type = 2),
                        revalue_fct = max(floor_revalue_fct, EWMA_REVAL[1], na.rm = TRUE),
                        revalued = (exp(deval * revalue_fct) - 1) * args$factor,
                        margin = quantile(revalued, 1 - args$quantile, na.rm = TRUE, type = 2)
                    ) |>
                        pull(margin)
                    mean(margin)
                    return(mean(margin))
            }
    )

    df <- df |>
        mutate(MARGIN = margin_vector * -1)

    # return output
    if (steps) {
        return(df |>
            filter(between(DATE, start, end)))
    } else {
        return(df |> select(DATE, MARGIN) |>  filter(between(DATE, start, end)))
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

calculate_sp_margin <- function(product, start, end,
                                args, abs = FALSE) {
    # load required packages
    require(zoo)
    require(tidyr)
    require(data.table)

    # retrieve returns of product from masterfile
    returns <-
        master$returns |>
        filter(INST == product) |>
        select(-INST)

    # retrieve stress dates from masterfile
    stress_dates <-
        master$stress_periods |>
        filter(LIQ_GROUP == args$liq_group) |>
        select(-LIQ_GROUP) |>
        left_join(returns, by = c("DATE")) |>
        na.omit()

    # nest data according to stress period validity
    sp_var_df <- stress_dates |>
        nest(data = c(CONF_LEVEL, DATE, LOG_RET, PRICE))

    # add bucket columns and rolling 3 day returns
    sp_var_df <- sp_var_df |>
        mutate(
            LOG_RET_MPOR = map(data, function(x) {
                runner(
                    x = x, k = args$MPOR,
                    f = function(x) sum(x$LOG_RET),
                    na_pad = TRUE,
                    lag = -args$MPOR + 1
                )
            }),
            BUCKET = map(data, function(x) {
                rep(seq_len(args$MPOR), length.out = nrow(x))
            })
        ) |>
        unnest(everything())

    # inverse returns for short positions
    if (args$short) {
        sp_var_df <- sp_var_df |>
            mutate(LOG_RET_MPOR = LOG_RET_MPOR * -1)
    }

    # calculate VAR (resp. MARGIN) per vailidity level and bucket
    sp_var_df <- sp_var_df |>
        group_by(VALID_FROM, VALID_TO, BUCKET) |>
        summarize(
            MARGIN = quantile(LOG_RET_MPOR, 1 - CONF_LEVEL[1] / 100,
                na.rm = TRUE, type = 2
            )
        ) |>
        ungroup()

    # calculate mean var per validity level over all buckets
    sp_var_df <- sp_var_df |>
        group_by(VALID_FROM, VALID_TO) |>
        summarize(MARGIN = abs(mean((MARGIN)))) |>
        ungroup()

    # convert to data.table to use non-equi join functionality of data.table
    sp_var_df <- data.table(sp_var_df)
    returns <- data.table(returns)

    joined <- sp_var_df[returns, on = .(VALID_FROM <= DATE, VALID_TO > DATE)] |>
        rename(DATE = VALID_FROM) |>
        select(- VALID_TO)

    # conditional for absulute margin
    if (abs) {
        joined <- joined |> mutate(MARGIN = MARGIN * PRICE)
    }

    return(joined |> filter(between(DATE, start, end)))
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

calculate_margin <- function(product, start, end = NA, args,
                             steps = FALSE, abs = FALSE, unfloored_df = NULL) {
    # load required packages
    require(tidyr)
    if (is.null(unfloored_df)){
        fhs <- calculate_fhs_margin(
            product = product, start = start,
            end = end, args = args, steps = steps, abs = abs
        )
    } else {
        fhs <- unfloored_df
    }

    sp <- calculate_sp_margin(
        product = product, start = start,
        end = end, args = args, abs = abs
    )  |>  select(DATE, MARGIN)

    combined <- fhs |>
        inner_join(sp, by = c("DATE")) |>
        rename(FHS_MARGIN = MARGIN.x, SP_MARGIN = MARGIN.y)

    # larger of floor or FHS Margin
    combined$MARGIN <- pmax(combined$FHS_MARGIN, combined$SP_MARGIN)

    # conditional output
    if (steps) {
        return(combined)
    } else {
        return(combined |> select(DATE, MARGIN))
    }
}


#' Function Description
#' @param margins
#' @return
#' @examples

# we could add the liquidation period of the model as an attribute to the output?
# lag = 3 not accurate for other liquidation periods!! Take this into account!!!
summary_stats <- function(margin_df, start, end) {
    # load required packages
    require(tidyr)
    MPOR <- max(margin_df$BUCKET)
    margin_df <- margin_df |>
        filter(between(DATE, start, end)) |>
        mutate(
            RET_MPOR = lag(RET_MPOR, MPOR),
            CHANGE_1D = MARGIN / lead(MARGIN, 1),
            CHANGE_5D = MARGIN / lead(MARGIN, 5),
            CHANGE_30D = MARGIN / lead(MARGIN, 20)
        )

    n_observations <- length(na.omit(margin_df$RET_MPOR))
    n_breaches <- sum(margin_df$MARGIN < margin_df$RET_MPOR * -1, na.rm = TRUE)

    perc_breaches <- (n_breaches / n_observations)
    realized_conf_level <- 1 - perc_breaches

    # gives average shortfall in % not in absolute terms!!! (must check!!!)
    avg_shortfall <- margin_df |>
        filter(MARGIN < RET_MPOR * -1) |>
        mutate(shortfall = MARGIN + RET_MPOR) |>
        summarize(avg_shortfall = mean(shortfall)) |>
        pull(avg_shortfall)

    # calculate here as is done by eurex the max loss to margin ratio and not the max shortfall ratio!
    max_shortfall <- margin_df |>
        filter(MARGIN < RET_MPOR * -1) |>
        mutate(shortfall = MARGIN + RET_MPOR) |>
        summarize(max_shortfall = min(shortfall)) |>
        pull(max_shortfall)
    
    max_ltm <- margin_df |>
        filter(MARGIN < RET_MPOR * -1) |>
        mutate(LTM = (RET_MPOR * -1) / MARGIN) |>
        summarize(max_ltm = max(LTM, na.rm = TRUE)) |> 
        pull(max_ltm)

    avg_ltm <- margin_df |>
        filter(MARGIN < RET_MPOR * -1) |>
        mutate(LTM = (RET_MPOR * -1) / MARGIN) |>
        summarize(avg_ltm = mean(LTM, na.rm = TRUE)) |>
        pull(avg_ltm)

    costs <- mean(margin_df$MARGIN, na.rm = TRUE)

    # make sure that the max is after the min!!! (this is not currently the case!)
    peak_to_through <- max(margin_df$MARGIN, na.rm = TRUE) /
        min(margin_df$MARGIN, na.rm = T)

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
            "n_breaches", "N_observations", "perc_breaches",
            "conf_level", "avg_shortfall",
            "max_shortfall", "costs", "peak_to_through",
            "max_1d", "max_5d", "max_30d", "max_ltm", "avg_ltm"
        ),
        values = c(
            n_breaches, n_observations, perc_breaches,
            realized_conf_level, avg_shortfall, max_shortfall,
            costs, peak_to_through, max_1d, max_5d, max_30d, max_ltm, avg_ltm
        )
    )

    return(out)
}

#' result_only == will only return true/ false whether the test is passed or not
#' otherwise the entire data frame with pass / fail for each sub-window is returned!!!
#'

# Kupiec Proportion of Failure Stuff
kupiec_test <- function(margin_df, window, model_conf_level,test_conf_level) {

    # calculate vector for rolling kupiec_test statistics
    test <- rollapply(
        fill = NA,
        data = margin_df,
        width = window,
        by.column = FALSE,
        align = "left",
        FUN = function(x) {
            n_breaches <- sum(as.numeric(x[, "MARGIN"]) <
                as.numeric(x[, "RET_MPOR"]) * -1, na.rm = TRUE)
            perc_breaches <- (n_breaches / window)

            # Calculate Kupiec Proportion of Failure statistic
            factor_1 <- ((1 - perc_breaches) /
                (model_conf_level))^(window - n_breaches)
            factor_2 <- (perc_breaches / (1 - model_conf_level))^n_breaches
            result <- ifelse(perc_breaches == 0, 0, 2 * log(factor_1 * factor_2))

        chisq_statistic <- qchisq(test_conf_level, 1)
        out <- ifelse((
            result < chisq_statistic | perc_breaches < (1 - model_conf_level)),
            TRUE, FALSE
        )
        return(out)
        }
    )

    out <- all(test, na.rm = TRUE)
}

cap_margin <- function(margin_df, cap, floor){

    margin_df <- margin_df |>
        mutate(MARGIN = ifelse(MARGIN > cap, cap, MARGIN))
    return(margin_df)
}

buffer_margin <- function(margin_df, buffer, release){

        margin_df <- margin_df |>
            mutate(
                MARGIN = pmax(pmin((1 + buffer) * MARGIN, (1 + buffer) * release), MARGIN)
            )
        
    return(margin_df)
}


speed_limit <- function(margin_df, n_day, limit){

      margin_df <- margin_df |>
        arrange(DATE)

    # initialize vectors
    breach <- vector()
    delta_nday  <- vector()
    margin_act <- margin_df$MARGIN
    margin_limit <- margin_df$MARGIN

    # loop over values
    for (i in (n_day + 1):length(margin_limit)) {

        delta_nday[i] <- ifelse(is.na(margin_limit[i] / margin_limit[i - n_day]), 0, margin_limit[i] / margin_limit[i - n_day])
        breach[i] <- ifelse(delta_nday[i] > limit, TRUE, FALSE)

        # if speed limit is breched
        if (breach[i]) {
            margin_limit[i] <- margin_limit[i - n_day] * limit
        } else {
            margin_limit[i] <- min(margin_limit[i - n_day] * limit, margin_act[i])
        }
        i <- i + 1
        print(i)
    }

    margin_df <- margin_df |>
        mutate(MARGIN = margin_limit) |>
        arrange(desc(DATE))

    return(margin_df)
}

plot_theme <- function(){
    require(ggplot2)
    theme <- theme_bw()
    theme_set(theme)
    theme_update(
            text = element_text(family = "lmroman"),
            plot.title = element_text(
                size = 10, face = "bold", hjust = 0,
                margin = margin(t = 0, l = 0, r = 0, b = 10)
            ),
            plot.subtitle = element_text(size = 8, face = "italic"),
            plot.caption = element_text(size = 7, hjust = 1),
            plot.margin = margin(0, 0, 0, 0),
            panel.background = element_rect(color = "black", fill = "white"),
            panel.grid.major = element_blank(),
            legend.text = element_text(size = 7),
            legend.title = element_text(size = 7),
            legend.position = "bottom",
            legend.box.spacing = unit(0, "cm"),
            legend.key = element_rect(fill = "white", color = "white"),
            axis.title = element_text(size = 7),
            axis.text = element_text(size = 6),
            strip.background = NULL,
            strip.text = NULL
        )
}

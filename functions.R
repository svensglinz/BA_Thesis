#' read_master() imports the master excel-sheet where all
#' essential information for the calculation of margins is stored and properly
#' formats it for further use in the below functions (instrument returns, stress period dates)
#' @param path local path where excel sheet with returns is stored

read_master <- function(path) {
    # load required packages
    library(readxl)
    stress_periods <- read_excel(path, sheet = "stress_periods") |>
        select(1:4)
    stress_periods[1] <- as.Date(stress_periods[[1]], format = "%d/%m/%Y")

    returns <- read_excel(path, sheet = "returns") |>
        select(1:4)
    returns[1] <- as.Date(returns[[1]], format = "%d/%m/%Y")

    out <- list(
        stress_periods = stress_periods,
        returns = returns
    )

    return(out)
}

#' ewma_vol() calculates the daily exponentially weighted volatility of a return vector
#' @param returns numerical vector of a financial return series (from newest to oldest observation)
#' @param burn_in amount of days considered for volatility calculation
#' @param lambda decay factor
#' @param mean indicates whether the EWMA volatility should be calculated using a mean (TRUE) or whether
#' the mean is assumed to be zero (FALSE)

ewma_vol <- function(returns, burn_in, lambda, mean = TRUE) {
    # load required packages
    library(zoo)
    # calculate return weights
    weight <- (1 - lambda) / (1 - (lambda^burn_in)) * ((lambda)^c(0:(burn_in - 1)))

    # calculate ewma mean
    ewma_mean <- rollapply(returns, burn_in,
        align = "left",
        fill = NA,
        FUN = \(x) {
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
        FUN = \(x) {
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

#' ewma_mean() calculates the daily exponentially weighted mean of a vector of returns
#' @param returns numerical vector of a financial return series (from newest to oldest observation)
#' @param burn_in amount of days considered for EWMA-mean calculation
#' @param lambda decay factor

ewma_mean <- function(returns, burn_in, lambda) {
    # load required packages
    library(zoo)

    # calculate return weights
    weight <- (1 - lambda) / (1 - (lambda^burn_in)) * ((lambda)^c(0:(burn_in - 1)))
    # rolling calculation of daily ewma mean
    out <- rollapply(returns, burn_in,
        align = "left",
        fill = NA,
        FUN = \(x) {
            ewma_mean <- sum(weight * x)
            return(ewma_mean)
        }
    )

    return(out)
}

#' calculate_fhs_margin() calculates the initial margin using a filtered
#' historical simulation model
#' @param product name of product (STRING) for which margin should be calculated.
#' Name must be in column INST of the masterfile
#' @param start Start period (DATE) for which margin should be calculated
#' @param end End period (DATE) for which margin should be calculated
#' @param steps indicates whether intermediary results (log returns, volatilities etc.)
#' should be displayed in the output df or not (set = TRUE for procyclicality metric calculations)
#' @param args list with model parameters which are needed to calculate the margin
#' MPOR (numeric), factor (numeric), quantile (numeric), lambda (numeric), n_day (numeric),
#' burn_in (numeric), liq_group (character), short (boolean), mean (boolean)

calculate_fhs_margin <- function(product, start, end, args, steps = FALSE) {
    # load package dependencies
    library(dplyr)
    library(tidyr)
    library(zoo)
    library(purrr)
    library(runner)

    options(dplyr.summarise.inform = FALSE)

    # get returns of product from master
    df <- master$returns |>
        filter(INST == product) |>
        filter(DATE <= end) |>
        select(-INST) |>
        arrange(desc(DATE))

    # cutoff date + values needed for vola calculation
    cutoff <- max(which(df$DATE >= start)) + args$n_day + 2 * args$burn_in

    # adjusted cutoff that rows are divisible by MPOR
    adj_cutoff <- round(cutoff / args$MPOR) * args$MPOR

    # shorten return data frame (nrows) for faster calculation
    df <- df[(1:adj_cutoff), ]

    # calculate MPOR day rolling returns and add buckets to the days
    df <- df |>
        mutate(
            LOG_RET_MPOR = rollsum(
                x = df$LOG_RET, k = args$MPOR,
                fill = NA, align = "left"
            ),
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
            EWMA_VOL = map(data, \(x) ewma_vol(
                returns = x$LOG_RET_MPOR, burn_in = (args$burn_in / args$MPOR),
                lambda = args$lambda, mean = args$mean
            )),
            EWMA_MEAN = map(data, \(x) ewma_mean(
                returns = x$LOG_RET_MPOR, burn_in = (args$burn_in / args$MPOR),
                lambda = args$lambda
            ))
        ) |>
        unnest(everything()) |>
        # ensure there that length is always divisible by MPOR
        slice(1:(trunc(n() / args$MPOR) * args$MPOR)) |>
        arrange(desc(DATE)) |>
        drop_na(EWMA_VOL)

    # create devalued returns
    df <- df |>
        mutate(deval = (LOG_RET_MPOR - lead(EWMA_MEAN, args$MPOR)) / lead(EWMA_VOL, args$MPOR))

    # calculate margin


    margin_vector <- runner(
        df,
        na_pad = TRUE,
        lag = -(args$n_day) + 1,
        k = args$n_day,
        f = \(x) {
            margin <- x |>
                mutate(EWMA_REVAL = mean(EWMA_VOL[1:args$MPOR])) |>
                group_by(BUCKET) |>
                summarize(
                    floor_revalue_fct = quantile(EWMA_VOL, .5, na.rm = TRUE, type = 2),
                    revalue_fct = max(floor_revalue_fct, EWMA_REVAL[1], na.rm = TRUE),
                    revalued = (exp(deval * revalue_fct) - 1) * args$factor,
                    margin = ifelse(
                        args$short,
                        quantile(revalued, args$quantile, na.rm = TRUE, type = 2),
                        quantile(revalued, 1 - args$quantile, na.rm = TRUE, type = 2)
                    )
                ) |>
                pull(margin)
            return(mean(margin, na.rm = TRUE))
        }
    )

    # append absolute value of margin to the df
    df <- df |>
        mutate(MARGIN = abs(margin_vector))

    # return output
    if (steps) {
        return(df |>
            filter(between(DATE, start, end))) # filter again for accuracy
    } else {
        return(df |> select(DATE, MARGIN) |> filter(between(DATE, start, end)))
    }
}

#' calculate_sp_margin() calculates the floored margin based on stress dates which
#' are noted in the master file
#' @param product name of product (CHARACTER) for which margin should be calculated.
#' Name must be in column INST of the masterfile
#' @param start Start period (DATE) for which margin should be calculated
#' @param end End period (DATE) for which margin should be calculated
#' @param args list with model parameters which are needed to calculate the margin
#' MPOR (numeric), factor (numeric), quantile (numeric), lambda (numeric), n_day (numeric),
#' burn_in (numeric), liq_group (character), short (boolean), mean (boolean)

calculate_sp_margin <- function(product, start, end, args) {
    # load required packages
    library(zoo)
    library(tidyr)

    # retrieve returns of product from masterfile
    returns <- master$returns |>
        filter(INST == product) |>
        select(-INST)

    # retrieve stress dates from masterfile
    df <- master$stress_periods |>
        filter(LIQ_GROUP == args$liq_group) |>
        select(-LIQ_GROUP) |>
        left_join(returns, by = c("DATE")) |>
        arrange(desc(DATE)) |>
        drop_na(LOG_RET)

    # add bucket columns and rolling 3 day returns
    df <- df |>
        nest(data = -GROUP) |>
        mutate(
            LOG_RET_MPOR = map(data, \(x) rollsum(
                x = x$LOG_RET, k = args$MPOR,
                fill = NA, align = "left"
            )),
            BUCKET = map(data, \(x) rep(seq_len(args$MPOR), length.out = nrow(x)))
        ) |>
        unnest(everything()) |>
        drop_na(LOG_RET_MPOR)

    # calculate VAR (resp. MARGIN)
    sp_margin <- df |>
        group_by(BUCKET) |>
        summarize(
            MARGIN = ifelse(
                args$short,
                quantile(LOG_RET_MPOR, CONF_LEVEL[1] / 100, na.rm = F, type = 2),
                quantile(LOG_RET_MPOR, 1 - CONF_LEVEL[1] / 100, na.rm = F, type = 2)
            )
        ) |>
        mutate(MARGIN = abs(mean(exp(MARGIN) - 1))) |>
        pull(MARGIN) |>
        unique()

    out <- returns |>
        mutate(MARGIN = sp_margin) |>
        select(DATE, MARGIN) |>
        filter(between(DATE, start, end))

    return(out)
}

#' calculate_margin() is a shortcut for max(FHS_Margin, SP_Margin)
#' @param product name of product (string) for which margin should be calculated.
#' Name must be in column INST of the masterfile
#' @param start Start period (DATE) for which margin should be calculated
#' @param end End period (DATE) for which margin should be calculated
#' @param args list with model parameters which are needed to calculate the margin
#' MPOR (numeric), factor (numeric), quantile (numeric), lambda (numeric), n_day (numeric),
#' burn_in (numeric), liq_group (character), short (boolean), mean (boolean)
#' @param steps indicates whether intermediary results (log returns, volatilities etc.)
#' should be displayed in the output df or not (set = TRUE for procyclicality metric calculations)
#' @param unfloored_df if the fhs margin data frame was already computed, it can be specified here
#' as this prevents the function from calculating the fhs margin again --> better performance

calculate_margin <- function(product, start, end, args,
                             steps = FALSE, unfloored_df = NULL) {
    # load required packages
    library(tidyr)

    # calculates fhs margin only if unfloored_df is not supplied
    if (is.null(unfloored_df)) {
        fhs <- calculate_fhs_margin(product = product, start = start, end = end, args = args, steps = steps)
    } else {
        fhs <- unfloored_df
    }

    sp <- calculate_sp_margin(product = product, start = start, end = end, args = args) |>
        select(DATE, MARGIN)

    combined <- fhs |>
        inner_join(sp, by = c("DATE")) |>
        rename(FHS_MARGIN = MARGIN.x, SP_MARGIN = MARGIN.y)

    # larger of floored or fhs margin
    combined$MARGIN <- pmax(combined$FHS_MARGIN, combined$SP_MARGIN)

    # conditional output
    if (steps) {
        return(combined)
    } else {
        return(combined |> select(DATE, MARGIN))
    }
}


#' summary_stats() calculates various procyclicality and coverage stats for a supplied
#' margin data frame
#' @param margin_df a margin data frame that was created by using the functions
#' calculate_fhs_margin() or calculate_margin(). Steps must have been set to TRUE
#' @param start Start date (DATE) of interval for which stats should be calculated
#' @param end End date (DATE) of interval for which stats should be calculated

summary_stats <- function(margin_df, start, end) {
    # load required packages
    library(tidyr)
    MPOR <- max(margin_df$BUCKET)
    margin_df <- margin_df |>
        filter(between(DATE, start, end)) |>
        mutate(
            RET_MPOR = lag(RET_MPOR, MPOR), # lag MPOR returns for analysis of breaches below
            CHANGE_1D = MARGIN / lead(MARGIN, 1),
            CHANGE_5D = MARGIN / lead(MARGIN, 5),
            CHANGE_30D = MARGIN / lead(MARGIN, 20)
        )

    n_observations <- length(na.omit(margin_df$RET_MPOR))
    n_breaches <- sum(margin_df$MARGIN < margin_df$RET_MPOR * -1, na.rm = TRUE)

    perc_breaches <- (n_breaches / n_observations)
    realized_conf_level <- 1 - perc_breaches

    # avg and max shorfall in % of notional
    avg_shortfall <- margin_df |>
        filter(MARGIN < RET_MPOR * -1) |>
        mutate(shortfall = MARGIN + RET_MPOR) |>
        summarize(avg_shortfall = mean(shortfall)) |>
        pull(avg_shortfall)

    max_shortfall <- margin_df |>
        filter(MARGIN < RET_MPOR * -1) |>
        mutate(shortfall = MARGIN + RET_MPOR) |>
        summarize(max_shortfall = min(shortfall)) |>
        pull(max_shortfall)

    # avg and max loss to margin
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

    peak_to_through <- rollapply(
        margin_df$MARGIN,
        by = 1,
        width = length(margin_df$MARGIN),
        partial = TRUE, align = "left",
        FUN = \(x) max(x[1] / min(x))
    )
    peak_to_through <- max(peak_to_through, na.rm = TRUE)

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

#' kupiec_test() performs the kupiec proportion of failure test on the margin_df
#' @param margin_df a margin data frame that was created by using the functions
#' calculate_fhs_margin() or calculate_margin(). Steps must have been set to TRUE
#' @param window window (in days) over which the test should be performed
#' @param model_conf_level confidence level of the margin model that is tested
#' @param test_conf_level quantile which must be surpassed to reject the KPF test

kupiec_test <- function(margin_df, window, model_conf_level, test_conf_level) {
    # extract MPOR from margin_df
    MPOR <- max(margin_df$MPOR)

    # lag returns to analyze number of breaches
    margin_df <- margin_df |>
        mutate(RET_MPOR = lag(RET_MPOR, MPOR))

    x <- as.zoo(margin_df)
    # calculate vector for rolling kupiec_test statistics (T/F for each sub-window)
    test <- rollapply(
        fill = NA,
        data = margin_df,
        width = window,
        by.column = FALSE,
        align = "left",
        FUN = \(x) {
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

    # true if all sub-windows == TRUE, else false
    out <- all(test, na.rm = TRUE)
}

#' cap_margin() floors and caps the column MARGIN in margin_df
#' @param margin_df a margin data frame that was created by using the functions
#' calculate_fhs_margin() or calculate_margin(). Steps must have been set to TRUE
#' @param cap the value where the margin should be capped at

cap_margin <- function(margin_df, cap) {
    margin_df <- margin_df |>
        mutate(MARGIN = ifelse(MARGIN > cap, cap, MARGIN))
    return(margin_df)
}

#' buffer_margin() implements the EMIR Buffer APC on the MARGIN column of the margin_df
#' @param margin_df a margin data frame that was created by using the functions
#' calculate_fhs_margin() or calculate_margin(). Steps must have been set to TRUE
#' @param buffer buffer to be added to the margin in %
#' @param release margin above which the buffer should be released

buffer_margin <- function(margin_df, buffer, release) {
    margin_df <- margin_df |>
        mutate(
            MARGIN = pmax(pmin((1 + buffer) * MARGIN, release), MARGIN)
        )

    return(margin_df)
}

#' speed_limit() implements the speed limit APC on the MARGIN column of the margin_df
#' @param margin_df a margin data frame that was created by using the functions
#' calculate_fhs_margin() or calculate_margin(). Steps must have been set to TRUE
#' @param n_day specifies the days over which the max margin change cannot go above "limit"
#' @param limit the maximum permissible margin change over n days

speed_limit <- function(margin_df, n_day, limit) {
    margin_df <- margin_df |>
        arrange(DATE)

    # initialize vectors
    breach <- vector()
    delta_nday <- vector()
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
    }

    margin_df <- margin_df |>
        mutate(MARGIN = margin_limit) |>
        arrange(desc(DATE))

    return(margin_df)
}

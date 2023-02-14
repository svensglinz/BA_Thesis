# load relevant packages
library(tidyverse)
library(glue)
source("functions.r")

master <- read_master("Data/data_input.xlsx")

# define function paremeters (FESX)
start_all <- as.Date("2001-03-20")
end_all <- as.Date("2023-01-01")
start_covid <- as.Date("2020-01-01")
end_covid <- as.Date("2020-12-31")
start_fc <- as.Date("2007-06-01")
end_fc <- as.Date("2009-03-31")
start_dotcom <- as.Date("2001-03-20")
end_dotcom <- as.Date("2003-04-01")
start_regular <- as.Date("2007-04-20")

periods <- list(
    all = tibble(start = start_all, end = end_all, period = "all"),
    covid = tibble(start = start_covid, end = end_covid, period = "covid"),
    financialcrisis = tibble(start = start_fc, end = end_fc, period = "financialcrisis"),
    dotcom = tibble(start = start_dotcom, end = end_dotcom, period = "dotcom")
)

# parameters for the Kupiec Test of Failure Function
window <- 750
model_conf_level <- .99
test_conf_level <- .99

# store parameters needed for margin calculation
args_short_FESX <- list(
    MPOR = 3, factor = 1.37, quantile = 0.978,
    lambda = NULL, n_day = 750, burn_in = 750,
    liq_group = "PEQ01", short = TRUE,
    mean = TRUE
)

args_short_FESX_dotcom <- list(
    MPOR = 3, factor = 1.37, quantile = 0.978,
    lambda = NULL, n_day = 500, burn_in = 200,
    liq_group = "PEQ01", short = TRUE,
    mean = FALSE
)

# define lambdas to loop over
lambda_loop <- seq(0.9, 0.995, by = 0.005)
measures <- tibble(NULL)
count <- 1

for (lambda in lambda_loop) {
    # assign lambda to the args list
    args_short_FESX$lambda <- lambda
    args_short_FESX_dotcom$lambda <- lambda

    # unmitigated margin
    margin_baseline_regular <-
        calculate_fhs_margin(
            product = "FESX", start = start_regular, end = end_all,
            args = args_short_FESX, steps = TRUE
        )

    # unmitigated margin during dotcom / calculate without mean
    margin_baseline_dotcom <- calculate_fhs_margin(
        product = "FESX", start = start_dotcom, end = start_regular - 1,
        args = args_short_FESX_dotcom, steps = TRUE
    )

    # join baseline margins
    margin_baseline <- bind_rows(margin_baseline_regular, margin_baseline_dotcom) |>
        drop_na()

    # floored margin
    margin_floor_regular <-
        calculate_margin(
            product = "FESX", start = start_regular, end = end_all,
            args = args_short_FESX, steps = TRUE, unfloored_df = margin_baseline
        )

    # floored margin during dotcom
    margin_floor_dotcom <- calculate_margin(
        product = "FESX", start = start_dotcom, end = start_regular - 1,
        args = args_short_FESX_dotcom, steps = TRUE, unfloored_df = margin_baseline_dotcom
    )

    # join baseline margins
    margin_floor <- bind_rows(margin_floor_regular, margin_floor_dotcom) |>
        drop_na()

    # Capped / floored & capped Margin
    cap <- quantile(margin_floor$MARGIN, .95, na.rm = TRUE)[[1]]
    margin_cap <- cap_margin(margin_baseline, cap = cap)
    margin_cap_floor <- cap_margin(margin_floor, cap = cap)

    # buffered margin
    release <- quantile(margin_baseline$MARGIN, .99, na.rm = TRUE) / 1.25
    margin_buffer <- buffer_margin(margin_baseline, buffer = .25, release = release)

    # speed & floor
    limit <- quantile(margin_baseline$MARGIN / lead(margin_baseline$MARGIN, 20), .99, na.rm = TRUE)[[1]]
    margin_speed_floor <- speed_limit(margin_floor, n_day = 20, limit = limit)
    margin_speed <- speed_limit(margin_baseline, n_day = 20, limit = limit)

    # run Kupiec Test and discard those that do not meet test
    kpf_baseline <- kupiec_test(margin_baseline,
        window = 750, model_conf_level = model_conf_level,
        test_conf_level = test_conf_level
    )
    kpf_floor <- kupiec_test(margin_floor,
        window = 750, model_conf_level = model_conf_level,
        test_conf_level = test_conf_level
    )
    kpf_speed <- kupiec_test(margin_speed,
        window = 750, model_conf_level = model_conf_level,
        test_conf_level = test_conf_level
    )
    kpf_speed_floor <- kupiec_test(margin_speed_floor,
        window = 750, model_conf_level = model_conf_level,
        test_conf_level = test_conf_level
    )
    kpf_cap <- kupiec_test(margin_cap,
        window = 750, model_conf_level = model_conf_level,
        test_conf_level = test_conf_level
    )
    kpf_cap_floor <- kupiec_test(margin_cap_floor,
        window = 750, model_conf_level = model_conf_level,
        test_conf_level = test_conf_level
    )
    kpf_buffer <- kupiec_test(margin_buffer,
        window = 750, model_conf_level = model_conf_level,
        test_conf_level = test_conf_level
    )
    kpf_test <- tibble(
        type = "kpf",
        values = c(
            kpf_baseline, kpf_floor, kpf_cap, kpf_speed, kpf_buffer,
            kpf_speed_floor, kpf_cap_floor
        ),
        model = c("baseline", "floor", "cap", "speed", "buffer", "speed_floor", "cap_floor"),
        period = "all"
    )

    temp_summary_stats <- tibble(NULL)
    for (i in periods) {
        temp_baseline <-
            summary_stats(margin_baseline, start = i[["start"]], end = i[["end"]]) |>
            mutate(model = "baseline", period = i[["period"]])
        temp_floor <-
            summary_stats(margin_floor, start = i[["start"]], end = i[["end"]]) |>
            mutate(model = "floor", period = i[["period"]])
        temp_cap <-
            summary_stats(margin_cap, start = i[["start"]], end = i[["end"]]) |>
            mutate(model = "cap", period = i[["period"]])
        temp_speed <-
            summary_stats(margin_speed, start = i[["start"]], end = i[["end"]]) |>
            mutate(model = "speed", period = i[["period"]])
        temp_buffer <-
            summary_stats(margin_buffer, start = i[["start"]], end = i[["end"]]) |>
            mutate(model = "buffer", period = i[["period"]])
        temp_speed_floor <-
            summary_stats(margin_speed_floor, start = i[["start"]], end = i[["end"]]) |>
            mutate(model = "speed_floor", period = i[["period"]])
        temp_cap_floor <-
            summary_stats(margin_cap_floor, start = i[["start"]], end = i[["end"]]) |>
            mutate(model = "cap_floor", period = i[["period"]])

        temp_summary_stats <- bind_rows(
            temp_summary_stats,
            temp_baseline, temp_speed, temp_floor, temp_cap,
            temp_buffer, temp_cap_floor, temp_speed_floor
        )
    }

    temp <- bind_rows(temp_summary_stats, kpf_test) |>
        mutate(lambda = lambda)

    measures <- measures |>
        bind_rows(temp)

    print(glue("loop {count} / {length(lambda_loop)} finished"))
    count <- count + 1
}

write_csv(measures, "Data/calculations_fesx_short.csv")

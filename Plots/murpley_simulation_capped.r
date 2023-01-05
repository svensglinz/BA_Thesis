# load relevant packages
library(tidyverse)
library(scales)
library(ggsci)
library(showtext)

# add font for plotting
font_add(
    family = "lmroman",
    regular = "Fonts/lmroman10_regular.ttf",
    bold = "Fonts/lmroman10_bold.ttf",
    italic = "Fonts/lmroman10_italic.ttf",
    bolditalic = "Fonts/lmroman10_bolditalic.ttf"
)

showtext_auto(enable = TRUE)
showtext_opts(dpi = 350)

# import written functions and store master sheet in memory
source("functions.R")
master <- read_master("Data/data_input.xlsx")
plot_theme()

# define function paremeters
start_date <- as.Date("2006-01-01")
end_date <- as.Date("2021-01-01")
start_2020 <- as.Date("2020-01-01")
end_2020 <- as.Date("2020-12-31")
# parameters for the Kupiec Test of Failure Function
window <- 750
model_conf_level <- .99
test_conf_level <- .99

# store parameters needed for margin calculation
args_long_FESX <-
    list(
        MPOR = 3, factor = 1.37, quantile = 0.974,
        lambda = NULL, n_day = 750, floor = FALSE,
        absolute = FALSE, liq_group = "PEQ01",
        short = FALSE
    )

# define lambdas to loop over
lambda_loop <- seq(0.9, 0.995, by = 0.005)
measures <- tibble(NULL)
count <- 1

for (i in lambda_loop) {
    # assign lambda to the args list
    args_long_FESX$lambda <- i

    # unmitigated margin
    margin_baseline <-
        calculate_fhs_margin(
            product = "FESX", start = start_date, end = end_date,
            args = args_long_FESX, steps = TRUE
        )

    # floored margin
    margin_floor <-
        calculate_margin(
            product = "FESX", start = start_date, end = end_date,
            args = args_long_FESX, steps = TRUE
        )

    # floored & capped margin
    cap <- quantile(margin_floor$MARGIN, .95, na.rm = TRUE)[[1]]
    margin_cap <- cap_margin(margin_floor, cap = cap, floor = 0)

    # buffered margin
    release <- quantile(margin_baseline$MARGIN, .99, na.rm = TRUE) / 1.25
    margin_buffer <- buffer_margin(margin_baseline, buffer = .25, release = release)

    # speed limit margin --> What we limit is the 99.5th percentile of 20day changes
    # quantile(margin_baseline$MARGIN / lead(margin_baseline$MARGIN, 20), .995,  na.rm = TRUE)
    margin_speed <- speed_limit(margin_baseline, n_day = 20, limit = 1.8)

    # run Kupiec Test and discard those that do not meet test
    kpf_baseline <- kupiec_test(margin_baseline,
        window = 750, model_conf_level = model_conf_level,
        test_conf_level = test_conf_level
    )
    kpf_floor <- kupiec_test(margin_floored,
        window = 750, model_conf_level = model_conf_level,
        test_conf_level = test_conf_level
    )
    kpf_speed <- kupiec_test(margin_speed,
        window = 750, model_conf_level = model_conf_level,
        test_conf_level = test_conf_level
    )
    kpf_cap <- kupiec_test(margin_cap,
        window = 750, model_conf_level = model_conf_level,
        test_conf_level = test_conf_level
    )
    kpf_buffer <- kupiec_test(margin_buffer,
        window = 750, model_conf_level = model_conf_level,
        test_conf_level = test_conf_level
    )
    kpf_test <- tibble(
        type = "kpf",
        values = c(kpf_baseline, kpf_floor, kpf_cap, kpf_speed, kpf_buffer),
        model = c("baseline", "floor", "cap", "speed", "buffer"),
        period = "all"
    )

    temp_baseline <- summary_stats(margin_baseline, start_date, end_date) |>
        mutate(model = "baseline", period = "all")
    temp_floor <- summary_stats(margin_floor, start_date, end_date) |>
        mutate(model = "floor", period = "all")
    temp_cap <- summary_stats(margin_cap, start_date, end_date) |>
        mutate(model = "cap", period = "all")
    temp_speed <- summary_stats(margin_speed, start_date, end_date) |>
        mutate(model = "speed", period = "all")
    temp_buffer <- summary_stats(margin_buffer, start_date, end_date) |>
        mutate(model = "buffer", period = "all")

    temp_baseline_2020 <-
        summary_stats(margin_baseline, start = start_2020, end = end_2020) |>
        mutate(model = "baseline", period = "2020")
    temp_floor_2020 <-
        summary_stats(margin_floor, start = start_2020, end = end_2020) |>
        mutate(model = "floor", period = "2020")
    temp_cap_2020 <-
        summary_stats(margin_cap, start = start_2020, end = end_2020) |>
        mutate(model = "cap", period = "2020")
    temp_speed_2020 <-
        summary_stats(margin_speed, start = start_2020, end = end_2020) |>
        mutate(model = "speed", period = "2020")
    temp_buffer_2020 <-
        summary_stats(margin_buffer, start = start_2020, end = end_2020) |>
        mutate(model = "buffer", period = "2020")


    temp <- bind_rows(
        temp_baseline, temp_speed, temp_floor, temp_cap,
        temp_buffer, temp_baseline_2020, temp_speed_2020,
        temp_floor_2020, temp_cap_2020, temp_buffer_2020,
        kpf_test
    ) |>
        mutate(lambda = i)

    measures <- measures |>
        bind_rows(temp)

    print(
        paste(
            "loop", as.character(count), "/",
            as.character(length(lambda_loop)), "finished",
            sep = " "
        )
    )
    count <- count + 1
}

measures <- measures |>
    pivot_wider(names_from = type, values_from = values)

measures <- measures |>
    mutate(plot_label = case_when(
        lambda == .93 & kpf == 1 ~ model,
        NA ~ NA_character_,
        TRUE ~ NA_character_
    ))

plot_theme()
tibble(NULL) |>
    ggplot(aes(y = max_30d - 1, x = costs, shape = model)) +
    geom_point(
        aes(alpha = lambda),
        data = measures |> filter(period == "all" & lambda != .96), size = 1
    ) +
    geom_point(
        data = measures |> filter(lambda == .96, kpf == 1),
        color = "red", size = 2, show.legend = FALSE
    ) +
    geom_text_repel(
        data = measures |> filter(period == "all"), size = 2, direction = "y",
        min.segment.length = 10, nudge_y = .2, color = "black",
        aes(label = plot_label),
        show.legend = FALSE
    ) +
    guides(shape = "none") +
    labs(
        title = "Comparison of APC Tools"
    )

ggsave(
    "Plots/Output/combined_murphey.png", last_plot(),
    width = 12, height = 8.4, unit = "cm"
)

measures |>
    filter(lambda == .96) |>
    view()

new <-
    theme_update(theme_bw())

measures |>
    filter(kpf & model == "speed" & period == "all") |>
    ggplot(aes(x = round(costs * 100, 1), color = lambda)) +
    geom_point(aes(y = max_30d)) +
    geom_point(aes(y = peak_to_through)) +
    geom_point(
        data = measures |> filter(kpf & model == "speed" & period == "all" & lambda == .96),
        aes(y = max_30d),
        color = "red"
    ) +
    geom_point(
        data = measures |> filter(kpf & model == "speed" & period == "all" & lambda == .96),
        aes(y = peak_to_through),
        color = "red"
    ) +
    scale_color_gradientn(
        colors = c("white", "grey", "darkgrey", "black"),
        breaks = c(.9, .95, .99)
    ) +
    scale_x_continuous(
        breaks = seq(7, 11, by = .2)
    ) +
    labs(
        title = "30-day Procycl. - Cap",
        x = "Avg. Costs (%)",
        y = NULL,
        color = expression(lambda)
    ) +
    guides(
        fill = guide_colorbar(
            barheight = .5,
            title.position = "left",
            title.vjust = 1
        )
    )

ggsave(
    "Plots/Output/capped.png", cap,
    width = 6.48, height = 5.45, unit = "cm"
)
tibble(NULL) |>
    ggplot(aes(x = NULL)) +
    geom_line(aes(x = DATE, y = MARGIN), data = margin_baseline) +
    geom_line(aes(x = DATE, y = MARGIN), data = margin_buffer, color = "red")
# xlim(c(start_2020, end_2020))

a <- measures |>
    select(-plot_label) |>
    filter(kpf == 1 | period == "2020") |>
    pivot_longer(!c(model, period, lambda), names_to = "type", values_to = "values") |>
    filter(type %in% c("n_breaches", "avg_shortfall", "max_shortfall")) |>
    filter((period == "2020" & type == "n_breaches") | period == "all") |>
    mutate(type = ifelse(period == "2020", "n_breaches_2020", type))

# baseline model
a <- a |>
    filter(model == "baseline") |>
    mutate(values = ifelse(type %in% c("n_breaches", "n_breaches_2020"), values / 4, values * -100))
a |>
    ggplot(aes(x = lambda, y = values, linetype = type)) +
    geom_line(linewidth = .3) +
    labs(
        y = "Shortfall (%)",
        title = "Shortfall & Coverage",
        linetype = NULL,
        caption = "baseline"
    ) +
    geom_text_repel(data = a |> filter(model == "baseline" & lambda == .90), nudge_y = .001, direction = "y", hjust = 0, aes(label = type), size = 1.7) +
    scale_y_continuous(
        sec.axis = sec_axis(trans = \(x) x * 4, name = "Breaches"), expand = expansion(mult = c(.04, .1))
    ) +
    guides(linetype = "none")

ggsave(
    "Plots/Output/risks_baseline.png", last_plot(),
    width = 8.55, height = 7.2, unit = "cm"
)

# capped model
a <- a |>
    filter(model == "cap") |>
    mutate(values = ifelse(type %in% c("n_breaches", "n_breaches_2020"), values / 4, values * -100))

a |>
    ggplot(aes(x = lambda, y = values, linetype = type)) +
    geom_line(linewidth = .3) +
    labs(
        y = "Shortfall (%)",
        title = "Shortfall & Coverage",
        linetype = NULL,
        caption = "cap"
    ) +
    geom_text_repel(
        data = a |> filter(model == "cap" & lambda == .90), nudge_y = .001,
        direction = "y", hjust = 0, aes(label = type), size = 1.7
    ) +
    scale_y_continuous(
        sec.axis = sec_axis(trans = \(x) x * 4, name = "Breaches"), expand = expansion(mult = c(.04, .1))
    ) +
    guides(linetype = "none")

ggsave(
    "Plots/Output/risks_cap.png", last_plot(),
    width = 8.55, height = 7.2, unit = "cm"
)

# speed model
# capped model
a <- a |>
    mutate(values = ifelse(type %in% c("n_breaches", "n_breaches_2020"), values / 4, values * -100))

a |>
    ggplot(aes(x = lambda, y = values, linetype = type)) +
    geom_line(linewidth = .3) +
    labs(
        y = "Shortfall (%)",
        title = "Shortfall & Coverage",
        linetype = NULL,
        caption = "speed"
    ) +
    # geom_text_repel(
    #    data = a  |> filter(lambda == .90), nudge_y = .001,
    #    direction = "y", hjust = 0, aes(label = type), size = 1.7
    # ) +
    scale_y_continuous(
        sec.axis = sec_axis(trans = \(x) x * 4, name = "Breaches"), expand = expansion(mult = c(.04, .1))
    ) +
    # guides(linetype = "none") +
    facet_wrap(~model) +
    theme(
        strip.background = element_rect(color = "black", fill = "white"),
        strip.text = element_text(size = 7)
    )

ggsave(
    "Plots/Output/risks_total.png", last_plot(),
    width = 17, height = 11.5, unit = "cm"
)


# calculation of untiltered historical margin vs. stress floor implemented by eurex!
rets <- margin_baseline |>
    group_by(BUCKET) |>
    summarize(quant = quantile(RET_MPOR, .974, na.rm = TRUE) * 1.37)
pull(RET_MPOR) |>
    mutate(RET = exp(LOG_RET) - 1) |>
    pull(RET)

margin_floor |>
    group_by(MARGIN) |>
    summarize(n())
quantile(rets * -1, .99, na.rm = TRUE)

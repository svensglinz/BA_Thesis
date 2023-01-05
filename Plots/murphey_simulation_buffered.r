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

# define function paremeters
start_date <- as.Date("2006-01-01")
end_date <- as.Date("2021-01-01")

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
lambda_loop <- seq(0.9, 0.995, by = 0.01)
measures <- tibble(NULL)
count <- 1

for (i in lambda_loop) {
    # assign lambda to the args list
    args_long_FESX$lambda <- i

    # calculate margins

    # release buffer when volatiltiy spikes --> Protects better from procyclicality than absolute margin levels !!! (this will only protect when they are already extremely high, other will protext from jump! ? --> Is this correct?)
    FESX_Margin <-
        calculate_fhs_margin(
            product = "FESX", start = start_date, end = end_date,
            args = args_long_FESX, steps = TRUE
        )
    release <- quantile(FESX_Margin$MARGIN, .9, na.rm = TRUE)
    FESX_Margin <- buffer_margin(FESX_Margin, buffer = .25, release = release)

    test <- test |>
        mutate(
            delta_5d = MARGIN / lead(MARGIN, 5),
        )

    # run Kupiec Test and discard those that do not meet test
    KPF_result <- kupiec_test(FESX_Margin,
        window = 750, model_conf_level = model_conf_level,
        test_conf_level = test_conf_level
    )

    temp <- summary_stats(FESX_Margin, start = start_date, end = end_date)
    n_breaches_2020 <- summary_stats(FESX_Margin, start = as.Date("2020-01-01"), end = as.Date("2020-12-31"))
    n_breaches_2020 <- temp_2020 |>
        filter(type == "n_breaches") |>
        mutate(type = "n_breaches_2020")
    temp <- bind_rows(temp, n_breaches_2020)

    temp <- temp |>
        mutate(
            lambda = i,
            KPF_result = KPF_result
        )

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

limit <- measures |>
    pivot_wider(names_from = type, values_from = values) |>
    filter(KPF_result) |>
    ggplot(aes(y = max_30d)) +
    geom_point(aes(x = round(costs * 100, 1), color = lambda)) +
    scale_color_gradientn(
        colors = c("white", "grey", "darkgrey", "black"),
        breaks = c(.9, .95, .99)
    ) +
    scale_x_continuous(
        breaks = seq(8.1, 9.6, by = .2)
    ) +
    labs(
        title = "30-day Procyclicality - Floored",
        x = "Avg. Costs (%)",
        y = NULL,
        color = expression(lambda)
    ) +
    guides(
        color = guide_colorbar(
            barheight = .5,
            title.position = "left",
            title.vjust = 1
        )
    ) +
    theme(
        text = element_text(family = "lmroman"),
        panel.grid.major = element_blank(),
        panel.grid.major.y = element_line(
            color = "grey",
            linetype = 2,
            linewidth = 0.5
        ),
        legend.text = element_text(size = 7, hjust = 1),
        legend.title = element_text(family = "sans", size = 7),
        panel.background = element_rect(color = "black", fill = "white"),
        legend.position = "bottom",
        legend.box.spacing = unit(0, "cm"),
        plot.title = element_text(size = 10, face = "bold"),
        legend.key = element_rect(fill = "white"),
        axis.title = element_text(size = 8),
        axis.text = element_text(size = 7),
        plot.margin = margin(0, 0, 0, 0)
    )

ggsave(
    "Plots/Output/floored.png", floored,
    width = 6.48, height = 5.45, unit = "cm"
)



out <- speed_limit(test, n_day = 1, limit = 1.025) |>
    select(DATE, MARGIN) |>
    rename(limit = MARGIN)

out |>
    left_join(test, by = "DATE") |>
    filter(between(DATE, start_procyclicality, end_procyclicality)) |>
    ggplot(aes(x = DATE)) +
    geom_line(aes(y = limit), linetype = 2) +
    geom_line(aes(y = MARGIN))

changes <- out |>
    filter(between(DATE, start, end)) |>
    select(DATE, limit) |>
    mutate(delta = limit / lag(limit, 20))

changes |>
    ggplot(aes(x = DATE, y = delta)) +
    geom_point()

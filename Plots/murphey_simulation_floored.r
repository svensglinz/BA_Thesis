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
start_procyclicality <- as.Date("2020-01-01")
end_procyclicality <- as.Date("2020-12-31")

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

    # calculate margins
    FESX_Margin <-
        calculate_margin(
            product = "FESX", start = start_date, end = end_date,
            args = args_long_FESX, steps = TRUE
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


floored <- measures |>
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

alt_risk_floored <- measures |>
    pivot_wider(names_from = type, values_from = values) |>
    mutate(
        max_shortfall = max_shortfall * -100,
        avg_shortfall = avg_shortfall * -100,
        n_breaches = n_breaches / 3,
        n_breaches_2020 = n_breaches / 3
    ) |>
    pivot_longer(!c(lambda, KPF_result), names_to = "type", values_to = "values") |>
    filter(KPF_result) |>
    filter(type %in% c("max_shortfall", "avg_shortfall", "n_breaches", "n_breaches_2020")) |>
    ggplot(aes(x = lambda, y = values, linetype = type)) +
    # geom_point(aes(shape = type)) +
    geom_line() +
    scale_y_continuous(
        breaks = seq(1, 20, 1),
        sec.axis = sec_axis(trans = \(x) x * 3, name = "N_Breaches")
    ) +
    labs(
        y = "Max Shortfall (%)",
        title = "Shortfall & Coverage",
        linetype = NULL,
        caption = "floored"
    ) +
    theme(
        text = element_text(family = "lmroman"),
        panel.grid.major = element_blank(),
        legend.text = element_text(size = 7, hjust = 1),
        plot.caption = element_text(size = 7),
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
    "Plots/Output/risks_floored.png", alt_risk_floored,
    width = 8, height = 8, unit = "cm"
)

# for the paper, before doing this, show baseline models wihtout lambda calibration!!!
# also in the lambda charts, highlight in color (or size or point type)
# which is the calibration used by eurex!


# explain the Kupiec Proportion of Failure test

# same with 25% floor!


# same with capped margin!

# same with speed limits!


# any other stuff???

# report short portfolio and portfolio for
# fixed income instruments in the appendix and just
# refer to it during the paper!!!

# also justify why we do average costs and not
# excess costs to baseline model as done in other papers
# --> Client centric view!

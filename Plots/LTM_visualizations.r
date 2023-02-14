# load libraries
library(tidyverse)
library(ggrepel)
library(ggh4x)
library(glue)
library(scales)
library(ggsci)
library(showtext)
library(latex2exp)
source("functions.r")

# add fonts for plotting & plot_theme
font_add(
    family = "lmroman",
    regular = "Fonts/lmroman10_regular.ttf",
    bold = "Fonts/lmroman10_bold.ttf",
    italic = "Fonts/lmroman10_italic.ttf",
    bolditalic = "Fonts/lmroman10_bolditalic.ttf",
    symbol = "Fonts/lmroman_math.ttf"
)

showtext_auto(enable = TRUE)
showtext_opts(dpi = 600)
set_plot_theme()

# load data frames
measures_long <- read_csv("Data/calculations_fesx_long.csv")
measures_short <- read_csv("Data/calculations_fesx_short.csv")

data <- list(long = measures_long, short = measures_short)

for (i in c("long", "short")) {
    ###########################
    # plot for different stress periods
    ###########################

    # mutate input df
    plot_df <- data[[i]] |>
        filter(type %in% c("avg_ltm", "max_ltm", "n_breaches"), period != "all")

    # generate plot
    plot_df |>
        ggplot(aes(x = lambda, y = values, color = model, group = model)) +
        geom_line(
            data = plot_df |> filter(type == "n_breaches"),
            position = position_jitter(width = .0022, height = 0), linewidth = .4
        ) +
        geom_line(
            data = plot_df |> filter(type == "avg_ltm"),
            position = position_jitter(width = .002, height = .01), linewidth = .4
        ) +
        geom_line(
            data = plot_df |> filter(type == "max_ltm"),
            position = position_jitter(width = .001, height = .01), linewidth = .4
        ) +
        scale_x_continuous(
            expand = expansion(add = c(.01, .01))
        ) +
        labs(
            title = glue("Loss to Margin and Breaches - Stress Periods (FESX {i})"),
            subtitle = TeX("Minimal random noise added to data to avoid overlapping lines | Grey Line = Baseline Calibration ($\\lambda$ = 0.96)"),
            caption = "Covid: 01.01.2020 - 31.12.2020, Financial Crisis: 01.06.2007 - 31.03.2009, Dotcom: 20.03.2001 - 01.04.2003",
            x = expression(lambda),
            y = NULL
        ) +
        geom_vline(xintercept = .96, linetype = "dashed", color = "darkgrey") +
        facet_grid2(period ~ type, scales = "free", independent = "y") +
        guides(color = guide_legend(label.position = "top", title = NULL, nrow = 1)) +
        scale_color_jama() +
        theme(
            strip.text = element_text(margin = margin(2, 2, 2, 2)),
            legend.key.width = unit(1.4, "cm")
        )

    # save plot
    ggsave(glue("Plots/Output/LTM_stress_periods_{i}.svg"),
        last_plot(),
        device = "svg",
        width = 16, height = 10, units = "cm", dpi = 600
    )

    ###########################
    # plot for entire time
    ###########################

    # mutate input df
    plot_df <- data[[i]] |>
        filter(type %in% c("avg_ltm", "max_ltm", "n_breaches"), period == "all")

    # generate plot
    plot_df |>
        ggplot(aes(x = lambda, y = values, color = model, group = model)) +
        geom_line(position = position_jitter(width = .0022, height = 0), linewidth = .4) +
        scale_x_continuous(
            expand = expansion(add = c(.01, .01))
        ) +
        labs(
            title = glue("Loss to Margin and Number of Breaches (FESX {i})"),
            subtitle = TeX("Minimal random noise added to data to avoid overlapping lines | Grey Line = Baseline Calibration ($\\lambda$ = 0.96)"),
            x = expression(lambda),
            y = NULL
        ) +
        geom_vline(xintercept = .96, linetype = "dashed", color = "darkgrey") +
        facet_wrap(~type, scales = "free") +
        guides(color = guide_legend(label.position = "top", title = NULL, nrow = 1)) +
        scale_color_jama() +
        theme(
            strip.text = element_text(margin = margin(2, 2, 2, 2)),
            legend.key.width = unit(1.4, "cm")
        )


    # save plot
    ggsave(glue("Plots/Output/LTM_total_{i}.svg"),
        last_plot(),
        device = "svg",
        width = 16, height = 7, units = "cm", dpi = 600
    )
}

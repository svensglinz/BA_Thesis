# load libraries
library(tidyverse)
library(ggrepel)
library(glue)
library(scales)
library(showtext)
library(latex2exp)
library(ggsci)
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
    # plots for APC tool comparisons (vs. baseline)
    ###########################

    plot_df <- data[[i]] |>
        filter(period == "all", type %in% c("costs", "max_30d", "peak_to_through", "kpf")) |>
        pivot_wider(names_from = type, values_from = values) |>
        pivot_longer(c(peak_to_through, max_30d), names_to = "measures", values_to = "values") |>
        mutate(label = ifelse(lambda == .91, model, NA_character_))

    for (j in list(c("speed", "speed_floor"), "baseline", "floor", "buffer", c("cap", "cap_floor"))) {
        # extract lambdas where kpf = FALSE / 0
        lambda_breach <- plot_df |>
            filter(kpf == 0, model == j) |>
            select(model, lambda) |>
            unique() |>
            group_by(model) |>
            summarize(lambda = paste(lambda, collapse = ", ")) |>
            arrange(model)

        # ensure that all models are inside the data frame (even if no backtesting breaches)
        lambda_breach <- tibble(model = j[order(j)]) |>
            left_join(lambda_breach, by = c("model")) |>
            replace_na(list(lambda = "None"))

        # control for multiple elements in i (currently cap and speed)
        if (length(j) > 1) {
            lambda_breach <- glue("{lambda_breach$lambda} ({lambda_breach$model})")
            lambda_breach <- paste(lambda_breach, collapse = ", ")
        } else {
            lambda_breach <- lambda_breach$lambda
        }

        # generate plot subtitle
        subtitle <- ifelse(nchar(lambda_breach) == 0,
            TeX("Backtesting not passed: None    |     $\\Delta$ = Baseline Model Specification ($\\lambda$ = 0.96)"),
            TeX(
                paste(
                    "Backtesting not passed: $\\lambda$ =",
                    lambda_breach,
                    "    |     $\\Delta$ = Baseline Model Specification ($\\lambda$ = 0.96)"
                )
            )
        )

        # generate plot
        plot_df |>
            filter(model %in% c(j, "baseline")) |>
            ggplot(aes(x = round(costs * 100, 4), y = values, color = model, alpha = lambda)) +
            geom_point() +
            # add empty observation for continuous alpha scale (via fill scale)
            geom_point(data = tibble(costs = NA_integer_, values = NA_integer_, lambda = .9, model = NA_character_), aes(fill = lambda)) +
            geom_point(
                data = plot_df |> filter(model %in% c(i, "baseline"), lambda == .96),
                shape = 24, color = "red", show.legend = FALSE
            ) +
            geom_text_repel(
                aes(label = label),
                alpha = 1, min.segment.length = unit(2, "cm"),
                show.legend = FALSE, size = 2.5
            ) +
            scale_x_continuous(breaks = scales::extended_breaks(n = 6)) +
            labs(
                title = glue("Procyclicality Evaluation: {j} (FESX {i})"),
                x = "Avg. Costs (% of Notional)",
                y = "Procyclicality",
                subtitle = subtitle
            ) +
            scale_fill_gradient(
                low = alpha("#374E55FF", .1), high = "#374E55FF",
                breaks = c(seq(.91, .99, .02)), limits = c(.9, 1),
            ) +
            guides(
                color = "none",
                alpha = "none",
                fill = guide_colorbar(
                    title = expression(lambda),
                    title.hjust = .5
                )
            ) +
            facet_wrap(~measures, scales = "free_y") +
            scale_color_jama() +
            theme(
                legend.position = "right",
                legend.direction = "vertical",
                legend.key.height = unit(.7, "cm")
            )

        # save plot
        ggsave(
            glue("Plots/Output/{j[1]}_{i}.svg"), last_plot(),
            width = 16, height = 7, unit = "cm", dpi = 600, device = "svg"
        )
    }

    ###########################
    # plot for comparison of APC tools
    ###########################

    # mutate df
    baseline_values <- data[[i]] |>
        filter(period == "all", model == "baseline" & lambda == .96, type %in% c("max_30d", "peak_to_through", "costs")) |>
        pivot_wider(values_from = values, names_from = type)

    plot_df <- data[[i]] |>
        filter(period == "all", type %in% c("costs", "max_30d", "peak_to_through", "kpf")) |>
        pivot_wider(names_from = type, values_from = values) |>
        pivot_longer(c(peak_to_through, max_30d), names_to = "measures", values_to = "values") |>
        filter(lambda == .96 | (model == "baseline" & lambda == .995)) |>
        mutate(
            label = case_when(
                lambda == .995 ~ "~lambda == .995",
                lambda == .96 & model == "baseline" ~ "~lambda == .96",
                TRUE ~ model
            )
        ) |>
        mutate(
            x_segment = baseline_values$costs,
            y_segment = case_when(
                measures == "peak_to_through" ~ baseline_values$peak_to_through,
                measures == "max_30d" ~ baseline_values$max_30d
            )
        )

    # generate plot
    plot_df |>
        ggplot(aes(x = round(costs * 100, 4), y = values, color = model)) +
        geom_point(show.legend = FALSE) +
        geom_text_repel(
            force = 10, force_pull = 10, nudge_y = -.1, size = 2.5,
            aes(label = label), parse = TRUE, show.legend = FALSE,
            min.segment.length = unit(2, "cm")
        ) +
        geom_segment(
            data = plot_df |> filter(!(lambda == .96 & model == "baseline")),
            aes(
                x = x_segment * 100, y = y_segment,
                xend = costs * 100 - 6 * (costs - baseline_values[["costs"]]),
                yend = ifelse(
                    (model == "speed" & measures == "peak_to_through") |
                        (i == "short" & model == "cap" & measures == "max_30d"),
                    values, values + 0.1
                )
            ), show.legend = FALSE, arrow = arrow(length = unit(.13, "cm")),
            alpha = .5, linewidth = .3
        ) +
        scale_x_continuous(
            breaks = scales::extended_breaks(n = 6)
        ) +
        labs(
            title = glue("Comparison of APC Tools (FESX {i})"),
            subtitle = TeX("Baseline Specification ($\\lambda$ = 0.96) unless otherwise indicated"),
            x = "Avg. Costs (% of Notional)",
            y = "Procyclicality",
            color = NULL
        ) +
        scale_alpha_continuous(
            breaks = c(seq(.9, .99, .02))
        ) +
        facet_wrap(~measures, scales = "free_y") +
        scale_color_jama()

    ggsave(
        glue("Plots/Output/APC_comparison_{i}.svg"), last_plot(),
        width = 16, height = 7, unit = "cm", dpi = 600, device = "svg"
    )
}

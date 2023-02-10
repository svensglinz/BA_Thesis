# load libraries
library(tidyverse)
library(ggrepel)
library(ggh4x)
library(ggsci)
library(showtext)
library(latex2exp)

# add fonts for plotting
font_add(
    family = "lmroman",
    regular = "Fonts/lmroman10_regular.ttf",
    bold = "Fonts/lmroman10_bold.ttf",
    italic = "Fonts/lmroman10_italic.ttf",
    bolditalic = "Fonts/lmroman10_bolditalic.ttf"
)

showtext_auto(enable = TRUE)
showtext_opts(dpi = 600)

###################################################
# Evaluation of Procyclicality Tools
###################################################

##############
# Long FESX
##############

# load data frame
measures <- read_csv("Data/procyclicality_calculations_fesx_long.csv")

# generate plots for baseline / APC tool combination for 30d & Peak-to-through Procylcicality
plot_df <- measures |>
    filter(period == "all", type %in% c("costs", "max_30d", "peak_to_through", "kpf")) |>
    pivot_wider(names_from = type, values_from = values) |>
    pivot_longer(c(peak_to_through, max_30d), names_to = "measures", values_to = "values") |>
    mutate(label = ifelse(lambda == .91, model, NA_character_))

for (i in list(c("speed", "speed_floor"), "baseline", "floor", "buffer", c("cap", "cap_floor"))) {
    lambda_breach <- plot_df |>
        filter(kpf == 0, model == i) |>
        select(model, lambda) |>
        unique() |>
        group_by(model) |>
        summarize(lambda = paste(lambda, collapse = ", ")) |>
        arrange(model)

    # ensure that all models are inside the data frame (even if no backtesting breaches)
    lambda_breach <- tibble(model = i[order(i)]) |>
        left_join(lambda_breach, by = c("model")) |>
        replace_na(list(lambda = "None"))

    # control for multiple elements in i (currently cap and speed)
    if (length(i) > 1) {
        lambda_breach <- glue::glue("{lambda_breach$lambda} ({lambda_breach$model})")
        lambda_breach <- paste(lambda_breach, collapse = ", ")
    } else {
        lambda_breach <- lambda_breach$lambda
    }


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

    plot_df |>
        filter(model %in% c(i, "baseline")) |>
        ggplot(aes(x = round(costs * 100, 4), y = values, color = model, alpha = lambda)) +
        geom_point() +
        # add empty observation for continuous alpha scale (via fill scale)
        geom_point(data = tibble(costs = NA_integer_, values = NA_integer_, lambda = .9, model = NA_character_), aes(fill = lambda)) +
        geom_point(
            # aes(fill = model),
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
            title = paste("Procyclicality Evaluation:", i, "(FESX Long)", sep = " "),
            x = "Avg. Costs (% of Notional)",
            y = "Procyclicality",
            subtitle = subtitle
        ) +
        scale_fill_gradient(
            low = alpha("#374E55FF", .1), high = "#374E55FF",
            breaks = c(seq(.91, .99, .02)), limits = c(.9, 1),
        ) +
        # scale_alpha_continuous(breaks = c(seq(.9, .99, .02))) +
        theme(
            text = element_text(family = "lmroman", colour = "#555555"),
            legend.position = "right",
            legend.key.width = unit(.3, "cm"),
            plot.subtitle = element_text(family = "sans", face = "italic", size = 7),
            plot.caption = element_text(size = 8),
            legend.background = element_rect(fill = "transparent", colour = "#cccccc", linewidth = 0),
            legend.justification = .5,
            panel.border = element_rect(colour = "#999999", fill = "transparent"),
            panel.background = element_rect(fill = "#FFFFFF", colour = "#999999", linewidth = 0),
            panel.grid.minor.y = element_line(colour = "#eeeeee", linewidth = 0.5),
            panel.grid.major = element_line(colour = "#eeeeee", linewidth = 0.5),
            panel.grid.minor.x = element_blank(),
            plot.background = element_rect(fill = "#F9F9F9", colour = "#CCCCCC", linewidth = 0),
            legend.box.spacing = unit(-.2, "cm"),
            legend.box.margin = margin(l = 6, 0, 0, 0),
            axis.ticks = element_blank(),
            axis.text = element_text(size = 6),
            axis.text.y = element_text(margin = margin(0, 0, 0, 0)),
            axis.text.x = element_text(margin = margin(0, 0, 0, 0)),
            axis.title = element_text(size = 8),
            plot.title = element_text(size = 10, face = "bold"),
            legend.title = element_text(size = 8, family = "sans", margin = margin(b = 0, 0, 0, 0)),
            legend.direction = "vertical",
            legend.text = element_text(size = 7, margin = margin(l = 0, 0, 0, 0)),
            plot.margin = margin(5, 5, 5, 5),
            legend.key = element_rect(fill = "transparent"),
            strip.background = element_rect(fill = "#FFFFFF", color = "#808080", linewidth = 0.5),
            strip.text = element_text(size = 8, margin = margin(t = 2, b = 2, 0, 0))
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
        scale_color_jama()

    ggsave(
        paste0("Plots/Output/", i[1], ".png"), last_plot(),
        width = 16, height = 7, unit = "cm", dpi = 600
    )
}

# Comparison of APC tools
baseline_values <- measures |>
    filter(period == "all", model == "baseline" & lambda == .96, type %in% c("max_30d", "peak_to_through", "costs")) |>
    pivot_wider(values_from = values, names_from = type)

plot_df <- measures |>
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

plot_df |>
    ggplot(aes(x = round(costs * 100, 4), y = values, color = model)) +
    geom_point(show.legend = FALSE) +
    geom_text_repel(force = 10, force_pull = 10, nudge_y = -.1, size = 2.5, aes(label = label), parse = TRUE, show.legend = FALSE) +
    geom_segment(
        data = plot_df |> filter(!(lambda == .96 & model == "baseline")),
        aes(
            x = x_segment * 100, y = y_segment,
            xend = costs * 100 - 6 * (costs - 0.0812), yend = ifelse(model == "speed" & measures == "peak_to_through", values, values + 0.1)
        ), show.legend = FALSE, arrow = arrow(length = unit(.13, "cm")),
        alpha = .5, linewidth = .3
    ) +
    scale_x_continuous(
        breaks = scales::extended_breaks(n = 6)
    ) +
    labs(
        title = "Comparison of APC Tools (FESX Long)",
        subtitle = TeX("Baseline Specification ($\\lambda$ = 0.96) unless otherwise indicated"),
        x = "Avg. Costs (% of Notional)",
        y = "Procyclicality",
        color = NULL
    ) +
    scale_alpha_continuous(
        breaks = c(seq(.9, .99, .02))
    ) +
    theme(
        text = element_text(family = "lmroman", colour = "#555555"),
        legend.position = "right",
        legend.background = element_rect(fill = "transparent", colour = "#cccccc", linewidth = 0),
        legend.justification = .5,
        panel.border = element_rect(colour = "#999999", fill = "transparent"),
        panel.background = element_rect(fill = "#FFFFFF", colour = "#999999", linewidth = 0),
        panel.grid.minor.y = element_line(colour = "#eeeeee", linewidth = 0.5),
        panel.grid.major = element_line(colour = "#eeeeee", linewidth = 0.5),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#F9F9F9", colour = "#CCCCCC", linewidth = 0, linetype = 1),
        legend.box.spacing = unit(-.2, "cm"),
        legend.box.margin = margin(0, 0, 0, 0),
        axis.ticks = element_blank(),
        axis.text = element_text(size = 6),
        axis.text.y = element_text(margin = margin(0, 0, 0, 0)),
        axis.text.x = element_text(margin = margin(0, 0, 0, 0)),
        axis.title = element_text(size = 8),
        plot.title = element_text(size = 10, face = "bold"),
        plot.subtitle = element_text(size = 8, family = "times"),
        plot.margin = margin(5, 5, 5, 5),
    ) +
    facet_wrap(~measures, scales = "free_y") +
    scale_color_jama()

ggsave(
    paste0("Plots/Output/procyclicality_comparison_long.png"), last_plot(),
    width = 16, height = 7, unit = "cm", dpi = 600
)

# plot with tail-behavior analysis (stress periods)
plot_df <- measures |>
    filter(type %in% c("avg_ltm", "max_ltm", "n_breaches"), period != "all")

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
        title = "Loss to Margin and Breaches - Stress Periods (FESX Long)",
        subtitle = TeX("Minimal random noise added to data to avoid overlapping lines | Grey Line = Baseline Calibration ($\\lambda$ = 0.96)"),
        caption = "Covid: 01.01.2020 - 31.12.2020, Financial Crisis: 01.06.2007 - 31.03.2009, Dotcom: 20.03.2001 - 01.04.2003",
        x = expression(lambda),
        y = NULL
    ) +
    geom_vline(xintercept = .96, linetype = "dashed", color = "darkgrey") +
    facet_grid2(period ~ type, scales = "free", independent = "y") +
    theme(
        text = element_text(family = "lmroman", colour = "#555555"),
        legend.position = "bottom",
        legend.key.width = unit(1.4, "cm"),
        legend.background = element_rect(fill = "transparent", colour = "#cccccc", linewidth = 0),
        legend.justification = .5,
        plot.caption = element_text(size = 8, margin = margin(0, 0, 0, 0)),
        panel.border = element_rect(colour = "#999999", fill = "transparent"),
        panel.background = element_rect(fill = "#FFFFFF", colour = "#999999", linewidth = 0),
        panel.grid.minor.y = element_line(colour = "#eeeeee", linewidth = 0.5),
        panel.grid.major = element_line(colour = "#eeeeee", linewidth = 0.5),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#F9F9F9", colour = "#CCCCCC", linewidth = 0, linetype = 1),
        legend.box.spacing = unit(-.2, "cm"),
        axis.ticks = element_blank(),
        axis.text = element_text(size = 6),
        axis.text.y = element_text(margin = margin(0, 0, 0, 0)),
        axis.text.x = element_text(margin = margin(0, 0, 0, 0)),
        axis.title.x = element_text(family = "times"),
        axis.title = element_text(size = 8),
        plot.title = element_text(size = 10, face = "bold"),
        plot.subtitle = element_text(size = 8, family = "times"),
        legend.direction = "vertical",
        legend.text = element_text(size = 8, margin = margin(b = -6, 0, 0, 0)),
        plot.margin = margin(5, 5, 5, 5),
        legend.key = element_rect(fill = "transparent"),
        strip.background = element_rect(fill = "#FFFFFF", color = "#808080", linewidth = 0.5),
        strip.text = element_text(size = 8, margin = margin(2, 2, 2, 2))
    ) +
    guides(color = guide_legend(label.position = "top", title = NULL, nrow = 1)) +
    scale_color_jama()

ggsave("Plots/Output/tail_risk_stress_periods_long.png", last_plot(), width = 16, height = 10, units = "cm", dpi = 600)

# chart only for entire time
plot_df <- measures |>
    filter(type %in% c("avg_ltm", "max_ltm", "n_breaches"), period == "all")

plot_df |>
    ggplot(aes(x = lambda, y = values, color = model, group = model)) +
    geom_line(position = position_jitter(width = .0022, height = 0), linewidth = .4) +
    scale_x_continuous(
        expand = expansion(add = c(.01, .01))
    ) +
    labs(
        title = "Loss to Margin and Number of Breaches (FESX Long)",
        subtitle = TeX("Minimal random noise added to data to avoid overlapping lines | Grey Line = Baseline Calibration ($\\lambda$ = 0.96)"),
        x = expression(lambda),
        y = NULL
    ) +
    geom_vline(xintercept = .96, linetype = "dashed", color = "darkgrey") +
    facet_wrap(~type, scales = "free") +
    theme(
        text = element_text(family = "lmroman", colour = "#555555"),
        legend.position = "bottom",
        legend.key.width = unit(1.4, "cm"),
        legend.background = element_rect(fill = "transparent", colour = "#cccccc", linewidth = 0),
        legend.justification = .5,
        plot.subtitle = element_text(size = 8, family = "times"),
        axis.title.x = element_text(family = "times"),
        plot.caption = element_text(size = 8, margin = margin(0, 0, 0, 0)),
        panel.border = element_rect(colour = "#999999", fill = "transparent"),
        panel.background = element_rect(fill = "#FFFFFF", colour = "#999999", linewidth = 0),
        panel.grid.minor.y = element_line(colour = "#eeeeee", linewidth = 0.5),
        panel.grid.major = element_line(colour = "#eeeeee", linewidth = 0.5),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#F9F9F9", colour = "#CCCCCC", linewidth = 0, linetype = 1),
        legend.box.spacing = unit(-.2, "cm"),
        axis.ticks = element_blank(),
        axis.text = element_text(size = 6),
        axis.text.y = element_text(margin = margin(0, 0, 0, 0)),
        axis.text.x = element_text(margin = margin(0, 0, 0, 0), family = ""),
        axis.title = element_text(size = 8),
        plot.title = element_text(size = 10, face = "bold"),
        legend.direction = "vertical",
        legend.text = element_text(size = 8, margin = margin(b = -6, 0, 0, 0)),
        plot.margin = margin(5, 5, 5, 5),
        legend.key = element_rect(fill = "transparent"),
        strip.background = element_rect(fill = "#FFFFFF", color = "#808080", linewidth = 0.5),
        strip.text = element_text(size = 8, margin = margin(2, 2, 2, 2))
    ) +
    guides(color = guide_legend(label.position = "top", title = NULL, nrow = 1)) +
    scale_color_jama()

ggsave("Plots/Output/tail_risk_total_long.png", last_plot(), width = 16, height = 7, units = "cm", dpi = 600)

# plot with tail behavior analysis (entire period)
##############
# Short FESX
##############

# generate plots for baseline / APC tool combination for 30d & Peak-to-through Procylcicality
measures <- read_csv("Data/procyclicality_calculations_fesx_short.csv")

# generate plots for baseline / APC tool combination for 30d & Peak-to-through Procylcicality
plot_df <- measures |>
    filter(period == "all", type %in% c("costs", "max_30d", "peak_to_through", "kpf")) |>
    pivot_wider(names_from = type, values_from = values) |>
    pivot_longer(c(peak_to_through, max_30d), names_to = "measures", values_to = "values") |>
    mutate(label = ifelse(lambda == .91, model, NA_character_))

for (i in list(c("speed", "speed_floor"), "baseline", "floor", "buffer", c("cap", "cap_floor"))) {
    lambda_breach <- plot_df |>
        filter(kpf == 0, model == i) |>
        select(model, lambda) |>
        unique() |>
        group_by(model) |>
        summarize(lambda = paste(lambda, collapse = ", ")) |>
        arrange(model)

    # ensure that all models are inside the data frame (even if no backtesting breaches)
    lambda_breach <- tibble(model = i[order(i)]) |>
        left_join(lambda_breach, by = c("model")) |>
        replace_na(list(lambda = "None"))

    # control for multiple elements in i (currently cap and speed)
    if (length(i) > 1) {
        lambda_breach <- glue::glue("{lambda_breach$lambda} ({lambda_breach$model})")
        lambda_breach <- paste(lambda_breach, collapse = ", ")
    } else {
        lambda_breach <- lambda_breach$lambda
    }


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

    plot_df |>
        filter(model %in% c(i, "baseline")) |>
        ggplot(aes(x = round(costs * 100, 4), y = values, color = model, alpha = lambda)) +
        geom_point() +
        # add empty observation for continuous alpha scale (via fill scale)
        geom_point(data = tibble(costs = NA_integer_, values = NA_integer_, lambda = .9, model = NA_character_), aes(fill = lambda)) +
        geom_point(
            # aes(fill = model),
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
            title = paste("Procyclicality Evaluation:", i, "(FESX Short)", sep = " "),
            x = "Avg. Costs (% of Notional)",
            y = "Procyclicality",
            subtitle = subtitle
        ) +
        scale_fill_gradient(
            low = alpha("#374E55FF", .1), high = "#374E55FF",
            breaks = c(seq(.91, .99, .02)), limits = c(.9, 1),
        ) +
        # scale_alpha_continuous(breaks = c(seq(.9, .99, .02))) +
        theme(
            text = element_text(family = "lmroman", colour = "#555555"),
            legend.position = "right",
            legend.key.width = unit(.3, "cm"),
            plot.subtitle = element_text(family = "sans", face = "italic", size = 7),
            plot.caption = element_text(size = 8),
            legend.background = element_rect(fill = "transparent", colour = "#cccccc", linewidth = 0),
            legend.justification = .5,
            panel.border = element_rect(colour = "#999999", fill = "transparent"),
            panel.background = element_rect(fill = "#FFFFFF", colour = "#999999", linewidth = 0),
            panel.grid.minor.y = element_line(colour = "#eeeeee", linewidth = 0.5),
            panel.grid.major = element_line(colour = "#eeeeee", linewidth = 0.5),
            panel.grid.minor.x = element_blank(),
            plot.background = element_rect(fill = "#F9F9F9", colour = "#CCCCCC", linewidth = 0),
            legend.box.spacing = unit(-.2, "cm"),
            legend.box.margin = margin(l = 6, 0, 0, 0),
            axis.ticks = element_blank(),
            axis.text = element_text(size = 6),
            axis.text.y = element_text(margin = margin(0, 0, 0, 0)),
            axis.text.x = element_text(margin = margin(0, 0, 0, 0)),
            axis.title = element_text(size = 8),
            plot.title = element_text(size = 10, face = "bold"),
            legend.title = element_text(size = 8, family = "sans", margin = margin(b = 0, 0, 0, 0)),
            legend.direction = "vertical",
            legend.text = element_text(size = 7, margin = margin(l = 0, 0, 0, 0)),
            plot.margin = margin(5, 5, 5, 5),
            legend.key = element_rect(fill = "transparent"),
            strip.background = element_rect(fill = "#FFFFFF", color = "#808080", linewidth = 0.5),
            strip.text = element_text(size = 8, margin = margin(t = 2, b = 2, 0, 0))
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
        scale_color_jama()

    ggsave(
        paste0("Plots/Output/", i[1], ".png"), last_plot(),
        width = 16, height = 7, unit = "cm", dpi = 600
    )
}

# Comparison of APC tools
baseline_values <- measures |>
    filter(period == "all", model == "baseline" & lambda == .96, type %in% c("max_30d", "peak_to_through", "costs")) |>
    pivot_wider(values_from = values, names_from = type)

plot_df <- measures |>
    filter(period == "all", type %in% c("costs", "max_30d", "peak_to_through", "kpf")) |>
    pivot_wider(names_from = type, values_from = values) |>
    pivot_longer(c(peak_to_through, max_30d), names_to = "measures", values_to = "values") |>
    filter(lambda == .96 | (model == "baseline" & lambda == .995)) |>
    mutate(
        label = case_when(
            lambda == .995 ~ "~lambda == .995",
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

plot_df |>
    filter(label != "baseline") |>
    ggplot(aes(x = round(costs * 100, 4), y = values, color = model)) +
    geom_point(show.legend = FALSE) +
    geom_text_repel(min.segment.length = unit(2, "cm"), force = 10, force_pull = 10, nudge_y = -.1, size = 2.5, aes(label = label), parse = TRUE, show.legend = FALSE) +
    geom_segment(
        aes(
            x = x_segment * 100, y = y_segment,
            xend = costs * 100 - 6 * (costs - 0.0749), yend = ifelse((model == "speed" & measures == "peak_to_through") | (model == "cap" & measures == "max_30d"), values, values + 0.05)
        ),
        show.legend = FALSE, arrow = arrow(length = unit(.13, "cm")),
        alpha = .5, linewidth = .3
    ) +
    scale_x_continuous(
        breaks = scales::extended_breaks(n = 6)
    ) +
    labs(
        title = "Comparison of APC Tools (FESX Short)",
        x = "Avg. Costs (% of Notional)",
        y = "Procyclicality",
        color = NULL
    ) +
    scale_alpha_continuous(
        breaks = c(seq(.9, .99, .02))
    ) +
    theme(
        text = element_text(family = "lmroman", colour = "#555555"),
        legend.position = "right",
        legend.background = element_rect(fill = "transparent", colour = "#cccccc", linewidth = 0),
        legend.justification = .5,
        panel.border = element_rect(colour = "#999999", fill = "transparent"),
        panel.background = element_rect(fill = "#FFFFFF", colour = "#999999", linewidth = 0),
        panel.grid.minor.y = element_line(colour = "#eeeeee", linewidth = 0.5),
        panel.grid.major = element_line(colour = "#eeeeee", linewidth = 0.5),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#F9F9F9", colour = "#CCCCCC", linewidth = 0, linetype = 1),
        legend.box.spacing = unit(-.2, "cm"),
        legend.box.margin = margin(0, 0, 0, 0),
        axis.ticks = element_blank(),
        axis.text = element_text(size = 6),
        axis.text.y = element_text(margin = margin(0, 0, 0, 0)),
        axis.text.x = element_text(margin = margin(0, 0, 0, 0)),
        axis.title = element_text(size = 8),
        plot.title = element_text(size = 10, face = "bold"),
        legend.title = element_text(size = 8, margin = margin(b = 0, 0, 0, 0), hjust = .5),
        legend.direction = "vertical",
        legend.text = element_text(size = 8, margin = margin(l = -6, 0, 0, 0)),
        plot.margin = margin(5, 5, 5, 5),
        legend.key = element_rect(fill = "transparent"),
        strip.background = element_rect(fill = "#FFFFFF", color = "#808080", linewidth = 0.5),
        strip.text = element_text(size = 8, margin = margin(t = 2, b = 2, 0, 0))
    ) +
    facet_wrap(~measures, scales = "free_y") +
    scale_color_jama()

ggsave(
    paste0("Plots/Output/procyclicality_comparison_short.png"), last_plot(),
    width = 16, height = 7, unit = "cm", dpi = 600
)

# tail behavior of stress periods
plot_df <- measures |>
    filter(type %in% c("avg_ltm", "max_ltm", "n_breaches"), period != "all")

plot_df |>
    ggplot(aes(x = lambda, y = values, color = model, group = model)) +
    geom_line(
        data = plot_df |> filter(type == "n_breaches"),
        position = position_jitter(width = .0022, height = 0), linewidth = .3
    ) +
    geom_line(
        data = plot_df |> filter(type == "max_ltm"),
        position = position_jitter(width = .002, height = .01), linewidth = .3
    ) +
    geom_line(
        data = plot_df |> filter(type == "avg_ltm"),
        position = position_jitter(width = .001, height = .01), linewidth = .3
    ) +
    scale_x_continuous(
        expand = expansion(add = c(.01, .01))
    ) +
    labs(
        title = "Loss to Margin and Breaches - Stress Periods (FESX Short)",
        subtitle = TeX("Minimal random noise added to data to avoid overlapping lines | Grey Line = Baseline Calibration ($\\lambda$ = 0.96)"),
        x = expression(lambda),
        y = NULL,
        caption = "Covid: 01.01.2020 - 31.12.2020, Financial Crisis: 01.06.2007 - 31.03.2009, Dotcom: 20.03.2001 - 01.04.2003"
    ) +
    geom_vline(xintercept = .96, linetype = "dashed", color = "darkgrey") +
    facet_grid2(period ~ type, scales = "free", independent = "y") +
    theme(
        text = element_text(family = "lmroman", colour = "#555555"),
        legend.position = "bottom",
        legend.key.width = unit(1.4, "cm"),
        legend.background = element_rect(fill = "transparent", colour = "#cccccc", linewidth = 0),
        legend.justification = .5,
        plot.caption = element_text(size = 8, margin = margin(0, 0, 0, 0)),
        panel.border = element_rect(colour = "#999999", fill = "transparent"),
        panel.background = element_rect(fill = "#FFFFFF", colour = "#999999", linewidth = 0),
        panel.grid.minor.y = element_line(colour = "#eeeeee", linewidth = 0.5),
        panel.grid.major = element_line(colour = "#eeeeee", linewidth = 0.5),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#F9F9F9", colour = "#CCCCCC", linewidth = 0, linetype = 1),
        legend.box.spacing = unit(-.2, "cm"),
        axis.ticks = element_blank(),
        axis.text = element_text(size = 6),
        axis.text.y = element_text(margin = margin(0, 0, 0, 0)),
        axis.text.x = element_text(margin = margin(0, 0, 0, 0)),
        axis.title.x = element_text(family = "times"),
        axis.title = element_text(size = 8),
        plot.title = element_text(size = 10, face = "bold"),
        plot.subtitle = element_text(size = 8, family = "times"),
        legend.direction = "vertical",
        legend.text = element_text(size = 8, margin = margin(b = -6, 0, 0, 0)),
        plot.margin = margin(5, 5, 5, 5),
        legend.key = element_rect(fill = "transparent"),
        strip.background = element_rect(fill = "#FFFFFF", color = "#808080", linewidth = 0.5),
        strip.text = element_text(size = 8, margin = margin(2, 2, 2, 2))
    ) +
    guides(color = guide_legend(label.position = "top", title = NULL, nrow = 1)) +
    scale_color_jama()

ggsave("Plots/Output/tail_risk_stress_periods_short.png", last_plot(), width = 16, height = 10, units = "cm", dpi = 600)

# tail measures for entire period
plot_df <- measures |>
    filter(type %in% c("avg_ltm", "max_ltm", "n_breaches"), period == "all")

plot_df |>
    ggplot(aes(x = lambda, y = values, color = model, group = model)) +
    geom_line(position = position_jitter(width = .0022, height = 0), linewidth = .3) +
    scale_x_continuous(
        expand = expansion(add = c(.01, .01))
    ) +
    labs(
        title = "Loss to Margin and Number of Breaches (FESX Short)",
        subtitle = TeX("Minimal random noise added to data to avoid overlapping lines | Grey Line = Baseline Calibration ($\\lambda$ = 0.96)"),
        x = expression(lambda),
        y = NULL
    ) +
    geom_vline(xintercept = .96, linetype = "dashed", color = "darkgrey") +
    facet_wrap(~type, scales = "free") +
    theme(
        text = element_text(family = "lmroman", colour = "#555555"),
        legend.position = "bottom",
        legend.key.width = unit(1.4, "cm"),
        legend.background = element_rect(fill = "transparent", colour = "#cccccc", linewidth = 0),
        legend.justification = .5,
        plot.subtitle = element_text(size = 8, family = "times"),
        axis.title.x = element_text(family = "times"),
        plot.caption = element_text(size = 8, margin = margin(0, 0, 0, 0)),
        panel.border = element_rect(colour = "#999999", fill = "transparent"),
        panel.background = element_rect(fill = "#FFFFFF", colour = "#999999", linewidth = 0),
        panel.grid.minor.y = element_line(colour = "#eeeeee", linewidth = 0.5),
        panel.grid.major = element_line(colour = "#eeeeee", linewidth = 0.5),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#F9F9F9", colour = "#CCCCCC", linewidth = 0, linetype = 1),
        legend.box.spacing = unit(-.2, "cm"),
        axis.ticks = element_blank(),
        axis.text = element_text(size = 6),
        axis.text.y = element_text(margin = margin(0, 0, 0, 0)),
        axis.text.x = element_text(margin = margin(0, 0, 0, 0), family = ""),
        axis.title = element_text(size = 8),
        plot.title = element_text(size = 10, face = "bold"),
        legend.direction = "vertical",
        legend.text = element_text(size = 8, margin = margin(b = -6, 0, 0, 0)),
        plot.margin = margin(5, 5, 5, 5),
        legend.key = element_rect(fill = "transparent"),
        strip.background = element_rect(fill = "#FFFFFF", color = "#808080", linewidth = 0.5),
        strip.text = element_text(size = 8, margin = margin(2, 2, 2, 2))
    ) +
    guides(color = guide_legend(label.position = "top", title = NULL, nrow = 1)) +
    scale_color_jama()

ggsave("Plots/Output/tail_risk_total_short.png", last_plot(), width = 16, height = 7, units = "cm", dpi = 600)

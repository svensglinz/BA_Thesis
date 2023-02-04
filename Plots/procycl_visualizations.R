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

# generate plots for baseline / APC tool combination for 30d & Peak-to-through Procylcicality
measures <- read_csv("Data/procyclicality_calculations_fesx_long.csv")

plot_df <- measures |>
    filter(period == "all", type %in% c("costs", "max_30d", "peak_to_through", "kpf")) |>
    pivot_wider(names_from = type, values_from = values) |>
    pivot_longer(c(peak_to_through, max_30d), names_to = "measures", values_to = "values") |> 
    mutate(label = ifelse(lambda == .91, model, NA_character_))

for (i in c("cap", "speed", "baseline", "floor", "buffer", "cap_floor", "speed_floor")) {
    
    lambda_breach <- plot_df |>
        filter(kpf == 0, model == i) |>
        select(model, lambda) |>
        unique() |>
        pull(lambda)

    lambda_breach  <- paste(lambda_breach, collapse = ", ")

    plot_df |>
        filter(model %in% c(i, "baseline")) |>
        ggplot(aes(x = round(costs * 100, 4), y = values, color = model, alpha = lambda)) +
        geom_point() +
        geom_point(
            aes(fill = model),
            data = plot_df |> filter(model %in% c(i, "baseline"), lambda == .96),
            shape = 25, color = "red", show.legend = FALSE
        ) +
        geom_text_repel(
            aes(label = label), alpha = 1, min.segment.length = unit(2, "cm"),
            show.legend = FALSE, size = 2.5
        ) +
        scale_x_continuous(breaks = scales::extended_breaks(n = 6)) +
        labs(
            title = paste("Procyclicality", i, sep = " "),
            x = "Avg. Costs (% of Notional)",
            y = "Procyclicality", 
            subtitle = latex2exp::TeX(paste("Backtesting not passed: $\\lambda$ =", lambda_breach))
        ) +
        scale_alpha_continuous(breaks = c(seq(.9, .99, .02))) +
        theme(
            text = element_text(family= "lmroman", colour = "#555555"),
            legend.position = "right",
            plot.subtitle = element_text(family = "sans", face = "italic", size = 7),
            plot.caption = element_text(size = 8),
            legend.background = element_rect(fill="transparent", colour = "#cccccc", linewidth = 0),
            legend.justification = .5,
            panel.border = element_rect(colour="#999999", fill = "transparent"),
            panel.background = element_rect(fill="#FFFFFF", colour="#999999", linewidth = 0),
            panel.grid.minor.y = element_line(colour = "#eeeeee", linewidth = 0.5),
            panel.grid.major = element_line(colour = "#eeeeee", linewidth = 0.5),
            panel.grid.minor.x = element_blank(),
            plot.background = element_rect(fill = "#F9F9F9", colour="#CCCCCC", linewidth = 0),
            legend.box.spacing = unit(-.2, "cm"), 
            legend.box.margin = margin(0, 0, 0, 0),
            axis.ticks = element_blank(),
            axis.text = element_text(size = 6),
            axis.text.y = element_text(margin = margin(0, 0, 0, 0)),
            axis.text.x = element_text(margin = margin(0, 0, 0, 0)),
            axis.title = element_text(size = 8),
            plot.title = element_text(size = 10, face = "bold"), 
            legend.title = element_text(size = 8, family = "sans", margin = margin(b = -5, 0, 0, 0)),
            legend.direction = "vertical",
            legend.text = element_text(size = 8, margin = margin(l = -6, 0, 0, 0)),
            plot.margin = margin(5, 5, 5, 5),
            legend.key = element_rect(fill = "transparent"),
            strip.background = element_rect(fill="#FFFFFF", color = "#808080", linewidth = 0.5),
            strip.text = element_text(size = 8, margin = margin(t = 2, b = 2, 0, 0))
        ) + 
        guides(
            color = "none",
            alpha = guide_legend(
                title = expression(lambda), 
                title.hjust = .6)) +
        facet_wrap(~measures, scales = "free_y") +
            scale_color_jama() +
            scale_fill_jama()
        
    ggsave(
        paste0("Plots/Output/", i, ".png"), last_plot(),
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
    )  |> 
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
    geom_text_repel(force = 10, force_pull = 10, nudge_y = -.1,size = 2.5, aes(label = label), parse = TRUE, show.legend = FALSE) +
    geom_segment(
        aes(
            x = x_segment * 100, y = y_segment,
            xend = costs * 100 - 6 * (costs -0.0812), yend = ifelse(model == "speed" & measures == "peak_to_through", values, values + 0.1)
        ), show.legend = FALSE, arrow = arrow(length = unit(.13, "cm")), 
        alpha = .5, linewidth = .3
    ) +
    scale_x_continuous(
        breaks = scales::extended_breaks(n = 6)
    ) +
    labs(
        title = "Comparison of APC Tools",
        x = "Avg. Costs (% of Notional)",
        y = "Procyclicality", 
        color = NULL
    ) +
    scale_alpha_continuous(
        breaks = c(seq(.9, .99, .02))
    ) +
    theme(
        text = element_text(family= "lmroman", colour = "#555555"),
        legend.position = "right",
        legend.background = element_rect(fill="transparent", colour = "#cccccc", linewidth = 0),
        legend.justification = .5,
        panel.border = element_rect(colour="#999999", fill = "transparent"),
        panel.background = element_rect(fill="#FFFFFF", colour="#999999", linewidth = 0),
        panel.grid.minor.y = element_line(colour = "#eeeeee", linewidth = 0.5),
        panel.grid.major = element_line(colour = "#eeeeee", linewidth = 0.5),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#F9F9F9", colour="#CCCCCC", linewidth = 0, linetype = 1),
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
        strip.background = element_rect(fill="#FFFFFF", color = "#808080", linewidth = 0.5),
        strip.text = element_text(size = 8, margin = margin(t = 2, b = 2, 0, 0))
    ) +
    facet_wrap(~measures, scales = "free_y") + 
    scale_color_jama()

ggsave(
    paste0("Plots/Output/procyclicality_comparison.png"), last_plot(),
    width = 16, height = 7, unit = "cm", dpi = 600
)

##############
# Short FESX
##############

# generate plots for baseline / APC tool combination for 30d & Peak-to-through Procylcicality
measures <- read_csv("Data/procyclicality_calculations_fesx_short.csv")

plot_df <- measures |>
    filter(period == "all", type %in% c("costs", "max_30d", "peak_to_through", "kpf")) |>
    pivot_wider(names_from = type, values_from = values) |>
    pivot_longer(c(peak_to_through, max_30d), names_to = "measures", values_to = "values") |> 
    mutate(label = ifelse(lambda == .91, model, NA_character_))

for (i in c("cap", "speed", "baseline", "floor", "buffer", "cap_floor", "speed_floor")) {
    
    lambda_breach <- plot_df |>
        filter(kpf == 0, model == i) |>
        select(model, lambda) |>
        unique() |>
        pull(lambda)

    lambda_breach  <- paste(lambda_breach, collapse = ", ")

    plot_df |>
        filter(model %in% c(i, "baseline")) |>
        ggplot(aes(x = round(costs * 100, 4), y = values, color = model, alpha = lambda)) +
        geom_point() +
        geom_point(
            aes(fill = model),
            data = plot_df |> filter(model %in% c(i, "baseline"), lambda == .96),
            shape = 25, color = "red", show.legend = FALSE
        ) +
        geom_text_repel(
            aes(label = label), alpha = 1, min.segment.length = unit(2, "cm"),
            show.legend = FALSE, size = 2.5
        ) +
        scale_x_continuous(breaks = scales::extended_breaks(n = 6)) +
        labs(
            title = paste("Procyclicality", i, sep = " "),
            x = "Avg. Costs (% of Notional)",
            y = "Procyclicality", 
            subtitle = latex2exp::TeX(paste("Backtesting not passed: $\\lambda$ =", lambda_breach)) 
        ) +
        scale_alpha_continuous(breaks = c(seq(.9, .99, .02))) +
        theme(
            text = element_text(family= "lmroman", colour = "#555555"),
            legend.position = "right",
            plot.subtitle = element_text(family = "sans", face = "italic", size = 7),
            plot.caption = element_text(size = 8),
            legend.background = element_rect(fill="transparent", colour = "#cccccc", linewidth = 0),
            legend.justification = .5,
            panel.border = element_rect(colour="#999999", fill = "transparent"),
            panel.background = element_rect(fill="#FFFFFF", colour="#999999", linewidth = 0),
            panel.grid.minor.y = element_line(colour = "#eeeeee", linewidth = 0.5),
            panel.grid.major = element_line(colour = "#eeeeee", linewidth = 0.5),
            panel.grid.minor.x = element_blank(),
            plot.background = element_rect(fill = "#F9F9F9", colour="#CCCCCC", linewidth = 0),
            legend.box.spacing = unit(-.2, "cm"), 
            legend.box.margin = margin(0, 0, 0, 0),
            axis.ticks = element_blank(),
            axis.text = element_text(size = 6),
            axis.text.y = element_text(margin = margin(0, 0, 0, 0)),
            axis.text.x = element_text(margin = margin(0, 0, 0, 0)),
            axis.title = element_text(size = 8),
            plot.title = element_text(size = 10, face = "bold"), 
            legend.title = element_text(size = 8, family = "sans", margin = margin(b = -5, 0, 0, 0)),
            legend.direction = "vertical",
            legend.text = element_text(size = 8, margin = margin(l = -6, 0, 0, 0)),
            plot.margin = margin(5, 5, 5, 5),
            legend.key = element_rect(fill = "transparent"),
            strip.background = element_rect(fill="#FFFFFF", color = "#808080", linewidth = 0.5),
            strip.text = element_text(size = 8, margin = margin(t = 2, b = 2, 0, 0))
        ) + 
        guides(
            color = "none",
            alpha = guide_legend(
                title = expression(lambda), 
                title.hjust = .6)) +
        facet_wrap(~measures, scales = "free_y") +
            scale_color_jama() +
            scale_fill_jama()
        
    ggsave(
        paste0("Plots/Output/", i, ".png"), last_plot(),
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
    )  |> 
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
    geom_text_repel(min.segment.length = unit(2, "cm"), force = 10, force_pull = 10, nudge_y = -.1,size = 2.5, aes(label = label), parse = TRUE, show.legend = FALSE) +
    geom_segment(
        aes(
            x = x_segment * 100, y = y_segment,
            xend = costs * 100 - 6 * (costs - 0.0749), yend = ifelse((model == "speed" & measures == "peak_to_through") | (model == "cap" & measures == "max_30d"), values, values + 0.05
        )
        ), show.legend = FALSE, arrow = arrow(length = unit(.13, "cm")), 
        alpha = .5, linewidth = .3
    ) +
    scale_x_continuous(
        breaks = scales::extended_breaks(n = 6)
    ) +
    labs(
        title = "Comparison of APC Tools",
        x = "Avg. Costs (% of Notional)",
        y = "Procyclicality", 
        color = NULL
    ) +
    scale_alpha_continuous(
        breaks = c(seq(.9, .99, .02))
    ) +
    theme(
        text = element_text(family= "lmroman", colour = "#555555"),
        legend.position = "right",
        legend.background = element_rect(fill="transparent", colour = "#cccccc", linewidth = 0),
        legend.justification = .5,
        panel.border = element_rect(colour="#999999", fill = "transparent"),
        panel.background = element_rect(fill="#FFFFFF", colour="#999999", linewidth = 0),
        panel.grid.minor.y = element_line(colour = "#eeeeee", linewidth = 0.5),
        panel.grid.major = element_line(colour = "#eeeeee", linewidth = 0.5),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#F9F9F9", colour="#CCCCCC", linewidth = 0, linetype = 1),
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
        strip.background = element_rect(fill="#FFFFFF", color = "#808080", linewidth = 0.5),
        strip.text = element_text(size = 8, margin = margin(t = 2, b = 2, 0, 0))
    ) +
    facet_wrap(~measures, scales = "free_y") + 
    scale_color_jama()

ggsave(
    paste0("Plots/Output/procyclicality_comparison_short.png"), last_plot(),
    width = 16, height = 7, unit = "cm", dpi = 600
)


# visualizations for short FESX
measures <- read_csv("Data/procyclicality_calculations_fesx_short.csv")

plot_df <- measures |>
    filter(period == "all", type %in% c("costs", "max_30d", "peak_to_through", "kpf")) |>
    pivot_wider(names_from = type, values_from = values) |>
    pivot_longer(c(peak_to_through, max_30d), names_to = "measures", values_to = "values")

for (i in c("cap", "speed", "baseline", "floor", "buffer", "cap_floor", "speed_floor")) {
    plot_df |>
        filter(model %in% c(i, "baseline"), kpf == 1) |>
        ggplot(aes(x = round(costs * 100, 4), y = values, color = model, alpha = lambda)) +
        geom_point() +
        geom_point(
            aes(fill = model),
            data = plot_df |> filter(model %in% c(i, "baseline"), lambda == .96),
            shape = 25, color = "red", show.legend = FALSE
        ) +
        scale_x_continuous(
            breaks = scales::extended_breaks(n = 6)
        ) +
        labs(
            title = paste("Procyclicality", i, sep = " "),
            x = "Avg. Costs (% of Notional)",
            y = "Procyclicality"
        ) +
        scale_alpha_continuous(
            breaks = c(seq(.9, .99, .02))
        ) +
        theme(
            text = element_text(family= "lmroman", colour = "#555555"),
            legend.position = "right",
            legend.background = element_rect(fill="transparent", colour = "#cccccc", linewidth = 0),
            legend.justification = .5,
            panel.border = element_rect(colour="#999999", fill = "transparent"),
            panel.background = element_rect(fill="#FFFFFF", colour="#999999", linewidth = 0),
            panel.grid.minor.y = element_line(colour = "#eeeeee", linewidth = 0.5),
            panel.grid.major = element_line(colour = "#eeeeee", linewidth = 0.5),
            panel.grid.minor = element_blank(),
            plot.background = element_rect(fill = "#F9F9F9", colour="#CCCCCC", linewidth = 0, linetype = 1),
            legend.box.spacing = unit(-.2, "cm"), 
            legend.box.margin = margin(0, 0, 0, 0),
            axis.ticks = element_blank(),
            axis.text = element_text(size = 6),
            axis.text.y = element_text(margin = margin(0, 0, 0, 0)),
            axis.text.x = element_text(margin = margin(0, 0, 0, 0)),
            axis.title = element_text(size = 8),
            plot.title = element_text(size = 10, face = "bold"), 
            legend.title = element_text(size = 8, family = "sans", margin = margin(b = -5, 0, 0, 0)),
            legend.direction = "vertical",
            legend.text = element_text(size = 8, margin = margin(l = -6, 0, 0, 0)),
            plot.margin = margin(5, 5, 5, 5),
            legend.key = element_rect(fill = "transparent"),
            strip.background = element_rect(fill="#FFFFFF", color = "#808080", linewidth = 0.5),
            strip.text = element_text(size = 8, margin = margin(t = 2, b = 2, 0, 0))
        ) + 
        guides(
            color = "none",
            alpha = guide_legend(
                title = expression(lambda), 
                title.hjust = .6)) +
        facet_wrap(~measures, scales = "free_y") +
            scale_color_jama() +
            scale_fill_jama()
        
    ggsave(
        paste0("Plots/Output/", i, "_short", ".png"), last_plot(),
        width = 16, height = 7, unit = "cm", dpi = 600
    )
}

plot_df <- measures |>
    filter(type %in% c("max_30d", "peak_to_through", "costs"), period == "all") |>
    pivot_wider(names_from = type, values_from = values) |>
    pivot_longer(c(peak_to_through, max_30d), names_to = "measures", values_to = "values") |>
    mutate(label = ifelse(lambda == .995, model, NA))

a <- plot_df
plot_df <- a |>
    filter(model %in% c("baseline", "cap", "cap_floor")) |>
    group_by

plot_df |>
    ggplot(aes(y = values, x = costs, color = model)) +
    geom_point(aes(alpha = lambda),
        data = plot_df |> filter(period == "all" & lambda != .96), size = 1
    ) +
    geom_point(
        data = plot_df |> filter(lambda == .96),
        color = "red", size = 2, show.legend = FALSE
    ) +
    # guides(color = "none") +
    labs(
        title = "Comparison of APC Tools"
    ) +
    facet_wrap(~measures, scales = "free_y")

ggsave(
    "Plots/Output/combined_murphey.png", last_plot(),
    width = 16, height = 12, unit = "cm", dpi = 600
)

# alternative shortfall measures Chart
#################
#################
#################
#################

measures |>
    filter(type == "kpf" & values == 0)
plot_df <- measures |>
    filter(type %in% c("avg_ltm", "max_ltm", "n_breaches"), period != "all") 

plot_df |>
    ggplot(aes(x = lambda, y = values, color = model, group = model)) +
    geom_line(
        data = plot_df |> filter(type == "n_breaches"),
        position = position_jitter(width = .0022, height = 0), size = .4
    ) +
    geom_line(
        data = plot_df |> filter(type == "max_ltm"),
        position = position_jitter(width = .002, height = .01), size = .4
    ) +
    geom_line(
        data = plot_df |> filter(type == "avg_ltm"),
        position = position_jitter(width = .001, height = .01), size = .4
    ) +
    scale_x_continuous(
        expand = expansion(add = c(.01, .01))
    ) +
    labs(
        title = "Loss to Margin and Breaches - Stress Periods",
        subtitle = "Minimal random noise added to data to avoid overlapping lines", 
        caption = "Own Depiction", 
        x = expression(lambda), 
        y = NULL
    ) +
    geom_vline(xintercept = .96, linetype = "dashed", color = "darkgrey") +
    facet_grid2(period ~ type, scales = "free", independent = "y") +
    theme(
            text = element_text(family= "lmroman", colour = "#555555"),
            legend.position = "bottom",
            legend.key.width = unit(1.4, "cm"),
            legend.background = element_rect(fill="transparent", colour = "#cccccc", linewidth = 0),
            legend.justification = .5,
            plot.subtitle = element_text(size = 8),
            plot.caption = element_text(size = 8, margin = margin(0, 0, 0, 0)),
            panel.border = element_rect(colour="#999999", fill = "transparent"),
            panel.background = element_rect(fill="#FFFFFF", colour="#999999", linewidth = 0),
            panel.grid.minor.y = element_line(colour = "#eeeeee", linewidth = 0.5),
            panel.grid.major = element_line(colour = "#eeeeee", linewidth = 0.5),
            panel.grid.minor = element_blank(),
            plot.background = element_rect(fill = "#F9F9F9", colour="#CCCCCC", linewidth = 0, linetype = 1),
            legend.box.spacing = unit(-.2, "cm"), 
            #legend.box.margin = margin(0, 0, 0, 0),
            axis.ticks = element_blank(),
            axis.text = element_text(size = 6),
            axis.text.y = element_text(margin = margin(0, 0, 0, 0)),
            axis.text.x = element_text(margin = margin(0, 0, 0, 0)),
            axis.title = element_text(size = 8),
            plot.title = element_text(size = 10, face = "bold"), 
            legend.direction = "vertical",
            legend.text = element_text(size = 8, margin = margin(b = -6, 0, 0, 0)),
            plot.margin = margin(5, 5, 5, 5),
            legend.key = element_rect(fill = "transparent"),
            strip.background = element_rect(fill="#FFFFFF", color = "#808080", linewidth = 0.5),
            strip.text = element_text(size = 8, margin = margin(2, 2, 2, 2))
        ) +
    guides(
        color = guide_legend(
            label.position = "top", title = NULL, nrow = 1,
            override.aes = list(linewidth = 1.5)
        )
    ) +
    scale_color_jama()

ggsave("Plots/Output/Risk_all.png", last_plot(), width = 16, height = 13.5, units = "cm", dpi = 600)

# alternative shortfall measures Chart
#################
# chart only for entire time 
plot_df <- measures |>
    filter(type %in% c("avg_ltm", "max_ltm", "n_breaches"), period  == "all") 

plot_df |>
    ggplot(aes(x = lambda, y = values, color = model, group = model)) +
    geom_line(position = position_jitter(width = .0022, height = 0), linewidth = .4) +
    scale_x_continuous(
        expand = expansion(add = c(.01, .01))
    ) +
    labs(
        title = "Loss to Margin and Number of Breaches",
        subtitle = "Minimal random noise added to data to avoid overlapping lines", 
        caption = "Own Depiction", 
        x = expression(lambda), 
        y = NULL
    ) +
    geom_vline(xintercept = .96, linetype = "dashed", color = "darkgrey") +
    facet_wrap(~ type, scales = "free") +
    theme(
            text = element_text(family= "lmroman", colour = "#555555"),
            legend.position = "bottom",
            legend.key.width = unit(1.4, "cm"),
            legend.background = element_rect(fill="transparent", colour = "#cccccc", linewidth = 0),
            legend.justification = .5,
            plot.subtitle = element_text(size = 8),
            plot.caption = element_text(size = 8, margin = margin(0, 0, 0, 0)),
            panel.border = element_rect(colour="#999999", fill = "transparent"),
            panel.background = element_rect(fill="#FFFFFF", colour="#999999", linewidth = 0),
            panel.grid.minor.y = element_line(colour = "#eeeeee", linewidth = 0.5),
            panel.grid.major = element_line(colour = "#eeeeee", linewidth = 0.5),
            panel.grid.minor = element_blank(),
            plot.background = element_rect(fill = "#F9F9F9", colour="#CCCCCC", linewidth = 0, linetype = 1),
            legend.box.spacing = unit(-.2, "cm"), 
            #legend.box.margin = margin(0, 0, 0, 0),
            axis.ticks = element_blank(),
            axis.text = element_text(size = 6),
            axis.text.y = element_text(margin = margin(0, 0, 0, 0)),
            axis.text.x = element_text(margin = margin(0, 0, 0, 0)),
            axis.title = element_text(size = 8),
            plot.title = element_text(size = 10, face = "bold"), 
            legend.direction = "vertical",
            legend.text = element_text(size = 8, margin = margin(b = -6, 0, 0, 0)),
            plot.margin = margin(5, 5, 5, 5),
            legend.key = element_rect(fill = "transparent"),
            strip.background = element_rect(fill="#FFFFFF", color = "#808080", linewidth = 0.5),
            strip.text = element_text(size = 8, margin = margin(2, 2, 2, 2))
        ) +
    guides(
        color = guide_legend(
            label.position = "top", title = NULL, nrow = 1,
            override.aes = list(linewidth = 1.5)
        )
    ) +
    scale_color_jama()

ggsave("Plots/Output/Risk_total.png", last_plot(), width = 16, height = 7, units = "cm", dpi = 600)


###################
###################
###################
# visualizations for short FESX
measures <- read_csv("Data/procyclicality_calculations_fesx_short.csv")

plot_df <- measures |>
    filter(period == "all", type %in% c("costs", "max_30d", "peak_to_through")) |>
    pivot_wider(names_from = type, values_from = values) |>
    pivot_longer(c(peak_to_through, max_30d), names_to = "measures", values_to = "values") |>
    mutate(label = ifelse(lambda == .92, model, ""))


for (i in c("cap", "speed", "baseline", "floor", "buffer", "cap_floor", "speed_floor")) {
    plot_df |>
        filter(model %in% c(i, "baseline")) |>
        ggplot(aes(x = round(costs * 100, 3), y = values, color = lambda, shape = model)) +
        geom_point() +
        geom_point(
            data = plot_df |> filter(model %in% c(i, "baseline"), lambda == .96),
            color = "red", show.legend = FALSE
        ) +
        scale_color_gradientn(
            colors = c("white", "grey", "darkgrey", "black"),
            breaks = c(.9, .95, .99)
        ) +
        scale_x_continuous(
            breaks = seq(7, 11, by = .2)
        ) +
        geom_text_repel(aes(label = label), color = "black", size = 2.5, nudge_x = .2, nudge_y = .1, family = "lmroman") +
        labs(
            title = paste("Procyclicality", i, sep = " "),
            x = "Avg. Costs (%)",
            y = NULL,
            color = expression(lambda)
        ) +
        theme(
            legend.position = "right",
            legend.title = element_text(family = "sans", hjust = .2),
            strip.text = element_text(size = 8, margin = margin(t = 3, b = 3, l = 0, r = 0))
        ) +
        guides(
            color = guide_colorbar(
                barwidth = .35,
                title.position = "top",
                title.vjust = 1,
                direction = "vertical"
            ),
            shape = "none"
        ) +
        facet_wrap(~measures) +
        scale_color_jama()

    ggsave(
        paste0("Plots/Output/", i, ".png"), last_plot(),
        width = 16, height = 7, unit = "cm"
    )
}

# alternative shortfall measures Chart
#################
#################
#################
#################
plot_df <- measures |>
    filter(type %in% c("avg_ltm", "max_ltm", "n_breaches"), period != "all") 

plot_df |>
    ggplot(aes(x = lambda, y = values, color = model, group = model)) +
    geom_line(
        data = plot_df |> filter(type == "n_breaches"),
        position = position_jitter(width = .0015, height = 0), size = .4
    ) +
    geom_line(
        data = plot_df |> filter(type == "max_ltm"),
        position = position_jitter(width = .002, height = .01), size = .4
    ) +
    geom_line(
        data = plot_df |> filter(type == "avg_ltm"),
        position = position_jitter(width = .001, height = .001), size = .4
    ) +
    scale_x_continuous(
        expand = expansion(add = c(.01, .01))
    ) +
    labs(
        title = "Loss to Margin and Breaches - Stress Periods (FESX Short)",
        subtitle = "Minimal random noise added to data to avoid overlapping lines", 
        caption = "Own Depiction", 
        x = expression(lambda), 
        y = NULL
    ) +
    geom_vline(xintercept = .96, linetype = "dashed", color = "darkgrey") +
    facet_grid2(period ~ type, scales = "free", independent = "y") +
    theme(
        legend.position = "bottom",
        plot.title = element_text(size = 12, face = "bold"),
        axis.title = element_text(size = 10),
        panel.grid = element_line(color = "transparent"), 
        axis.title.x = element_text(family = "sams"),
        text = element_text(family = "lmroman"),
        strip.background = element_rect(color = "transparent", fill = "transparent"),
        panel.background = element_rect(color = "black", fill = "white"),
        strip.placement = "outside",
        legend.key = element_rect(color = "white", fill = "white"),
        legend.text = element_text(size = 8, margin = margin(b = -5, 0, 0, 0)),
        legend.key.width = unit(1.4, "cm"),
        legend.box.spacing = unit(0, "cm"), 
        plot.subtitle = element_text(size = 8), 
        plot.caption = element_text(size = 8)
    ) +
    guides(
        color = guide_legend(
            label.position = "top", title = NULL, nrow = 1,
            override.aes = list(linewidth = 1.5)
        )
    ) +
    scale_color_jama()

ggsave("Plots/Output/Risk_all_short.png", last_plot(), width = 16, height = 10.5, units = "cm", dpi = 600)

# alternative shortfall measures Chart
#################
# chart only for entire time 
plot_df <- measures |>
    filter(type %in% c("avg_ltm", "max_ltm", "n_breaches"), period  == "all") 

plot_df |>
    ggplot(aes(x = lambda, y = values, color = model, group = model)) +
    geom_line(position = position_jitter(width = .0022, height = 0), linewidth = .4) +
    scale_x_continuous(
        expand = expansion(add = c(.01, .01))
    ) +
    labs(
        title = "Loss to Margin and Breaches (FESX Short)",
        subtitle = "Minimal random noise added to data to avoid overlapping lines", 
        caption = "Own Depiction", 
        x = expression(lambda)
    ) +
    geom_vline(xintercept = .96, linetype = "dashed", color = "darkgrey") +
    facet_wrap(~ type, scales = "free") +
    theme(
        legend.position = "bottom",
        plot.title = element_text(size = 12, face = "bold"),
        axis.title = element_text(size = 10),
        panel.grid = element_line(color = "transparent"), 
        axis.title.x = element_text(family = "sams"),
        text = element_text(family = "lmroman"),
        strip.background = element_rect(color = "transparent", fill = "transparent"),
        panel.background = element_rect(color = "black", fill = "white"),
        strip.placement = "outside",
        legend.key = element_rect(color = "white", fill = "white"),
        legend.text = element_text(size = 8, margin = margin(b = -5, 0, 0, 0)),
        legend.key.width = unit(1.4, "cm"),
        legend.box.spacing = unit(0, "cm"), 
        plot.subtitle = element_text(size = 8), 
        plot.caption = element_text(size = 8)
    ) +
    guides(
        color = guide_legend(
            label.position = "top", title = NULL, nrow = 1,
            override.aes = list(linewidth = 1.5)
        )
    ) +
    scale_color_jama()

ggsave("Plots/Output/Risk_total_short.png", last_plot(), width = 16, height = 9, units = "cm", dpi = 600)

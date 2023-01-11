# load libraries
library(tidyvere)
library(ggrepel)
library(ggh4x)

measures <- read_csv("Data/procyclicality_calculations_CAP95.csv")


plot_df <- measures |>
    filter(period == "all", type %in% c("costs", "max_30d", "peak_to_through")) |>
    pivot_wider(names_from = type, values_from = values) |>
    pivot_longer(c(peak_to_through, max_30d), names_to = "measures", values_to = "values") |>
    mutate(label = ifelse(lambda == .92, model, ""))

plot_df |>
    filter(model %in% c("baseline", "cap_floor"), measures == "peak_to_through") |>
    view()

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
        facet_wrap(~measures)

    ggsave(
        paste0("Plots/Output/", i, ".png"), last_plot(),
        width = 16, height = 7, unit = "cm"
    )
}


plot_df <- measures |>
    filter(type %in% c("max_30d", "peak_to_through", "costs"), period == "all") |>
    pivot_wider(names_from = type, values_from = values) |>
    pivot_longer(c(peak_to_through, max_30d), names_to = "measures", values_to = "values") |>
    mutate(label = ifelse(lambda == .995, model, NA))

plot_df |>
    ggplot(aes(y = values, x = costs, color = model)) +
    geom_point(aes(alpha = lambda),
        data = plot_df |> filter(period == "all" & lambda != .96), size = 1
    ) +
    geom_point(
        data = plot_df |> filter(lambda == .96),
        color = "red", size = 2, show.legend = FALSE
    ) +
    geom_text_repel(
        color = "black",
        aes(label = label),
        show.legend = FALSE, max.overlaps = 1000
    ) +
    guides(color = "none") +
    labs(
        title = "Comparison of APC Tools"
    ) +
    facet_wrap(~measures, scales = "free_y")

ggsave(
    "Plots/Output/combined_murphey.png", last_plot(),
    width = 16, height = 12, unit = "cm"
)

# alternative shortfall measures
plot_df <- measures |>
    mutate(type = ifelse(period == "2020", paste0(type, "_2020"), type)) |>
    filter(type %in% c("avg_shortfall", "max_shortfall", "n_breaches", "n_breaches_2020")) |>
    mutate(
        values = ifelse(type %in% c("avg_shortfall", "max_shortfall"), values * -100, values),
        label = ifelse(lambda == .995, model, NA)
    )

# abbreviations
# buffer --> b
# buffer_floored --> bf
# cap --> c
# cap_floord --> cf
# baseline --> bl
# floored --> f
# speed --> s
# speed floored --> sf

# set labels
lambda_label <- .995
plot_df <- measures |>
    mutate(label = case_when(
        (lambda == lambda_label & model == "buffer" & type == "max_shortfall") ~ "buffer",
        (lambda == lambda_label & period != "covid" & model == "cap" & type == "max_shortfall") ~ "other",
        (lambda == lambda_label & period == "covid" & model == "speed" & type == "max_shortfall") ~ "speed",
        (lambda == lambda_label & period == "covid" & model == "cap" & type == "max_shortfall") ~ "other",
        (lambda == lambda_label & model == "buffer" & type == "n_breaches") ~ "buffer",
        (lambda == lambda_label & period == "covid" & model == "speed" & type == "max_shortfall") ~ "speed",
        (lambda == lambda_label & period == "dotcom" & model == "cap" & type == "n_breaches") ~ "other",
        (lambda == lambda_label & period == "financialcrisis" & model == "cap" & type == "n_breaches") ~ "other + ?",
        (lambda == lambda_label & period == "financialcrisis" & model == "floor" & type == "n_breaches") ~ "other + ?",
        (lambda == lambda_label & period == "covid" & model == "buffer" & type == "avg_shortfall") ~ "buffer",
        (lambda == lambda_label & period == "covid" & model == "cap" & type == "avg_shortfall") ~ "cap",
        (lambda == lambda_label & period == "covid" & model == "cap" & type == "n_breaches") ~ "other",
        TRUE ~ NA_character_
    )) |>
    filter(type %in% c("n_breaches", "avg_shortfall", "max_shortfall"), period != "all") |>
    mutate(
        values = ifelse(grepl("shortfall", type), values * -100, values)
    )
plot_df |>
    ggplot(aes(x = lambda, y = values, color = model, group = model)) +
    geom_line() +
    geom_point(data = plot_df |> filter(lambda == .995)) +
    scale_x_continuous(
        expand = expansion(add = c(.01, .05))
    ) +
    labs(
        title = "Shortfall and Number of Breaches - Stress Periods",
        x = expression(lambda)
    ) +
    geom_text_repel(aes(label = label, ), hjust = 0, size = 3, direction = "y", max.overlaps = 10000) +
    geom_vline(xintercept = .96, linetype = "dashed", color = "darkgrey") +
    # facet_nested_wrap(period ~ type, scales = "free_y") +
    facet_grid2(period ~ type, scales = "free", independent = "y") +
    # guides(color = "none") +
    theme(
        legend.position = "bottom",
        axis.title.x = element_text(family = "sams"),
        text = element_text(family = "lmroman"),
        strip.background = element_rect(color = "transparent", fill = "transparent"),
        panel.background = element_rect(color = "black", fill = "white"),
        strip.placement = "outside"
    ) +
    scale_colour_wsj()
ggsave("Plots/Output/Risk_all.png", last_plot(), width = 17.6, height = 12, unit = "cm")


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

start_date <- as.Date("2006-01-01")
end_date <- as.Date("2021-01-01")

# plot of short and long margin in baseline scenario!
args_long_FESX <-
    list(
        MPOR = 3, factor = 1.37, quantile = .978,
        lambda = .9593, n_day = 750, burn_in = 350,
        absolute = FALSE, liq_group = "PEQ01",
        short = FALSE
    )

args_FGBL <- list(
    MPOR = 2, factor = 1.29, quantile = 0.978,
    lambda = .9727, n_day = 750, burn_in = 350,
    absolute = FALSE, liq_group = "PEQ01",
    short = FALSE
)

tic()
long_FESX_reg <- calculate_margin(
    product = "FESX", start = as.Date("2018-01-01"), end = as.Date("2023-01-01"),
    args = args_long_FESX, steps = TRUE
)
toc()

margin_baseline_short <- calculate_fhs_margin(
    product = "FESX", start = start_date, end = end_date,
    args = args_short_FESX, steps = TRUE
)

df <- margin_baseline_long |>
    left_join(margin_baseline_short, by = "DATE") |>
    select(DATE, RET_MPOR.x, MARGIN.x, MARGIN.y) |>
    rename(
        RET_MPOR_LONG = RET_MPOR.x,
        MARGIN_LONG = MARGIN.x, MARGIN_SHORT = MARGIN.y
    ) |>
    mutate(COLOR = case_when(
        (lag(RET_MPOR_LONG, 0) < -MARGIN_LONG) | (lag(RET_MPOR_LONG, 0) > MARGIN_SHORT) ~ "red",
        TRUE ~ "black"
    ))

long_FESX_reg |>
    ggplot(aes(DATE, MARGIN * -1)) +
    geom_line(color = "blue") +
    geom_line(data = long_FESX_reg, color = "red")

df |>
    ggplot(aes(x = DATE)) +
    geom_line(aes(y = MARGIN_LONG * -1)) +
    geom_line(aes(y = MARGIN_SHORT)) +
    geom_jitter(aes(y = lag(RET_MPOR_LONG, 3)), color = df$COLOR) +
    scale_y_continuous(breaks = seq(-.2, .2, .05)) +
    long |>
    ggplot(aes(DATE, MARGIN)) +
    geom_line()
test |>
    filter(BUCKET == 1) |>
    slice(1:250) |>
    arrange((revalued)) |>
    filter(revalued < -0.0885) |>
    pull(revalued)


tibble(NULL) |>
    ggplot(aes(x = DATE, y = MARGIN)) +
    geom_line(data = margin_baseline_long_3, color = "red") +
    geom_line(data = margin_baseline_long_4, color = "blue") +
    geom_line(data = margin_baseline_long_2, color = "green") +
    geom_hline(yintercept = .088) +
    geom_hline(yintercept = .094)

summary_stats(long_FESX_reg, start_covid, end_covid)
summary_stats(margin_baseline_long_4, start_2020, end_2020)
summary_stats(margin_baseline_long_2, start_date, end_date)
margin_baseline_long_margin_baseline_long_3$MARGIN
mean(x, na.rm = TRUE)


x <- pmax(margin_baseline_long_3$MARGIN, margin_baseline_long_4$MARGIN, na.rm = TRUE)

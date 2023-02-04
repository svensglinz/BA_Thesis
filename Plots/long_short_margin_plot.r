# load necessary libraries
library(tidyverse)
library(showtext)
# import written functions and store master sheet in memory
source("functions.R")
master <- read_master("Data/data_input.xlsx")

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

start_all <- as.Date("2001-03-20")
end_all <- as.Date("2023-01-01")
start_regular <- as.Date("2007-04-20")

args_long_FESX <- list(
    MPOR = 3, factor = 1.37, quantile = 0.978,
    lambda = .96, n_day = 750, burn_in = 750,
    liq_group = "PEQ01", short = FALSE,
    mean = TRUE
)

args_long_FESX_dotcom <- list(
    MPOR = 3, factor = 1.37, quantile = 0.978,
    lambda = .96, n_day = 500, burn_in = 200,
    liq_group = "PEQ01", short = FALSE,
    mean = FALSE
)

args_short_FESX <- list(
    MPOR = 3, factor = 1.37, quantile = 0.978,
    lambda = .96, n_day = 750, burn_in = 750,
    liq_group = "PEQ01", short = TRUE,
    mean = TRUE
)

args_short_FESX_dotcom <- list(
    MPOR = 3, factor = 1.37, quantile = 0.978,
    lambda = .96, n_day = 500, burn_in = 200,
    liq_group = "PEQ01", short = TRUE,
    mean = FALSE
)

# long margin
fesx_long_regular <- calculate_fhs_margin("FESX", start_regular, end_all, args = args_long_FESX, steps = TRUE)
fesx_long_dotcom <- calculate_fhs_margin("FESX", start_all, start_regular - 1, args = args_long_FESX_dotcom, steps = TRUE)

# short margin
fesx_short_regular <- calculate_fhs_margin("FESX", start_regular, end_all, args = args_short_FESX, steps = FALSE)
fesx_short_dotcom <- calculate_fhs_margin("FESX", start_all, start_regular - 1, args = args_short_FESX_dotcom, steps = FALSE)

# paste regular & dotcom margin periods together
fesx_long <- bind_rows(fesx_long_regular, fesx_long_dotcom)
fesx_short <- bind_rows(fesx_short_regular, fesx_short_dotcom)


# join long & short & mark breaches
fesx_margin <- fesx_long |>
    full_join(fesx_short, by = "DATE") |>
    rename(MARGIN_LONG = MARGIN.x, MARGIN_SHORT = MARGIN.y) |>
    mutate(
        BREACH = case_when(
            lag(RET_MPOR, 3) < -MARGIN_LONG ~ TRUE,
            lag(RET_MPOR, 3) > MARGIN_SHORT ~ TRUE,
            TRUE ~ FALSE
        ),
        COLOR = ifelse(BREACH, "red", "black"),
        SIZE = ifelse(BREACH, .8, .5)
    )

write_csv(fesx_margin, "fesx_margin.csv")
fesx_margin <- read_csv("fesx_margin.csv")

# assemble plot
a <- fesx_margin |>
    ggplot(aes(x = DATE)) +
    geom_line(aes(y = MARGIN_LONG * -1)) +
    geom_line(aes(y = MARGIN_SHORT)) +
    geom_point(
        aes(y = lag(RET_MPOR, 3), color = I(COLOR), size = I(SIZE)),
        position = position_jitter()
    ) +
    geom_hline(yintercept = -.0695, color = "#0c6b9a", linewidth = .5) + # long floor
    geom_hline(yintercept = .0686, color = "#0c6b9a", linewidth = .5) + # short floor
    geom_hline(yintercept = -.1512, color = "#ccb997", linewidth = .5) + # long cap
    geom_hline(yintercept = .1414, color = "#ccb997", linewidth = .5) + # short cap
    geom_hline(yintercept = -0.1446, color = "#826448", linewidth = .5) + # long buffer
    geom_hline(yintercept = 0.1347, color = "#826448", linewidth = .5) + # short buffer
    annotate("label", x = as.Date("2023-05-01"), y = .0686, label = "floor", hjust = 0, size = 2.5, label.padding = unit(.15, "lines")) +
    annotate("label", x = as.Date("2023-05-01"), y = .146, label = "cap", hjust = 0, size = 2.5, label.padding = unit(.15, "lines")) +
    annotate("label", x = as.Date("2023-05-01"), y = .129, label = "buffer", hjust = 0, size = 2.5, label.padding = unit(.15, "lines")) +
    labs(
        x = NULL,
        y = NULL,
        title = "Margin",
        caption = "Own Depiction"
    ) +
    scale_y_continuous(
        breaks = seq(-.2, .2, .05),
        labels = scales::label_percent(),
        limits = c(-.2, .2)
    ) +
    scale_x_date(
        breaks = seq(as.Date("2001-01-01"), as.Date("2023-01-01"), by = "2 years"),
        labels = scales::label_date(format = "%y")
    ) +
    theme(
        text = element_text(family = "lmroman", colour = "#555555"),
        legend.position = "bottom",
        legend.key.width = unit(1.4, "cm"),
        legend.background = element_rect(fill = "transparent", colour = "#cccccc", linewidth = 0),
        legend.justification = .5,
        plot.subtitle = element_text(size = 8),
        plot.caption = element_text(size = 8, margin = margin(0, 0, 0, 0)),
        panel.border = element_rect(colour = "#999999", fill = "transparent"),
        panel.background = element_rect(fill = "#FFFFFF", colour = "#999999", linewidth = 0),
        panel.grid.minor.y = element_line(colour = "#eeeeee", linewidth = 0.5),
        panel.grid.major = element_line(colour = "#eeeeee", linewidth = 0.5),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#F9F9F9", colour = "#CCCCCC", linewidth = 0, linetype = 1),
        legend.box.spacing = unit(-.2, "cm"),
        # legend.box.margin = margin(0, 0, 0, 0),
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
        strip.background = element_rect(fill = "#FFFFFF", color = "#808080", linewidth = 0.5),
        strip.text = element_text(size = 8, margin = margin(2, 2, 2, 2))
    ) +
    coord_cartesian(xlim = c(start_all, end_all), clip = "off")

# save chart
ggsave("Plots/Output/long_short_margin.png",
    plot = a, width = 16.3, height = 9.5, units = "cm", dpi = 350
)

# plot without lines!
fesx_margin |>
    ggplot(aes(x = DATE)) +
    geom_line(aes(y = MARGIN_LONG * -1, color = "Long")) +
    geom_line(aes(y = MARGIN_SHORT, color = "Short")) +
    geom_point(
        aes(y = lag(RET_MPOR, 3), size = I(SIZE), shape = "lagged 3-day returns"),
        position = position_jitter(), color = fesx_margin$COLOR
    ) +
    labs(
        x = NULL,
        y = NULL,
        title = "FESX Margin (in % of Notional)",
        caption = "Own Depiction"
    ) +
    scale_y_continuous(
        breaks = seq(-.2, .2, .05),
        labels = scales::label_percent(),
        limits = c(-.2, .2)
    ) +
    scale_x_date(
        expand = expansion(mult = c(.01, .01)),
        breaks = seq(as.Date("2001-01-01"), as.Date("2023-01-01"), by = "2 years"),
        labels = scales::label_date(format = "%y")
    ) +
    theme(
        text = element_text(family = "lmroman", colour = "#555555"),
        legend.position = "bottom",
        legend.key.width = unit(.5, "cm"),
        legend.background = element_rect(fill = "transparent", colour = "#cccccc", linewidth = 0),
        legend.justification = .5,
        plot.subtitle = element_text(size = 8),
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
        axis.title = element_text(size = 8),
        plot.title = element_text(size = 10, face = "bold"),
        legend.direction = "horizontal",
        legend.text = element_text(size = 8, margin = margin(b = -6, 0, 0, 0)),
        plot.margin = margin(5, 5, 5, 5),
        legend.key = element_rect(fill = "transparent"),
        strip.background = element_rect(fill = "#FFFFFF", color = "#808080", linewidth = 0.5),
        strip.text = element_text(size = 8, margin = margin(2, 2, 2, 2))
    ) +
    ggsci::scale_color_jama() +
    # scale_color_manual(values = c("Long" = "#374E55FF", "Short" = "#80796BFF")) +
    guides(
        color = guide_legend(
            label.position = "top", title = NULL,
            override.aes = list(linewidth = 1.5)
        ),
        shape = guide_legend(
            label.position = "top", title = NULL,
            override.aes = list(size = 2)
        )
    )

# save chart
ggsave("Plots/Output/recent.png", width = 16.3, height = 9.5, units = "cm", dpi = 600)

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

# assemble plot FESX long
fesx_margin |>
    mutate(
        RET_MPOR = ifelse(RET_MPOR < max(-fesx_margin$MARGIN_LONG), RET_MPOR, NA),
        SIZE = ifelse(SIZE == .5, .3, SIZE)
    ) |>
    ggplot(aes(x = DATE, y = MARGIN_LONG * -1)) +
    geom_line(linewidth = .3) +
    geom_point(
        aes(y = lag(RET_MPOR, 3), color = I(COLOR), size = I(SIZE)),
        position = position_jitter()
    ) +
    geom_hline(aes(yintercept = -.0695, linetype = "Floor"), linewidth = .3, color = "#B24745FF") + # long floor
    geom_hline(aes(yintercept = -.1512, linetype = "Cap"), color = "#DF8F44FF", linewidth = .3) + # long cap
    geom_hline(aes(yintercept = -0.1446, linetype = "Release Buffer"), color = "#00A1D5FF", linewidth = .3) + # long buffer
    labs(
        x = NULL,
        y = NULL,
        title = "Long FESX Margin (in % of Notional)",
        subtitle = "Only returns below the minimum margin requirment are shown"
    ) +
    scale_y_continuous(
        breaks = seq(-.2, .2, .05),
        labels = scales::label_percent(),
        expand = expansion(mult = c(.01, .01))
    ) +
    scale_x_date(
        breaks = seq(as.Date("2001-01-01"), as.Date("2023-01-01"), by = "2 years"),
        labels = scales::label_date(format = "%y"),
        expand = expansion(mult = c(.01, .01))
    ) +
    scale_linetype_manual(values = c(1, 1, 1), breaks = c("Floor", "Release Buffer", "Cap"), labels = c("Floor", "Release Buffer", "Cap")) +
    theme(
        text = element_text(family = "lmroman", colour = "#555555"),
        legend.position = "bottom",
        legend.key.width = unit(.3, "cm"),
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
        legend.box.spacing = unit(-.35, "cm"),
        axis.ticks = element_blank(),
        axis.text = element_text(size = 6),
        axis.text.y = element_text(margin = margin(0, 0, 0, 0)),
        axis.text.x = element_text(margin = margin(0, 0, 0, 0)),
        axis.title = element_text(size = 8),
        plot.title = element_text(size = 10, face = "bold"),
        legend.direction = "vertical",
        legend.text = element_text(size = 8, margin = margin(b = 0, 0, 0, 0)),
        plot.margin = margin(5, 5, 5, b = 0),
        legend.key = element_rect(fill = "transparent"),
    ) +
    guides(
        linetype = guide_legend(
            title = NULL, label.position = "left",
            nrow = 1,
            override.aes = list(color = c("#B24745FF", "#00A1D5FF", "#DF8F44FF"), linetype = "solid")
        )
    ) +
    coord_cartesian(
        xlim = c(start_all, end_all),
        ylim = c(-.2, -0.035)
    )

# save chart
ggsave("Plots/Output/margin_long_with_APC.png",
    plot = last_plot(), width = 16.3, height = 6, units = "cm", dpi = 600
)

# assemble plot FESX Short
fesx_margin |>
    mutate(
        RET_MPOR = ifelse(RET_MPOR > min(fesx_margin$MARGIN_SHORT), RET_MPOR, NA),
        SIZE = ifelse(SIZE == .5, .3, SIZE)
    ) |>
    ggplot(aes(x = DATE, y = MARGIN_SHORT)) +
    geom_line(linewidth = .3) +
    geom_point(
        aes(y = lag(RET_MPOR, 3), color = I(COLOR), size = I(SIZE - .4)),
        position = position_jitter()
    ) +
    geom_hline(aes(yintercept = .0686, linetype = "Floor"), linewidth = .3, color = "#B24745FF") + # long floor
    geom_hline(aes(yintercept = .1414, linetype = "Cap"), color = "#DF8F44FF", linewidth = .3) + # long cap
    geom_hline(aes(yintercept = 0.1347, linetype = "Release Buffer"), color = "#00A1D5FF", linewidth = .3) + # long buffer
    labs(
        x = NULL,
        y = NULL,
        title = "Short FESX Margin (in % of Notional)",
        subtitle = "Only returns below the minimum margin requirment are shown"
    ) +
    scale_y_continuous(
        breaks = seq(-.2, .2, .05),
        labels = scales::label_percent(),
        expand = expansion(mult = c(.01, .01))
    ) +
    scale_x_date(
        breaks = seq(as.Date("2001-01-01"), as.Date("2023-01-01"), by = "2 years"),
        labels = scales::label_date(format = "%y"),
        expand = expansion(mult = c(.01, .01))
    ) +
    scale_linetype_manual(values = c(1, 1, 1), breaks = c("Floor", "Release Buffer", "Cap"), labels = c("Floor", "Release Buffer", "Cap")) +
    theme(
        text = element_text(family = "lmroman", colour = "#555555"),
        legend.position = "bottom",
        legend.key.width = unit(.3, "cm"),
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
        legend.box.spacing = unit(-.35, "cm"),
        axis.ticks = element_blank(),
        axis.text = element_text(size = 6),
        axis.text.y = element_text(margin = margin(0, 0, 0, 0)),
        axis.text.x = element_text(margin = margin(0, 0, 0, 0)),
        axis.title = element_text(size = 8),
        plot.title = element_text(size = 10, face = "bold"),
        legend.direction = "vertical",
        legend.text = element_text(size = 8, margin = margin(b = 0, 0, 0, 0)),
        plot.margin = margin(5, 5, 5, b = 0),
        legend.key = element_rect(fill = "transparent"),
    ) +
    guides(
        linetype = guide_legend(
            title = NULL, label.position = "left",
            nrow = 1,
            override.aes = list(color = c("#B24745FF", "#00A1D5FF", "#DF8F44FF"), linetype = "solid")
        )
    ) +
    coord_cartesian(
        xlim = c(start_all, end_all),
        ylim = c(0.03, .2)
    )

# save chart
ggsave("Plots/Output/margin_short_with_APC.png",
    plot = last_plot(), width = 16.3, height = 6, units = "cm", dpi = 600
)

# plot without lines (Long and Short)
fesx_margin |>
    ggplot(aes(x = DATE)) +
    geom_line(aes(y = MARGIN_LONG * -1, color = "Long"), linewidth = .3) +
    geom_line(aes(y = MARGIN_SHORT, color = "Short"), linewidth = .3) +
    geom_point(
        aes(y = lag(RET_MPOR, 3), size = I(SIZE), shape = "lagged 3-day returns"),
        position = position_jitter(), color = fesx_margin$COLOR
    ) +
    # add invisible point for "Margin breach" legend element
    geom_point(
        aes(
            y = min(fesx_margin$MARGIN_LONG), x = min(fesx_margin$DATE),
            alpha = "Margin breach"
        ),
        size = 0
    ) +
    labs(
        x = NULL,
        y = NULL,
        title = "FESX Margin (in % of Notional)"
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
        legend.box.spacing = unit(0, "cm"),
        axis.ticks = element_blank(),
        axis.text = element_text(size = 6),
        axis.text.y = element_text(margin = margin(0, 0, 0, 0)),
        axis.text.x = element_text(margin = margin(0, 0, 0, 0)),
        axis.title = element_text(size = 8),
        plot.title = element_text(size = 10, face = "bold"),
        legend.direction = "horizontal",
        legend.text = element_text(size = 8, margin = margin(b = -6, 0, 0, 0)),
        plot.margin = margin(5, 5, 5, b = 0),
        legend.key = element_rect(fill = "transparent"),
    ) +
    ggsci::scale_color_jama(
        breaks = c("lagged 3-day returns", "Margin breach", "Long", "Short")
    ) +
    guides(
        shape = guide_legend(
            order = 1,
            label.position = "top", title = NULL,
            override.aes = list(size = 1.5)
        ),
        color = guide_legend(
            order = 3,
            label.position = "top", title = NULL
        ),
        alpha = guide_legend(
            order = 2,
            label.position = "top", title = NULL,
            override.aes = list(color = "red", alpha = 1, size = 1.5)
        )
    )

# save chart
ggsave(
    "Plots/Output/long_short_margin.png",
    width = 16.3, height = 7.5, units = "cm", dpi = 600
)

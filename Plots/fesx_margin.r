# load necessary libraries
library(tidyverse)
library(showtext)
library(scales)
library(glue)
library(ggsci)
source("functions.r")

master <- read_master("Data/data_input.xlsx")

# add fonts for plotting
font_add(
    family = "lmroman",
    regular = "Fonts/lmroman10_regular.ttf",
    bold = "Fonts/lmroman10_bold.ttf",
    italic = "Fonts/lmroman10_italic.ttf",
    bolditalic = "Fonts/lmroman10_bolditalic.ttf",
    symbol = "Fonts/lmroman_math.ttf"
)

set_plot_theme()
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
        SIZE = ifelse(BREACH, .8, .3)
    )

# tibble with release levels for plotting
release_levels <- tibble(
    short = c(.0686, .1347, .1414),
    long = c(-.0695, -.1446, -.1512),
    tool = c("floor", "buffer", "cap")
)

#############################
# generate plots with release levels
#############################
for (i in c("short", "long")) {
    # motify plot df
    plot_df <- fesx_margin |>
        select(DATE, (glue("MARGIN_{toupper(i)}")), RET_MPOR, COLOR, SIZE) |>
        rename(MARGIN = glue("MARGIN_{toupper(i)}")) |>
        mutate(
            MARGIN = MARGIN * ifelse(i == "long", -1, 1),
            RET_MPOR = case_when(
                RET_MPOR < max(MARGIN) & i == "long" ~ RET_MPOR,
                RET_MPOR > min(MARGIN) & i == "short" ~ RET_MPOR,
                TRUE ~ NA_real_
            )
        )

    plot_df |>
        ggplot(aes(x = DATE, y = MARGIN)) +
        geom_line(linewidth = .3) +
        geom_point(
            aes(y = lag(RET_MPOR, 3), color = I(COLOR), size = I(SIZE)),
            position = position_jitter()
        ) +
        geom_hline(
            aes(yintercept = release_levels[release_levels$tool == "floor", i][[1]], linetype = "Floor"),
            color = "#B24745FF", linewidth = .3,
        ) +
        geom_hline(
            aes(yintercept = release_levels[release_levels$tool == "cap", i][[1]], linetype = "Cap"),
            color = "#DF8F44FF", linewidth = .3
        ) +
        geom_hline(
            aes(yintercept = release_levels[release_levels$tool == "buffer", i][[1]], linetype = "Release Buffer"),
            color = "#00A1D5FF", linewidth = .3
        ) +
        labs(
            x = NULL,
            y = NULL,
            title = glue("{str_to_title(i)} FESX Margin (in % of Notional)"),
            subtitle = glue("Only returns {ifelse(i == 'short', 'above', 'below')} the minimum (absolute) margin requirment are shown")
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
        scale_linetype_manual(
            values = c(1, 1, 1), breaks = c("Floor", "Release Buffer", "Cap"),
            labels = c("Floor", "Release Buffer", "Cap")
        ) +
        guides(
            linetype = guide_legend(
                title = NULL, label.position = "left", nrow = 1,
                override.aes = list(
                    color = c("#B24745FF", "#00A1D5FF", "#DF8F44FF"),
                    linetype = "solid"
                )
            )
        )

    # save chart
    ggsave(
        glue("Plots/Output/release_levels_{i}.svg"),
        device = "svg",
        plot = last_plot(), width = 16.3, height = 6, units = "cm", dpi = 600
    )
}

#############################
# generate plot with long and short margin
#############################

fesx_margin |>
    ggplot(aes(x = DATE)) +
    geom_line(aes(y = MARGIN_LONG * -1, color = "Margin long"), linewidth = .3) +
    geom_line(aes(y = MARGIN_SHORT, color = "Margin short"), linewidth = .3) +
    geom_point(
        aes(y = lag(RET_MPOR, 3), size = I(SIZE), shape = "lagged 3-day returns"),
        position = position_jitter(), color = fesx_margin$COLOR
    ) +
    # add invisible point for "Margin breach" legend element
    geom_point(aes(y = min(fesx_margin$MARGIN_LONG), x = min(fesx_margin$DATE), alpha = "Margin breach"), size = 0) +
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
    ) +
    scale_color_jama()

# save chart
ggsave(
    "Plots/Output/long_short_margin.svg",
    device = "svg", plot = last_plot(),
    width = 16.3, height = 7.5, units = "cm", dpi = 600
)

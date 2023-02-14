# load libraries
library(tidyverse)
library(showtext)
source("functions.r")

# add fonts for plotting
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

# set seed for replicability
set.seed(7)

# create paths
paths <- matrix(data = NA, nrow = 300, ncol = 20)
for (i in 1:20) paths[, i] <- rnorm(n = 300, mean = 0, sd = 1)

# cumsum over all steps --> Random motion
paths <- paths |>
    as.data.frame() |>
    sapply(cumsum) |>
    as_tibble()

# pivot longer for plotting of all paths by groups
paths$index <- 1:300

paths <- paths |>
    pivot_longer(-index, names_to = "values")

# mark colored paths
paths <- paths |>
    mutate(
        index = as.factor(index),
        col = case_when(
            values == "V1" ~ "#00A1D5FF",
            values == "V14" ~ "#DF8F44FF",
            TRUE ~ "grey50"
        ),
        width = case_when(
            values == "V1" ~ .4,
            values == "V14" ~ .4,
            TRUE ~ .2
        )
    )

# plot graph
paths |>
    ggplot(aes(x = index, y = value, color = I(col), group = values)) +
    geom_density(
        aes(y = value, after_stat(density) * 1000 + 301),
        inherit.aes = FALSE, linewidth = .3
    ) +
    geom_line(aes(linewidth = I(width)), show.legend = FALSE) +
    geom_vline(xintercept = 100, linetype = 2, linewidth = .3) +
    geom_vline(xintercept = 200, linetype = 2, linewidth = .3) +
    geom_hline(yintercept = 0, color = "#838383", linewidth = .3) +
    labs(
        x = NULL,
        y = "PnL",
        title = "Concept of Initial Margin"
    ) +
    scale_y_continuous(breaks = seq(from = -50, to = 50, by = 10)) +
    scale_x_discrete(
        breaks = c(1, 100, 200, 250, 300),
        labels = c("t", "t+1", "t+2", "...", "t+n")
    ) +
    coord_cartesian(clip = "off", xlim = c(0, 300)) +
    theme(plot.margin = margin(.1, .1, .1, r = 1.8, unit = "cm"))

# save output
ggsave("Plots/Output/IM_concept.svg",
    plot = last_plot(), device = "svg",
    dpi = 600, width = 12.89, height = 6.23, unit = "cm"
)

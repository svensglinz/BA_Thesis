# load libraries
library(tidyverse)
library(showtext)
library(patchwork)

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

# set seed for replicability
set.seed(7)

# create start path (identical for all paths)
start <- c(100, rnorm(n = 99, mean = 0, sd = 1))

# create diffusion & paste together with start path
paths <- matrix(data = NA, nrow = 400, ncol = 10)
for (i in 1:10) paths[, i] <- c(start, rnorm(n = 300, mean = 0, sd = 1))

# cumsum over all steps --> Random motion
paths <- paths |>
    as.data.frame() |>
    sapply(cumsum) |>
    as.data.frame()

# pivot longer for plotting of all paths by groups
# & assign color to paths
paths$index <- 1:400
paths$index <- as.factor(paths$index)
paths$col <- c(rep("black", 99), rep("darkgrey", 301))

paths <- paths |>
    pivot_longer(-c(index, col), names_to = "values")

# plot graph
graph <- paths |>
    ggplot(aes(x = index, y = value, group = values, color = I(col))) +
    geom_density(
        aes(y = value, after_stat(density) * 1000 + 401),
        inherit.aes = FALSE, linewidth = .3
    ) +
    geom_line(linewidth = .3) +
    geom_vline(xintercept = 100, linewidth = .3) +
    geom_segment(aes(
        y = paths$value[paths$index == 100][1], x = 100,
        yend = paths$value[paths$index == 100][1], xend = 400
    ), color = "#838383", linewidth = .3) +
    geom_vline(xintercept = 200, linetype = 2, linewidth = .3) +
    geom_vline(xintercept = 300, linetype = 2, linewidth = .3) +
    labs(
        x = NULL,
        y = NULL,
        title = "Concept of Initial Margin",
        caption = "Own Depiction"
    ) +
    scale_y_continuous(breaks = seq(from = 50, to = 140, by = 10)) +
    theme(
        text = element_text(family = "lmroman", colour = "#555555"),
        panel.border = element_rect(colour = "#999999", fill = "transparent"),
        panel.background = element_rect(fill = "#FFFFFF", colour = "#999999", linewidth = 0),
        panel.grid.minor.y = element_line(colour = "#eeeeee", linewidth = 0.5),
        panel.grid.major = element_line(colour = "#eeeeee", linewidth = 0.5),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#F9F9F9", colour = "#CCCCCC", linewidth = 0, linetype = 1),
        axis.ticks = element_blank(),
        axis.text = element_text(size = 6),
        axis.text.y = element_text(margin = margin(0, 0, 0, 0)),
        axis.text.x = element_text(margin = margin(0, 0, 0, 0)),
        axis.title = element_text(size = 8),
        plot.title = element_text(size = 10, face = "bold"),
        plot.margin = margin(.1, .1, .1, r = 1.8, unit = "cm"),
        plot.caption = element_text(size = 8)
    ) +
    scale_x_discrete(
        breaks = c(1, 100, 200, 300, 350, 400),
        labels = c(
            "1" = "t-1", "100" = "t",
            "200" = "t+1", "300" = "t+2",
            "350" = "...", "400" = "t+n"
        )
    ) +
    coord_cartesian(clip = "off", xlim = c(0, 400))

# save output
ggsave("Plots/Output/IM_graph.png",
    plot = graph,
    dpi = 600, width = 12.89, height = 6.23, unit = "cm"
)

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
showtext_opts(dpi = 350)

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
graph <-
    paths |>
    ggplot(aes(x = index, y = value, group = values, color = col)) +
    geom_line() +
    geom_vline(xintercept = 100) +
    geom_segment(aes(
        y = paths$value[paths$index == 100][1], x = 100,
        yend = paths$value[paths$index == 100][1], xend = 400
    ), color = "#838383") +
    geom_vline(xintercept = 200, linetype = 2) +
    geom_vline(xintercept = 300, linetype = 2) +
    labs(
        x = NULL,
        y = NULL,
        title = "Concept of Initial Margin",
        caption = "Own Depiction"
    ) +
    theme_minimal() +
    scale_y_continuous(breaks = seq(from = 50, to = 140, by = 10)) +
    theme(
        text = element_text(family = "lmroman"),
        panel.grid = element_blank(),
        panel.background = element_rect(color = "black"),
        legend.position = "right",
        axis.title = element_text(size = 8),
        plot.title = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 8),
        plot.caption = element_text(size = 8),
        plot.margin = margin(t = 0, b = 0, l = 0, r = 2, "cm")
    ) +
    scale_x_discrete(
        breaks = c(1, 100, 200, 300, 350, 400),
        labels = c(
            "1" = "t-1", "100" = "t",
            "200" = "t+1", "300" = "t+2",
            "350" = "...", "400" = "t+n"
        )
    ) +
    scale_color_identity()

# create density plot (density of all points of grey line, not only end path
# for "more normal-dist looking" density)
dens <- paths |>
    filter(col == "darkgrey") |>
    ggplot(aes(x = value)) +
    geom_density() +
    theme(
        panel.grid = element_blank(),
        panel.background = element_rect(
            fill = "transparent",
            color = "transparent"
        ),
        plot.background = element_rect(
            fill = "transparent",
            color = "transparent"
        ),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        plot.margin = margin(r = 0, l = 0, b = -1, t = -1, unit = "cm"),
        axis.title = element_blank()
    ) +
    coord_flip()

# assemble two plots
out <-
    graph + inset_element(dens,
        l = .99, b = .2, r = 1.2,
        t = .8, align_to = "panel"
    )

# save output
ggsave("Plots/Output/IM_graph.png",
    plot = out, device = "png",
    dpi = 350, width = 12.89, height = 6.23, unit = "cm"
)

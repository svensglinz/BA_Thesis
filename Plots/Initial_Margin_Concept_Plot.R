# load libraries
library(tidyverse)
library(ggExtra)
library(showtext)

# add fonts for plotting
font_add(
  family = "lmroman", regular = "Fonts/lmroman10_regular.ttf",
  bold = "Fonts/lmroman10_bold.ttf",
  italic = "Fonts/lmroman10_italic.ttf",
  bolditalic = "Fonts/lmroman10_bolditalic.ttf"
)

showtext_auto(enable = TRUE)
showtext_opts(dpi = 350)

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
out <-
    paths |>
    ggplot(aes(x = index, y = value, group = values, color = col)) +
    geom_point(size = 0, color = "white") + # points needed to add density below
    geom_line() +
    geom_vline(xintercept = 100) +
    geom_segment(aes(
        y = paths$value[paths$index == 100][1], x = 100,
        yend = paths$value[paths$index == 100][1], xend = 400
    ), color = "red") +
    geom_vline(xintercept = 200, linetype = 2) +
    geom_vline(xintercept = 300, linetype = 2) +
    labs(
        x = "Time",
        y = "Value",
        title = "Concept of Initial Margin"
    ) +
    theme_minimal() +
    scale_y_continuous(breaks = seq(from = 50, to = 130, by = 10)) +
    theme(
        text = element_text(family = "lmroman"),
        panel.grid = element_blank(),
        panel.background = element_rect(color = "black"),
        legend.position = "right",
        axis.title = element_text(size = 8),
        plot.title = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 8)
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

# add marginal distribution of returns
out <- ggMarginal(out, type = "density", margins = "y")

out <- out + theme(text = element_text(family = "lmroman"))
# save output
ggsave("Plots/Output/IM_graph.png",
    plot = out, device = "png",
    dpi = 350, height = 6.23, width = 12.9, units = "cm"
)

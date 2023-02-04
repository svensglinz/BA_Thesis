# load relevant packages
library(tidyverse)
library(scales)
library(ggsci)
library(showtext)
library(lubridate)

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

# define parameters
start_date <- as.Date("2020-03-01")
end_date <- as.Date("2020-03-31")

# load data set
imc <-
    read_csv("Data/Eurex_Data/IMC.csv",
        col_types = cols(FACT_DATE = col_date(format = "%d/%m/%Y"))
    )

# plot graph
out <-
    imc |>
    mutate(DAY = day(FACT_DATE)) |>
    filter(between(FACT_DATE, start_date, end_date)) |>
    ggplot(aes(x = as.factor(DAY), y = N_CALLS, fill = TYPE)) +
    geom_bar(stat = "identity", position = "stack") +
    scale_y_continuous(
        breaks = seq(from = 0, to = 90, by = 20),
        expand = expansion(mult = c(.01, .07))
    ) +
    scale_x_discrete(
        breaks = c("2", "4", "6", "10", "12", "16", "18", "20", "24", "26", "30"),
    ) +
    labs(
        title = "Number of IMC (March 2020)",
        x = NULL,
        y = NULL,
        fill = NULL,
        caption = "Own Depiction | Data Source: Eurex Clearing AG"
    ) +
    theme(
        text = element_text(family = "lmroman", colour = "#555555"),
        legend.position = "bottom",
        legend.background = element_rect(fill = "transparent", colour = "#cccccc", linewidth = 0),
        legend.justification = .5,
        legend.key.size = unit(.3, "cm"),
        plot.subtitle = element_text(size = 8),
        plot.caption = element_text(size = 8, margin = margin(t = 4, 0, 0, 0)),
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
    scale_fill_jama(
        labels = c("Initial Margin", "Variation Margin")
    )

# save output
ggsave("Plots/Output/IMC_March.png",
    plot = out,
    dpi = 600, width = 7.6, height = 6.2, units = "cm"
)

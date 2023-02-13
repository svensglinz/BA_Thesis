# load relevant packages
library(tidyverse)
library(scales)
library(ggsci)
library(showtext)
library(lubridate)
source("functions.R")

# add fonts for plotting
font_add(
    family = "lmroman",
    regular = "Fonts/lmroman10_regular.ttf",
    bold = "Fonts/lmroman10_bold.ttf",
    italic = "Fonts/lmroman10_italic.ttf",
    bolditalic = "Fonts/lmroman10_bolditalic.ttf",
    symbol = "Fonts/lmroman10_math.otf"
)

set_plot_theme()
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
        fill = NULL
    ) +
    scale_fill_jama(
        labels = c("Initial Margin", "Variation Margin")
    )

# save output
ggsave("Plots/Output/IMC_March.svg",
    plot = last_plot(), device = "svg",
    dpi = 600, width = 7.6, height = 6.2, units = "cm"
)

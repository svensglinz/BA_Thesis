# load relevant packages
library(tidyverse)
library(scales)
library(ggsci)
library(showtext)

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
start_date <- as.Date("2020-01-01")
end_date <- as.Date("2020-12-31")

# load data set
imc <-
    read_csv("Data/Eurex_Data/IMC.csv",
        col_types = cols(FACT_DATE = col_date(format = "%d/%m/%Y"))
    )

# modify and clean data
imc |>
    filter(between(FACT_DATE, start_date, end_date)) |>
    mutate(MONTH = format(FACT_DATE, "%b")) |>
    ggplot(
        aes(
            x = reorder(MONTH, FACT_DATE), y = VOLUME / 10^9,
            group = interaction(MONTH, TYPE), fill = TYPE
        )
    ) +
    geom_boxplot(outlier.size = .5, fatten = .8, linewidth = .3) +
    labs(
        x = NULL,
        y = "Bio. EUR",
        title = "Volume of Daily IMCs at Eurex (2020)",
        fill = NULL
    ) +
    scale_y_continuous(breaks = seq(0, 10, 2)) +
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
        legend.box.spacing = unit(0, "cm"),
        axis.ticks = element_blank(),
        axis.text = element_text(size = 6, margin = margin(0, 0, 0, 0)),
        axis.title = element_text(size = 8),
        plot.title = element_text(size = 10, face = "bold"),
        legend.direction = "horizontal",
        legend.text = element_text(size = 8, margin = margin(b = 0, 0, 0, 0)),
        plot.margin = margin(5, 5, 5, 5),
        legend.key = element_rect(fill = "transparent"),
    ) +
    scale_fill_jama(
        labels = c("Initial Margin", "Variation Margin")
    )

# save output
ggsave("Plots/Output/volume_IMC_2020.svg",
    plot = last_plot(), width = 8.33, height = 6.2,
    dpi = 600, unit = "cm", device = "svg"
)

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
    scale_fill_jama(
        labels = c("Initial Margin", "Variation Margin")
    )

# save output
ggsave("Plots/Output/volume_IMC_2020.svg",
    plot = last_plot(), width = 8.33, height = 6.2,
    dpi = 600, unit = "cm", device = "svg"
)

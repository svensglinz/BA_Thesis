# load relevant packages
library(tidyverse)
library(ggsci)
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

set_plot_theme()
showtext_auto(enable = TRUE)
showtext_opts(dpi = 600)

# define parameters
start_date <- as.Date("2020-01-01")
end_date <- as.Date("2020-12-31")
breaks <- c("EQUITY DERIVATIVES", "FI DERIVATIVES", "OTC IRS", "OTHER")
# load data
df <- read_csv("Data/Eurex_Data/IM per Product Class.csv",
    col_types = cols(FACT_DATE = col_date(format = "%d/%m/%Y"))
)

# filter & clean data
df <- df |>
    filter(between(FACT_DATE, start_date, end_date)) |>
    mutate(PRODUCT_GROUP = ifelse(PRODUCT_GROUP %in% c(
        "EQUITY DERIVATIVES",
        "FIXED INCOME DERIVATIVES", "OTC IRS"
    ), PRODUCT_GROUP, "OTHER")) |>
    group_by(FACT_DATE, PRODUCT_GROUP) |>
    summarize(IM_EUR = sum(IM_EUR))

# create plot
df |>
    ggplot(aes(x = FACT_DATE, y = IM_EUR / 10^9, fill = PRODUCT_GROUP)) +
    geom_area(position = "stack") +
    scale_y_continuous(
        breaks = seq(from = 0, to = 80, by = 20),
        expand = expansion(mult = c(.01, .05))
    ) +
    scale_x_date(
        breaks = seq.Date(
            from = start_date,
            to = end_date, by = "month"
        ), labels = scales::label_date(format = "%b"),
        expand = expansion(mult = c(.005, .005))
    ) +
    labs(
        title = "IM per Asset Class at Eurex Clearing (2020, in Bio EUR)",
        x = NULL,
        y = NULL,
        fill = NULL
    ) +
    scale_fill_jama(
        breaks = c("EQUITY DERIVATIVES", "FIXED INCOME DERIVATIVES", "OTC IRS", "OTHER"),
        labels = c("Equity Derivatives", "FI Derivatives", "OTC IRS", "Other")
    )

# save plot
ggsave("Plots/Output/IM_per_assetclass.svg",
    plot = last_plot(),
    device = "svg",
    dpi = 600, height = 6.6,
    width = 13.45, units = "cm"
)

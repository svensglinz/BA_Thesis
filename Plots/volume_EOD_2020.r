# load relevant packages
library(lubridate)
library(tidyverse)
library(ggsci)
library(showtext)
source("functions.R")

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

start_date <- as.Date("2020-01-01")
end_date <- as.Date("2020-12-31")

df <- read_csv("Data/Eurex_Data/VM_IM_Calls.csv",
    col_types = cols(FACT_DATE = col_date(format = "%d/%m/%Y"))
)

# filter out day with large option & index future expiries
plot_df <- df |>
    filter(TYP == "EOD", between(FACT_DATE, start_date, end_date)) |>
    mutate(
        DAY = wday(FACT_DATE),
        WEEK = stringi::stri_datetime_fields(FACT_DATE)$WeekOfMonth,
        MONTH_STR = format(FACT_DATE, "%b"),
        MONTH = month(FACT_DATE),
        EXPIRY = ifelse((DAY == 6 & WEEK == 3 & MONTH %in% c(3, 6, 9, 12)),
            TRUE, FALSE
        )
    )

plot_df |>
    filter(!EXPIRY) |>
    ggplot(
        aes(
            x = reorder(MONTH_STR, FACT_DATE),
            y = DEBIT_EUR / 10^9, group = interaction(MONTH, TYPE), fill = TYPE
        )
    ) +
    geom_boxplot(outlier.size = .5, fatten = .8, linewidth = .3) +
    labs(
        x = NULL,
        y = "Bio. EUR",
        title = "Daily settled Margins at Eurex (2020)",
        fill = NULL
    ) +
    scale_fill_jama(labels = c("Initial Margin", "Variation Margin"))

# save output
ggsave("Plots/Output/volume_EOD_2020.svg",
    plot = last_plot(), device = "svg",
    width = 8.33, height = 5.89, units = "cm", dpi = 600
)

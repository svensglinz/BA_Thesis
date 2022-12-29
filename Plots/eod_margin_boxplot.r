# load relevant packages
library(lubridate)
library(tidyverse)
library(scales)
library(ggsci)
library(showtext)

start_date <- as.Date("2020-01-01")
end_date <- as.Date("2020-12-31")

df <- read_csv("Data/Eurex_Data/VM_IM_Calls.csv",
    col_types = cols(FACT_DATE = col_date(format = "%d/%m/%Y"))
)

# filter out day with large option & index future expiries!
test <- df |>
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

out <- test |>
    filter(!EXPIRY) |>
    ggplot(
        aes(
            x = reorder(MONTH_STR, FACT_DATE),
            y = DEBIT_EUR / 10^9, group = interaction(MONTH, TYPE), fill = TYPE
        )
    ) +
    geom_boxplot(outlier.size = .5, fatten = .8) +
    labs(
        x = NULL,
        y = NULL,
        title = "Daily settled Margins (in Bio. EUR)",
        fill = NULL
    ) +
    theme_bw() +
    theme(
        text = element_text(family = "lmroman"),
        plot.title = element_text(size = 10, face = "bold"),
        plot.caption = element_text(size = 8, margin = margin(t = -.1, b = 0, r = 0, l = 0)),
        panel.grid = element_blank(),
        legend.key.size = unit(.3, "cm"),
        legend.text = element_text(size = 8),
        legend.position = "bottom",
        legend.margin = margin(t = 0, b = .2, l = 0, r = 0, unit = "cm"),
        plot.margin = margin(0, 0, 0, 0),
        legend.box.spacing = unit(10, "pt")
    ) +
    scale_fill_grey(start = .8, end = .5)

# save output
ggsave("Plots/Output/EOD_VM_IM_Boxplot.png",
    plot = out,
    device = "png", dpi = 350,
    width = 8.33, height = 5.89, units = "cm"
)

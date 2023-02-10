# load relevant packages
library(lubridate)
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

start_date <- as.Date("2020-01-01")
end_date <- as.Date("2020-12-31")

df <- read_csv("Data/Eurex_Data/VM_IM_Calls.csv",
    col_types = cols(FACT_DATE = col_date(format = "%d/%m/%Y"))
)

# filter out day with large option & index future expiries!
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
        y = NULL,
        title = "Daily settled Margins (Bio. EUR, 2020)",
        fill = NULL
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
    scale_fill_jama(labels = c("Initial Margin", "Variation Margin"))

# save output
ggsave("Plots/Output/EOD_VM_IM_Boxplot.png",
    plot = last_plot(),
    width = 8.33, height = 5.89, units = "cm", dpi = 600
)

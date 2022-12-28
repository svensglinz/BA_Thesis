# load relevant packages
library(tidyverse)
library(scales)
library(ggsci)
library(showtext)
library(ggpattern)

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
out <-
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
        title = "Initial Margin per Asset Class (in Bio EUR)",
        x = NULL,
        y = NULL,
        fill = NULL,
        caption = "Own Depiction | Source: Eurex Clearing AG"
    ) +
    theme(
        text = element_text(family = "lmroman"),
        plot.title = element_text(size = 10, face = "bold"),
        panel.grid = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 8),
        legend.key.size = unit(.4, "cm"),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white", color = "black"),
        axis.ticks = element_line(color = "black"),
        plot.caption = element_text(size = 8),
        plot.margin = margin(0, 0, 0, 0)
    ) +
    scale_fill_grey(
        breaks = c("EQUITY DERIVATIVES", "FIXED INCOME DERIVATIVES", "OTC IRS", "OTHER"),
        labels = c("Equity Derivatives", "FI Derivatives", "OTC IRS", "Other")
    )


# save plot
ggsave("Plots/Output/IM_per_asset.png",
    plot = out,
    device = "png", dpi = 350, height = 6.6,
    width = 10.9, units = "cm"
)

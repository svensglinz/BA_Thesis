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
    bolditalic = "Fonts/lmroman10_bolditalic.ttf",
    symbol = "Fonts/lmroman10_math.otf"
)

showtext_auto(enable = TRUE)
showtext_opts(dpi = 600)

# define parameters
start_date <- as.Date("2020-01-01")
end_date <- as.Date("2020-12-31")
line_date <- as.Date("2020-03-20")

# load data
collateral <-
    read_csv("Data/Eurex_Data/Collateral per Security Type.csv",
        col_types = cols(
            FACT_DATE = col_date(format = "%d/%m/%Y"),
            SECURITY_TYPE = readr::col_factor(),
            COLLATERAL_TYPE = readr::col_factor()
        )
    )

# calculate daily margin deposits across all securities
grouped_daily <-
    collateral |>
    group_by(FACT_DATE) |>
    summarize(total = sum(MARKET_VALUE_EUR))

# bind total margin value to collateral df to calculate % contribution
collateral <- collateral |>
    left_join(grouped_daily, by = c("FACT_DATE")) |>
    mutate(perc = MARKET_VALUE_EUR / total)

# add factor cash
collateral$SECURITY_TYPE <- fct_expand(collateral$SECURITY_TYPE, "CASH")
collateral$SECURITY_TYPE[collateral$COLLATERAL_TYPE == "C"] <- "CASH"

collateral$SECURITY_TYPE <-
    factor(
        collateral$SECURITY_TYPE,
        levels = c(
            "CASH", "SOVEREIGN GOVERNMENT BONDS", "BANK BONDS",
            "STATE/MUNICIPAL BONDS", "STATE AGENCIES", "CORPORATE BONDS", "STOCKS"
        )
    )

# plot graph
collateral |>
    filter(
        SECURITY_TYPE %in% c(
            "BANK BONDS", "CORPORATE BONDS", "SOVEREIGN GOVERNMENT BONDS",
            "STATE AGENCIES", "STOCKS", "STATE/MUNICIPAL BONDS"
        ) | COLLATERAL_TYPE == "C",
        between(FACT_DATE, start_date, end_date)
    ) |>
    ggplot(aes(x = FACT_DATE, y = perc)) +
    geom_line() +
    geom_vline(
        xintercept = line_date,
        color = "#515151", linewidth = 2, alpha = .2
    ) +
    scale_y_continuous(labels = scales::percent_format()) +
    scale_x_date(
        breaks = seq.Date(from = start_date, to = end_date, by = "2 month"),
        labels = scales::label_date(format = "%b")
    ) +
    labs(
        title = "Total Share of Deposited Collateral per Security Type",
        subtitle = "grey line = 20th March 2020",
        y = NULL,
        x = NULL,
        color = NULL
    ) +
    theme(
        text = element_text(family = "lmroman", colour = "#555555"),
        plot.subtitle = element_text(family = "sans", face = "italic", size = 7),
        plot.caption = element_text(size = 8),
        legend.background = element_rect(fill = "transparent", colour = "#cccccc", linewidth = 0),
        legend.justification = .5,
        panel.border = element_rect(colour = "#999999", fill = "transparent"),
        panel.background = element_rect(fill = "#FFFFFF", colour = "#999999", linewidth = 0),
        panel.grid.minor.y = element_blank(), # element_line(colour = "#eeeeee", linewidth = 0.5),
        panel.grid.major = element_line(colour = "#eeeeee", linewidth = 0.5),
        panel.grid.minor.x = element_blank(),
        plot.background = element_rect(fill = "#F9F9F9", colour = "#CCCCCC", linewidth = 0),
        axis.ticks = element_blank(),
        axis.text = element_text(size = 6),
        axis.text.y = element_text(margin = margin(0, 0, 0, 0)),
        axis.text.x = element_text(margin = margin(0, 0, 0, 0)),
        axis.title = element_text(size = 8),
        plot.title = element_text(size = 10, face = "bold"),
        plot.margin = margin(5, 5, 5, 5),
        strip.background = element_rect(fill = "#FFFFFF", color = "#808080", linewidth = 0.5),
        strip.text = element_text(size = 6, margin = margin(t = 2, b = 2, 0, 0))
    ) +
    facet_wrap(~SECURITY_TYPE, scales = "free_y")

# save output
ggsave("Plots/Output/collateral_share.png",
    plot = out,
    device = "png", dpi = 600, height = 8.5,
    width = 15.9, units = "cm"
)

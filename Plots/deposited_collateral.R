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
showtext_opts(dpi = 350)

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

out <-
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
        color = "red", size = 2, alpha = .2
    ) +
    scale_y_continuous(labels = scales::percent_format()) +
    scale_x_date(
        breaks = seq.Date(from = start_date, to = end_date, by = "2 month"),
        labels = scales::label_date(format = "%b")
    ) +
    labs(
        title = "Total Share of Deposited Collateral per Security Type",
        subtitle = "red line = 20th March 2020",
        y = NULL,
        x = NULL,
        color = NULL
    ) +
    theme(
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(size = 10, face = "bold"),
        plot.subtitle = element_text(size = 8, face = "italic"),
        panel.grid = element_line(color = "grey"),
        strip.background = element_rect(color = "grey", fill = "grey"),
        panel.background = element_rect(fill = "white", color = "white"),
        strip.text = element_text(size = 6),
        axis.ticks.x = element_line(color = "black"),
        plot.background = element_rect(fill = "white")
    ) +
    facet_wrap(~SECURITY_TYPE, scales = "free_y")

ggsave("Plots/Output/collateral_share.png",
    plot = out,
    device = "png", dpi = 350, height = 8.5,
    width = 15.9, units = "cm"
)

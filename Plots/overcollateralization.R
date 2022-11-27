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
# define function paremeters
start_date <- as.Date("2020-01-01")
end_date <- as.Date("2022-01-01")
line_date <- as.Date("2020-03-20")

# load required data
tmr <-
    read_csv("Data/Eurex_Data/TMR.csv",
        col_types = cols(FACT_DATE = col_date(format = "%d/%m/%Y"))
    )

defaultfund <-
    read_csv("Data/Eurex_Data/Default Fund Requirement by Date.csv",
        col_types = cols(FACT_DATE = col_date(format = "%d/%m/%Y"))
    )

collateral <-
    read_csv("Data/Eurex_Data/Collateral per Security Type.csv",
        col_types = cols(
            FACT_DATE = col_date(format = "%d/%m/%Y"),
            SECURITY_TYPE = readr::col_factor(),
            COLLATERAL_TYPE = readr::col_factor()
        )
    )

# clean and edit loaded data
tmr <-
    tmr |>
    filter(between(FACT_DATE, start_date, end_date)) |>
    group_by(FACT_DATE) |>
    summarize(MARGIN_REQ = sum(TMR_EUR)) |>
    select(FACT_DATE, MARGIN_REQ)

defaultfund <-
    defaultfund |>
    filter(between(FACT_DATE, start_date, end_date)) |>
    rename(DEFAULT_REQ = REQUIREMENT_EOP) |>
    select(FACT_DATE, DEFAULT_REQ)

collateral <-
    collateral |>
    filter(between(FACT_DATE, start_date, end_date)) |>
    group_by(FACT_DATE) |>
    summarize(COLLATERAL = sum(COLLATERAL_VALUE_EUR)) |>
    select(FACT_DATE, COLLATERAL)

# join data sets for plotting
joined <-
    defaultfund |>
    left_join(tmr, by = c("FACT_DATE")) |>
    left_join(collateral, by = c("FACT_DATE")) |>
    mutate(
        TOTAL_REQ = MARGIN_REQ + DEFAULT_REQ,
        RATIO = COLLATERAL / TOTAL_REQ
    )

# generate plot
out <-
    joined |>
    ggplot(aes(x = FACT_DATE, RATIO - 1)) +
    geom_line() +
    geom_vline(
        xintercept = line_date,
        color = "red", size = 2, alpha = .2
    ) +
    labs(
        title = "Daily Overcollateralization",
        x = NULL,
        y = NULL,
        subtitle = paste("Excess of Total Margin -& Deault Fund Requirement \n",
        "over Deposited Collateral (Post Haircut)", sep = ""),
        caption = "Own depiction | Data Source: Eurex Clearing"
    ) +
    scale_y_continuous(
        labels = scales::label_percent()
    ) +
    scale_x_date(
        breaks = seq.Date(
            from = start_date,
            to = end_date, by = "3 months"
        ),
        labels = scales::label_date(format = "%b-%y"),
        expand = expansion(mult = .02)
    ) +
theme(
    text = element_text(family = "lmroman"),
    plot.title = element_text(size = 10, face = "bold"),
    panel.background = element_rect(color = "black", fill = "white"),
    panel.grid.major.y = element_line(
        color = "darkgrey",
        linetype = "dashed", size = .3
    ),
    plot.subtitle = element_text(size = 8, face = "italic", hjust = 0),
    axis.text = element_text(size = 7),
    plot.caption = element_text(size = 7),
    plot.margin = margin(t = 0, r = .5, b = 0, l = 0, "cm")
)

# save output
ggsave("Plots/Output/overcollateralization.png",
    plot = out,
    device = "png", dpi = 350, height = 6.5,
    width = 8.1, units = "cm"
)

# https://www.blackrock.com/corporate/literature/publication/bcbs-cpmi-iosco-margin-report-blackrock-response.pdf
# opinion by blackrock on overcollaterlaization during covid

# large overcoll could also mean that companies already insure against massive rises by proactively posting  more margin!!!



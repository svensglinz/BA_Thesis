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

showtext_auto(enable = TRUE)
showtext_opts(dpi = 600)
set_plot_theme()

# define parameters
start_date <- as.Date("2020-01-01")
end_date <- as.Date("2020-12-31")
line_date <- as.Date("2020-03-20")

# load data
securities <- read_csv("Data/Eurex_Data/eligible_securities.csv",
    col_types = cols(
        SECURITY_EVALUATION_FACTOR = col_double(),
        FACT_DATE = col_date(format = "%d/%m/%Y"),
        SECURITY_TYPE = readr::col_factor()
    )
)

# clean and summarize data set
securities <-
    securities |>
    group_by(SECURITY_TYPE, FACT_DATE) |>
    filter(
        between(FACT_DATE, start_date, end_date),
        SECURITY_TYPE %in% c(
            "BANK BONDS", "CORPORATE BONDS",
            "SOVEREIGN GOVERNMENT BONDS", "STATE AGENCIES",
            "STATE/MUNICIPAL BONDS", "STOCKS"
        )
    ) |>
    summarize(AVG_HAIRCUT = mean(SECURITY_EVALUATION_FACTOR, na.rm = TRUE))

securities$SECURITY_TYPE <-
    factor(
        securities$SECURITY_TYPE,
        levels = c(
            "SOVEREIGN GOVERNMENT BONDS", "BANK BONDS",
            "STATE/MUNICIPAL BONDS", "STATE AGENCIES", "CORPORATE BONDS", "STOCKS"
        )
    )
# generate plot
securities |>
    ggplot(aes(x = FACT_DATE, y = AVG_HAIRCUT)) +
    geom_vline(
        xintercept = line_date,
        color = "#575656", size = 2, alpha = .2
    ) +
    geom_line() +
    scale_x_date(
        breaks = seq.Date(from = start_date, to = end_date, by = "2 month"),
        labels = scales::label_date(format = "%b")
    ) +
    labs(
        title = "Evolution of Collateralization Rates at Eurex (in % of Market Value, 2020)",
        subtitle = "grey line = 20th March 2020",
        x = NULL,
        y = NULL
    ) +
    facet_wrap(~SECURITY_TYPE, scales = "free_y") +
    theme(strip.text = element_text(size = 7))

# save output
ggsave("Plots/Output/collateral_haircuts.svg",
    plot = last_plot(), device = "svg",
    dpi = 600, height = 8.5, width = 15.9, units = "cm"
)

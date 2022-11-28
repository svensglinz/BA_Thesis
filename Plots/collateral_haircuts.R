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

# generate plot
out <-
    securities |>
    ggplot(aes(x = FACT_DATE, y = AVG_HAIRCUT)) +
    geom_vline(
        xintercept = line_date,
        color = "red", size = 2, alpha = .2
    ) +
    geom_line() +
        scale_x_date(
        breaks = seq.Date(from = start_date, to = end_date, by = "2 month"),
        labels = scales::label_date(format = "%b")) +
    theme(
        text = element_text(family = "lmroman"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(size = 10, face = "bold"),
        plot.subtitle = element_text(size = 8, face = "italic"),
        panel.grid = element_line(color = "grey"),
        strip.background = element_rect(color = "grey", fill = "grey"),
        panel.background = element_rect(fill = "white", color = "white"),
        strip.text = element_text(size = 6),
        plot.caption = element_text(size = 7),
        axis.ticks.x = element_line(color = "black"),
        plot.background = element_rect(fill = "white", color = "white")
    ) +
    labs(
        title = "Evolution of Collateral Haircuts",
        subtitle = "red line = 20th March 2020",
        caption = "Own Depiction | Data Source: Eurex Clearing AG",
        x = NULL,
        y = NULL
    ) +
    facet_wrap(~SECURITY_TYPE, scales = "free_y")

# save output
ggsave("Plots/Output/collateral_haircut.png",
    plot = out, device = "png",
    dpi = 350, height = 8.5, width = 15.9, units = "cm"
)
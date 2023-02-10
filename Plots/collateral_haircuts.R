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
showtext_opts(dpi = 600)

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
out <-
    securities |>
    ggplot(aes(x = FACT_DATE, y = AVG_HAIRCUT)) +
    geom_vline(
        xintercept = line_date,
        color = "#838383", size = 2, alpha = .2
    ) +
    geom_line() +
    scale_x_date(
        breaks = seq.Date(from = start_date, to = end_date, by = "2 month"),
        labels = scales::label_date(format = "%b")
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
    labs(
        title = "Evolution of Collateral Haircuts",
        subtitle = "grey line = 20th March 2020",
        caption = "Own Depiction | Data Source: Eurex Clearing AG",
        x = NULL,
        y = NULL
    ) +
    facet_wrap(~SECURITY_TYPE, scales = "free_y")


# save output
ggsave("Plots/Output/collateral_haircut.png",
    plot = out, device = "png",
    dpi = 600, height = 8.5, width = 15.9, units = "cm"
)

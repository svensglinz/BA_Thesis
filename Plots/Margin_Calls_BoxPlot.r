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

# load data set
imc <-
    read_csv("Data/Eurex_Data/IMC.csv",
        col_types = cols(FACT_DATE = col_date(format = "%d/%m/%Y"))
    )

# modify and clean data
out <- imc |>
    filter(between(FACT_DATE, start_date, end_date)) |>
    mutate(MONTH = format(FACT_DATE, "%b")) |>
    ggplot(
        aes(
            x = reorder(MONTH, FACT_DATE), y = VOLUME / 10^9,
            group = interaction(MONTH, TYPE), fill = TYPE
        )
    ) +
    geom_boxplot(outlier.size = .5, fatten = .8) +
    labs(
        x = NULL,
        y = NULL,
        title = "Volume of Daily IMC (2020)",
        fill = NULL,
        caption = "Own Depiction | Source: Eurex Clearing AG"
    ) +
    scale_y_continuous(breaks = seq(0, 10, 2)) +
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
    scale_fill_jama(
        labels = c("Initial Margin", "Variation Margin")
    )

# save output
ggsave("Plots/Output/IMC_boxplot.png",
    plot = out, width = 8.33, height = 6.2,
    dpi = 350, unit = "cm"
)



# testing table plotting features!
update.packages()

summarize(mtcars, hello = sum(disp), .by = mpg)

a <- mtcars

mtcars |> left_join(a, join_by(mpg))


mtcars |>
    head() |>
    gt() |>
    tab_style(style = cell_text(font = google_font("Fira Mono")))

data.frame(x = c(1.2345, 12.345, 123.45, 1234.5, 12345)) %>%
    gt() %>%
    tab_style(
        # MUST USE A MONO-SPACED FONT
        style = cell_text(font = "lmroman"),
        locations = cells_body(columns = x)
    )

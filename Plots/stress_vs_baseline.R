# load relevant packages
library(tidyverse)
library(scales)
library(ggsci)
library(showtext)

# import written functions and store master sheet in memory
source("functions.R")
master <- read_master("Data/data_input.xlsx")

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

# define function inputs
start_date <- as.Date("2020-01-01")
end_date <- as.Date("2020-12-31")

args_long <-
  list(
    MPOR = 3, factor = 1.37, quantile = 0.974,
    lambda = 0.9593, n_day = 750, floor = FALSE,
    absolute = FALSE, liq_group = "PEQ01", short = FALSE
  )

args_short <-
  list(
    MPOR = 3, factor = 1.37, quantile = 0.974,
    lambda = 0.9593, n_day = 750, floor = FALSE,
    absolute = FALSE, liq_group = "PEQ01", short = TRUE
  )

# calculate margin requirements (FHS & Floored for long and short)
floored_short <- calculate_margin(
  product = "FESX", start = start_date,
  end = end_date, args = args_short,
  steps = FALSE
) |>
  mutate(
    TYPE = "FLOORED",
    DIRECTION = "SHORT"
  )

floored_long <- calculate_margin(
  product = "FESX", start = start_date,
  end = end_date, args = args_long,
  steps = FALSE
) |>
  mutate(
    TYPE = "FLOORED",
    MARGIN = MARGIN * -1,
    DIRECTION = "LONG"
  )

fhs_long <- calculate_fhs_margin(
  product = "FESX", start = start_date,
  end = end_date, args = args_long,
  steps = FALSE
) |>
  mutate(
    TYPE = "FHS",
    DIRECTION = "LONG",
    MARGIN = MARGIN * -1
  )

fhs_short <- calculate_fhs_margin(
  product = "FESX", start = start_date,
  end = end_date, args = args_short,
  steps = FALSE
) |>
  mutate(
    TYPE = "FHS",
    DIRECTION = "SHORT"
  )

# join floored and unfloored margin requirements
joined <- floored_long |>
  bind_rows(floored_short, fhs_long, fhs_short)

# assemble plot
out <-
  joined |>
  ggplot(aes(
    x = DATE, y = MARGIN,
    group = interaction(DIRECTION, TYPE),
    color = TYPE,
  )) +
  geom_line() +
  geom_hline(
    yintercept = 0, color = "darkgrey",
    linetype = "dashed", size = .4
  ) +
  scale_y_continuous(
    breaks = seq(from = -.2, to = .2, by = .05),
    labels = scales::label_percent()
  ) +
  scale_x_date(
    breaks = seq.Date(
      from = start_date,
      to = end_date, by = "1 month"
    ),
    labels = scales::label_date(format = "%b"),
    expand = expansion(mult = .02)
  ) +
  labs(
    title = "Floored Margin FESX",
    color = NULL,
    x = NULL,
    y = NULL,
    subtitle = "Front Month Contract (Expiry 0-90d)",
    caption = "Own Depiction | Data Source: Eurex Clearing AG"
  ) +
  theme(
    text = element_text(family = "lmroman"),
    panel.grid = element_blank(),
    panel.background = element_rect(color = "black", fill = "white"),
    axis.text.x = element_text(angle = 45, vjust = .5),
    legend.position = "bottom",
    plot.subtitle = element_text(size = 8, face = "italic"),
    plot.title = element_text(size = 10, face = "bold"),
    legend.key = element_rect(fill = "white"),
    legend.key.width = unit(.2, "cm"),
    legend.box.spacing = unit(0, "cm"),
    plot.caption = element_text(size = 8),
    axis.text = element_text(size = 7)
  ) +
  scale_color_jama(labels = c("Baseline", "Floored"))

# save output
ggsave("Plots/Output/baseline_vs_stress.png",
    plot = out,
    device = "png", dpi = 350, height = 6.32,
    width = 7.86, units = "cm"
)

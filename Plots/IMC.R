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
start_date <- as.Date("2020-03-01")
end_date <- as.Date("2020-03-31")

# load data set
imc <-
  read_csv("Data/Eurex_Data/IMC.csv",
    col_types = cols(FACT_DATE = col_date(format = "%d/%m/%Y"))
  )

# modify and clean data
grouped <- imc |>
  group_by(FACT_DATE) |>
  summarize(COUNT = sum(N_CALLS))

imc <- imc |>
  left_join(grouped, by = c("FACT_DATE")) |>
  mutate(DAY = as.factor(format(FACT_DATE, "%d")))

# plot graph
out <-
  imc |>
  filter(between(FACT_DATE, start_date, end_date)) |>
  ggplot(aes(x = DAY, y = N_CALLS, fill = TYPE)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_y_continuous(
    breaks = seq(from = 0, to = 90, by = 20),
    expand = expansion(mult = c(.01, .07))
  ) +
  scale_x_discrete(breaks = c("02", "04", "06", "10", "12", "16", "18", "20", "24", "26", "30")) +
  labs(
    title = "Number of IMC (March 2020)",
    x = NULL,
    y = NULL,
    fill = NULL,
    caption = "Own Depiction | Data Source: Eurex Clearing AG"
  ) +
  theme(
    text = element_text(family = "lmroman"),
    plot.title = element_text(size = 10, face = "bold"),
    plot.caption = element_text(size = 8, margin = margin(t = -.1, b = 0, r = 0, l = 0)),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = "black"),
    legend.position = "bottom",
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    legend.key.size = unit(.3, "cm"),
    legend.box.spacing = unit(5, "pt"),
    plot.margin = margin(0, 0, 0, 0),
    legend.margin = margin(t = 0, b = .2, l = 0, r = 0, unit = "cm")
  ) +
  scale_fill_grey(
    start = .7, end = .3,
    labels = c("Initial Margin", "Variation Margin")
  )

# save output
ggsave("Plots/Output/IMC_March.png",
  plot = out,
  dpi = 350, width = 7.6, height = 6.2, units = "cm"
)

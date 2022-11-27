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
  left_join(grouped, by = c("FACT_DATE"))

# plot graph
out <-
  imc |>
  filter(between(FACT_DATE, start_date, end_date)) |>
  mutate(FACT_DATE = as.factor(format(FACT_DATE, "%d"))) |>
  ggplot(aes(x = FACT_DATE, y = N_CALLS, fill = TYPE)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_y_continuous(
    breaks = seq(from = 0, to = 90, by = 10),
    expand = expansion(mult = c(.01, .07))
  ) +
  labs(
    title = "Margin Calls per Trigger Type (March 2020)",
    x = NULL,
    y = "Number of Calls",
    fill = NULL,
    caption = "Own Depiction | Data Source: Eurex Clearing AG"
  ) +
  theme(
    text = element_text(family = "lmroman"),
    plot.title = element_text(size = 10, face = "bold"),
    plot.caption = element_text(size = 8),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = "black"),
    legend.position = "bottom",
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 8),
    legend.key.size = unit(.3, "cm"),
    plot.subtitle = element_text(size = 8, face = "italic"),
    legend.box.spacing = unit(0, "pt"),
  ) +
  scale_fill_jama(labels = c("Initial Margin", "Variation Margin"))

# save output
ggsave("Plots/Output/IMC_March.png",
  plot = out,
  dpi = 350, width = 9.9, height = 6.6, units = "cm"
)

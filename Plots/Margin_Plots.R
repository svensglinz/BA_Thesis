#load relevant packages
library(tidyverse)
library(scales)
library(ggsci)
library(patchwork)
library(showtext)
# import written functions and store master sheet in memory
source("functions.R")
master <- read_master("Data/data_input.xlsx")

#define function parameters
start_date <- as.Date("2020-01-01")
end_date <- as.Date("2021-01-01")

args_long_FESX <-
  list(
    MPOR = 3, factor = 1.37, quantile = 0.974,
    lambda = 0.9593, n_day = 750, floor = FALSE,
    absolute = FALSE, liq_group = "PEQ01",
    short = FALSE
  )

args_short_FESX <-
  list(
    MPOR = 3, factor = 1.37, quantile = 0.974,
    lambda = 0.9593, n_day = 750, floor = FALSE,
    absolute = FALSE, liq_group = "PEQ01",
    short = TRUE
  )

args_long_FGBX <-
  list(
    MPOR = 3, factor = 1.37, quantile = 0.974,
    lambda = 0.95, n_day = 750, floor = FALSE,
    absolute = FALSE, liq_group = "PFI01",
    short = FALSE
  )

args_short_FGBX <-
  list(
    MPOR = 3, factor = 1.37, quantile = 0.974,
    lambda = 0.95, n_day = 750, floor = FALSE,
    absolute = FALSE, liq_group = "PFI01",
    short = TRUE
  )


#Visualization of Graph XX XX in final Thesis (label with graph number here to find easier!)
FESX_Margin_long <-
  calculate_margin(
    product = "FESX", start = start_date, end = end_date,
    args = args_long_FESX, steps = TRUE
  ) |>
  select(DATE, RET_MPOR, MARGIN) |>
  mutate(
    MARGIN = MARGIN * -1,
    RET_MPOR = lag(RET_MPOR, 3),
    breach = ifelse(RET_MPOR < MARGIN, TRUE, FALSE)
  )

FESX_Margin_short <-
  margin_calculator(
    product = "FESX", start = start_date, end = end_date,
    args = args_short, steps = TRUE
  ) |>
  select(DATE, RET_MPOR, MARGIN) |>
  mutate(
    MPOR_returns = lag(RET_MPOR * -1, 3),
    breach = ifelse(RET_MPOR > MARGIN, TRUE, FALSE)
  )

FESX_Margin <-
  FESX_Margin_long |>
  full_join(FESX_Margin_short, by = c("DATE")) |>
  rename(
    LONG = Margin.x,
    SHORT = Margin.y,
    MPOR_returns_LONG = MPOR_returns.x,
    MPOR_returns_SHORT = MPOR_returns.y,
    breach_long = breach.x,
    breach_short = breach.y) |>
  mutate(breach = ifelse((breach_long | breach_short), TRUE, FALSE),
         color = ifelse(breach, "red", "black"),
         size = ifelse(breach, 2, 1))|>
  pivot_longer(c("SHORT", "LONG"), names_to = "margin") |>
  select(-c(MPOR_returns_SHORT, breach_long, breach_short))

FGBX_Margin <-
  FGBX_Margin_long |>
  full_join(FGBX_Margin_short, by = c("DATE")) |>
  rename(
    LONG = Margin.x,
    SHORT = Margin.y,
    MPOR_returns_LONG = MPOR_returns.x,
    MPOR_returns_SHORT = MPOR_returns.y,
    breach_long = breach.x,
    breach_short = breach.y) |>
  mutate(breach = ifelse((breach_long | breach_short), TRUE, FALSE),
         color = ifelse(breach, "red", "black"),
         size = ifelse(breach, 2, 1))|>
  pivot_longer(c("SHORT", "LONG"), names_to = "margin") |>
  select(-c(MPOR_returns_SHORT, breach_long, breach_short))

FESX_plot <-
  FESX_Margin |>
  ggplot(aes(x = DATE, y = value, groups = margin)) +
  geom_line(aes(color = margin)) +
  scale_y_continuous(
    breaks = seq(from = -0.25, to = 0.25, by = 0.05),
    labels = scales::label_percent()) +
  scale_x_date(breaks = seq.Date(from = start_date,
                                 to = start_date,
                                 by = "2 month"),
               labels = scales::date_format(format = "%b-%Y")) +
  geom_point(aes(y = MPOR_returns_LONG, fill = "lagged n-day Return"),
             color = FESX_Margin$color,
             size = FESX_Margin$size) +
  theme_minimal() +
  labs(title = "FESX", subtitle = "n = 3",
       x = NULL, y = NULL, color = NULL, fill = NULL) +
  theme(
    panel.grid = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.background = element_rect(color = "black"),
    axis.text.x = element_text(angle = 45, vjust = .5),
    legend.position = "bottom",
    plot.subtitle = element_text(size = 8, face = "italic"),
    plot.title = element_text(size = 10)) +
  scale_color_jama()

FGBX_plot <-
  FGBX_Margin |>
  ggplot(aes(x = DATE, y = value, groups = margin, color = margin)) +
  geom_line() +
  scale_y_continuous(
    breaks = seq(from = -0.08, to = 0.08, by = 0.02),
    labels = scales::label_percent()) +
  scale_x_date(breaks = seq.Date(from = start_date,
                                 to = end_date,
                                 by = "2 month"),
               labels = scales::date_format(format = "%b-%Y")) +
  geom_point(aes(y = MPOR_returns_LONG, fill = "lagged n-day Return"),
             color = FGBX_Margin$color,
             size = FGBX_Margin$size) +
  theme_minimal() +
  scale_alpha_manual(values = 1) +
  labs(
    title = "FGBX",
    subtitle = "n = 2",
    x = NULL, y = NULL, color = NULL,
    fill = NULL) +
  theme(
    panel.grid = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.background = element_rect(color = "black"),
    axis.text.x = element_text(angle = 45, vjust = .5),
    legend.position = "bottom",
    plot.title = element_text(size = 10),
    plot.subtitle = element_text(size = 8, face = "italic")) +
  scale_color_jama()

#path two graphs together
out <-
  FGBX_plot + FESX_plot +
  plot_annotation(
    title = "Margin Requirement MRIM",
    theme = theme(
      plot.title = element_text(hjust = .5, size = 14, face = "bold"),
      legend.position = "bottom"
    )
  ) +
  plot_layout(guides = "collect")

ggsave("margins.png",
  plot = out, device = png,
  width = 15.9, height = 8.5, unit = "cm"
)



args <-
  list(
    MPOR = 3, factor = 1.37, quantile = 0.974,
    lambda = 0.9593, n_day = 750, floor = FALSE,
    absolute = FALSE, liq_group = "PEQ01",
    short = FALSE
  )

margin <- calculate_margin(product = "FESX", start = start_date,
end = end_date, args = args, steps = FALSE)

margin |> 
ggplot(aes(x = DATE, y = MARGIN))+
geom_line()

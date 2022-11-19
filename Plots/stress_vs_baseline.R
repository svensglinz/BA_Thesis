#load relevant packages
library(readxl)
library(lubridate)
library(zoo)
library(tidyverse)
library(scales)
library(ggthemes)
library(ggsci)
library(ggExtra)
library(ggforce)
library(patchwork)

#define initial variables
source("functions_redone.R")
master <- read_master("Data/data_input.xlsx")
start_date <- as.Date("01.01.2005", format = "%d.%m.%Y")
end_date <- as.Date("31.12.2021", format = "%d.%m.%Y")

#define model parameters
args_long_FESX <-
  list(MPOR = 3, factor = 1.37, quantile = 0.974,  quantile = 0.974,
       lambda = 0.9593, n_day = 750, floor = FALSE,
       absolute = FALSE, liq_group  = "PEQ01",
       short = FALSE)

stress_long <- margin_calculator(product = "FESX", start = start_date, 
                            end = end_date, args = args_long_FESX,
                            steps = F) |> 
  mutate(type = "STRESS")

baseline_long <- calculate_FHS_margin(product = "FESX", start = start_date, 
                                 end = end_date, args = args_long_FESX,
                                 steps = F) |> 
  mutate(type = "BASELINE") |> 
  rename(Margin = FHS_Margin)

joined <- stress_long |>
  bind_rows(baseline_long)

joined |> 
  ggplot(aes(x = DATE, y = Margin, color = type))+
  geom_line()+
  labs(title = "",
       color = NULL,
       x = NULL,
       y = NULL)+
  theme(
    panel.grid = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.background = element_rect(color = "black", fill = "white"),
    axis.text.x = element_text(angle = 45, vjust = .5),
    legend.position = "bottom",
    plot.subtitle = element_text(size = 8, face = "italic"),
    plot.title = element_text(size = 10),
    legend.key = element_rect(fill = "white")) +
  scale_color_jama()



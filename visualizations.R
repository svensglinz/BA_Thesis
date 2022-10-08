#load libraries
library(readxl)
library(lubridate)
library(zoo)
library(tidyverse)
library(scales)
library(ggthemes)
library(ggsci)


####################
#EWMA VOLATILITY CALCULATION
######################

vol_FESX <- calculate_vola(product = "FESX", start = start, end = end, lambda = lambda, n_day = n_day) |> 
  rename(date = returns, FESX = vola)
vol_FSMI <- calculate_vola(product = "FSMI", start = start, end = end, lambda = lambda, n_day = n_day)|> 
  rename(date = returns, FSMI = vola)
vol_FGBX <- calculate_vola(product = "FGBX", start = start, end = end, lambda = lambda, n_day = n_day)|> 
  rename(date = returns, FGBX = vola)
vol_FGBL <- calculate_vola(product = "FGBL", start = start, end = end, lambda = lambda, n_day = n_day)|> 
  rename(date = returns, FGBL = vola)

combined <- vol_FESX |> 
  full_join(vol_FSMI) |>
  full_join(vol_FGBX) |>
  full_join(vol_FGBL) |> 
  na.omit() |> 
  pivot_longer(cols = 2:5, names_to = "product", values_to = "vola")

#PLOT the volatility graph
#this plot should show that if we put it in larger context, this was not an extreme outlier 
#especially if we compare it to the financial crisis 
# --> What has changed then, was it clearing volumes or what is it ???!!!

#could also calculate margin backwards to these days and show our effects --> Large backesting!

vol_plot <- 
  combined |> 
  ggplot(aes(x = date, y = vola, group= product, color = product))+
  geom_line()+
  theme_minimal()+
  labs(title = "Daily Exponentially weighted Volatility of Returns",
       x = NULL,
       y = NULL, 
       color = NULL)+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid = element_line(color = "grey", linetype = 2, size = 0.5),
        panel.background = element_rect(color = "black"),
        legend.position = "right",
        plot.title = element_text(size = 12, face = "bold"))+
  scale_y_continuous(breaks = seq(from = 0.01, to = 0.06, by = 0.01), labels = scales::label_percent())+
  expand_limits(y = 0.06)+
  scale_color_jama()

ggsave("graphs/volplot.png", plot = vol_plot, device = "png", dpi = 300, height = 6.03, width = 15.9, units = "cm")


##############################################
#MARGIN CALCULATIONS
##############################################
args <- list(MPOR = 3, factor = 1.37, quantile = 0.974, lambda = 0.9593, 
             n_day = 750, floor = FALSE, absolute = FALSE, liq_group  = "PEQ01",
             short = FALSE)

args_short <- list(MPOR = 3, factor = 1.37, quantile = 0.013, lambda = 0.9593, 
                  n_day = 750, floor = FALSE, absolute = FALSE, liq_group  = "PEQ01",
                  short = TRUE)

FESX_Margin_long <- margin_calculator(product = "FESX", start = start, end = end, args = args_long)
FESX_Margin_short <- margin_calculator(product = "FESX", start = start, end = end, args = args_short)

FESX_Margin <- FESX_Margin_long |> full_join(FESX_Margin_short, by = c("dates")) |> 
  rename(MARGIN_LONG = MARGIN.x, MARGIN_SHORT = MARGIN.y) |> 
  mutate(MARGIN_SHORT = -MARGIN_SHORT) |> 
  pivot_longer(2:3, names_to = "margin")

FESX_Margin |>
  ggplot(aes(x = dates, y = value, groups = margin, color = margin))+
  geom_line()+
  scale_y_continuous(breaks = seq(from = -0.25, to = 0.25, by = 0.05),
                     labels = scales::label_percent())+
theme_minimal()+
  labs(title = "Daily Exponentially weighted Volatility of Returns",
       x = NULL,
       y = NULL, 
       color = NULL)+
  theme(panel.grid = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(color = "black"),
        legend.position = "right",
        plot.title = element_text(size = 12, face = "bold"))+
  scale_color_jama()

#####################################################################
#check out how this was done in previous data file!
IMC <-  read_excel("Data/Eurex_Data/IMC_Redone.xlsx")




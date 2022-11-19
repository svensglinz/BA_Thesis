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
start_date <- as.Date("01.01.2020", format = "%d.%m.%Y")
end_date <- as.Date("31.12.2021", format = "%d.%m.%Y")

#define model parameters


args_long_FESX <-
  list(MPOR = 3, factor = 1.37, quantile = 0.974,
       lambda = 0.9593, n_day = 750, floor = FALSE,
       absolute = FALSE, liq_group  = "PEQ01",
       short = FALSE)

factor <- vector()
procyclicality <- vector()
costs <- vector()
max_shortfall <- vector()

count <- 1

for (i in seq(0.8,0.99, by = 0.01)){
  
  args_long_FESX$lambda <- i
  
  FESX_Margin <- 
    margin_calculator(product = "FESX", start = start_date, end = end_date,
                      args = args_long_FESX, steps = T)
  
  measures <- 
    summary_stats(FESX_Margin, start = start_date, end = end_date)
  
  factor[count] <- i
  costs[count] <- measures |>
    filter(type == "costs") |>
    pull(value)
  procyclicality[count] <- measures |> 
    filter(type == "max_30d") |> 
    pull(value)
  max_shortfall[count] <- measures |> 
    filter(type == "max_shortfall") |> 
    pull(value)
  
  count <- count + 1
}

results <- tibble(factor = factor, 
                  procyclicality = procyclicality, 
                  costs = costs,
                  max_shortfall = max_shortfall)


#also somewhere put average shortfall into the graph to show the tradeoff!!!!!!!
results |> 
  ggplot(aes(x = costs, y = procyclicality))+
  geom_point(aes(color = factor))+
  scale_color_gradient(high = "red", low = "blue")+
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid = element_line(
      color = "grey",
      linetype = 2,
      size = 0.5
    ),
    panel.background = element_rect(color = "black", fill = "white"),
    legend.position = "right",
    plot.title = element_text(size = 12, face = "bold"),
    legend.key = element_rect(fill = "white")
  )+
  scale_y_continuous(
    sec.axis = sec_axis(trans = ~.*10, name = "second axis")
  )+
  labs(title = "title",
       x = "x",
       y = "y",
       color = "lambda")

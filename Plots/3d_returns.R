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


#define basic parameters 
source("functions_redone.R")
master <- read_master("Data/data_input.xlsx")

d_returns_FESX <- 
  master$returns |> filter(INST == "FESX") |> 
  filter(between(DATE, as.Date("2007-01-01"), as.Date("2021-01-01"))) |> 
  arrange(desc(DATE)) |> 
  ggplot(aes(x = DATE, y = exp(LOG_RET)-1))+
  geom_line()+
  scale_y_continuous(breaks = seq(from = -0.2, to = 0.2, by = 0.05), labels = scales::label_percent())+
  scale_x_continuous(breaks = seq.Date(from = as.Date("2007-01-01"), to = as.Date("2021-01-01"), by = "year"),
                     labels = scales::label_date(format = "%Y"))+
  theme_bw()+
  labs(title = "Daily Returns FESX",
       y = NULL, 
       x = NULL)+
  theme(plot.title = element_text(size = 12, face = "bold"))

ggsave("graphs/d_returns_FESX.png", plot = d_returns_FESX, device = "png", dpi = 300, height = 6.03, width = 15.9, units = "cm")

